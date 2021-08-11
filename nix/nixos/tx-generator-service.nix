pkgs:
let
  ## Standard, simplest possible value transaction workload.
  ##
  ## For definitions of the cfg attributes referred here,
  ## please see the 'defServiceModule.extraOptionDecls' attset below.
  basicValueTxWorkload =
    cfg: with cfg; with pkgs.lib;
    [
      { setNumberOfInputsPerTx   = inputs_per_tx; }
      { setNumberOfOutputsPerTx  = outputs_per_tx; }
      { setNumberOfTxs           = tx_count; }
      { setTxAdditionalSize      = add_tx_size; }
      { setMinValuePerUTxO       = min_utxo_value; }
      { setFee                   = tx_fee; }
      { setTTL                   = 1000000; }
      { startProtocol            = nodeConfigFile; }
      { setEra                   = capitalise era; }
      { setTargets =
           __attrValues
             (__mapAttrs (name: { ip, port }:
                            { addr = ip; port = port; })
                         targetNodes);
      }
      { setLocalSocket    = localNodeSocketPath; }
      { readSigningKey    = "pass-partout"; filePath = sigKey; }
      { importGenesisFund = "pass-partout"; fundKey  = "pass-partout"; }
      { delay             = init_cooldown; }
    ]
    ++
    (if continuousMode
             ## WARNING: this could go over the genesis UTxO funds!
     then createChangeScript cfg
            (tx_count * tx_fee + min_utxo_value)
            (length (__attrNames targetNodes) * 2 * inputs_per_tx)
     else createChangeRecursive cfg
            (min_utxo_value + tx_fee)
            (tx_count * inputs_per_tx)
    )
    ++
    [
      { runBenchmark      = "walletBasedBenchmark";
                  txCount = tx_count; tps = tps; }
      { waitBenchmark     = "walletBasedBenchmark"; }
    ];

  defaultGeneratorScriptFn = basicValueTxWorkload;

  ## The standard decision procedure for the run script:
  ##
  ##  - if the config explicitly specifies a script, take that,
  ##  - otherwise compute it from the configuration.
  defaultDecideRunScript =
    cfg: with cfg;
      __toJSON
        (if runScript != null
         then runScript
         else runScriptFn cfg);

  capitalise = x: (pkgs.lib.toUpper (__substring 0 1 x)) + __substring 1 99999 x;

  createChangeScript = cfg: value: count:
    [ { createChange = value; count=count; }
      { delay = cfg.init_cooldown; }
    ];

  createChangeRecursive = cfg: value: count: if count <= 30
    then createChangeScript cfg value count
    else createChangeRecursive cfg (value * 30 + cfg.tx_fee) (count / 30 + 1) ++ createChangeScript cfg value count;

in pkgs.commonLib.defServiceModule
  (lib: with lib;
    { svcName = "tx-generator";
      svcDesc = "configurable transaction generator";

      svcPackageSelector =
        pkgs: ## Local:
              pkgs.cardanoNodeHaskellPackages.tx-generator
              ## Imported by another repo, that adds an overlay:
                or pkgs.tx-generator;
              ## TODO:  that's actually a bit ugly and could be improved.
      ## This exe has to be available in the selected package.
      exeName = "tx-generator";

      extraOptionDecls = {
        scriptMode      = opt bool true      "Whether to use the modern script parametrisation mode of the generator.";
        continuousMode  = opt bool false     "Whether to use continuous generation, without the full UTxO pre-splitting phase.";

        ## TODO: the defaults should be externalised to a file.
        ##
        tx_count        = opt int 1000       "How many Txs to send, total.";
        add_tx_size     = opt int 100        "Extra Tx payload, in bytes.";
        inputs_per_tx   = opt int 4          "Inputs per Tx.";
        outputs_per_tx  = opt int 4          "Outputs per Tx.";
        tx_fee          = opt int 10000000   "Tx fee, in Lovelace.";
        tps             = opt int 100        "Strength of generated load, in TPS.";
        init_cooldown   = opt int 50         "Delay between init and main submissions.";
        min_utxo_value  = opt int 10000000   "Minimum value allowed per UTxO entry";

        runScriptFn     = opt (functionTo (listOf attrs)) defaultGeneratorScriptFn
          "Function accepting this service config and producing the generator run script (a list of command attrsets).  Takes effect unless runScript or runScriptFile are specified.";
        runScript       = mayOpt (listOf attrs)
          "Generator run script (a list of command attrsets).  Takes effect unless runScriptFile is specified.";
        runScriptFile   = mayOpt str         "Generator config script file.";

        nodeConfigFile  = mayOpt str         "Node-style config file path.";
        nodeConfig      = mayOpt attrs       "Node-style config, overrides the default.";

        sigKey          = mayOpt str         "Key with funds";

        localNodeSocketPath =
                           mayOpt str        "Local node socket path";
        localNodeConf   = mayOpt attrs       "Config of the local node";

        targetNodes     = mayOpt attrs       "Targets: { name = { ip, port } }";

        era             = opt (enum [ "shelley"
                                      "allegra"
                                      "mary"
                                      "alonzo"
                                    ])
                              "mary"
                              "Cardano era to generate transactions for.";

        ## Internals: not user-serviceable.
        decideRunScript = opt (functionTo str) defaultDecideRunScript
          "Decision procedure for the run script content.";
      };

      configExeArgsFn =
        cfg: with cfg;
          if scriptMode
          then
            let jsonFile =
                  if runScriptFile != null then runScriptFile
                  else "${pkgs.writeText "generator-config-run-script.json"
                                         (decideRunScript cfg)}";
            in ["json" jsonFile]
          else
          (["cliArguments"

            "--config"                 nodeConfigFile

            "--socket-path"            localNodeSocketPath

            ## XXX
            "--${if era == "alonzo" then "mary" else era}"

            "--num-of-txs"             tx_count
            "--add-tx-size"            add_tx_size
            "--inputs-per-tx"          inputs_per_tx
            "--outputs-per-tx"         outputs_per_tx
            "--tx-fee"                 tx_fee
            "--tps"                    tps
            "--init-cooldown"          init_cooldown

            "--genesis-funds-key"      sigKey
          ] ++
          __attrValues
            (__mapAttrs (name: { ip, port }: "--target-node '(\"${ip}\",${toString port})'")
              targetNodes));

      configSystemdExtraConfig = _: {};

      configSystemdExtraServiceConfig =
        cfg: with cfg; {
          Type = "exec";
          User = "cardano-node";
          Group = "cardano-node";
          Restart = "no";
          RuntimeDirectory = localNodeConf.runtimeDir;
          WorkingDirectory = localNodeConf.stateDir;
        };
    })
