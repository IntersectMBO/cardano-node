pkgs:
let
  ## Standard, simplest possible value transaction workload.
  ##
  ## For definitions of the cfg attributes referred here,
  ## please see the 'defServiceModule.extraOptionDecls' attset below.
  basicValueTxWorkload =
    cfg: with cfg; with pkgs.lib;
    [
      { Set.SNumberOfInputsPerTx   = inputs_per_tx; }
      { Set.SNumberOfOutputsPerTx  = outputs_per_tx; }
      { Set.SNumberOfTxs           = tx_count; }
      { Set.STxAdditionalSize      = add_tx_size; }
      { Set.SMinValuePerUTxO       = min_utxo_value; }
      { Set.SFee                   = tx_fee; }
      { Set.STTL                   = 1000000; }
      { StartProtocol              = nodeConfigFile; }
      { Set.SEra                   = capitalise era; }
      { Set.STargets =
           __attrValues
             (__mapAttrs (name: { ip, port }:
                            { addr = ip; port = port; })
                         targetNodes);
      }
      { Set.SLocalSocket  = localNodeSocketPath; }
      { ReadSigningKey    = [ "pass-partout" sigKey]; }
      { ImportGenesisFund = [ { LocalSocket = []; } "pass-partout"  "pass-partout" ]; }
      { Delay             = init_cooldown; }
    ]
    ++
    ( let
        ## hard-code mainnet cost model
        ## scriptFees = executionMemory * 577 / 10000 + executionSteps  * 721 / 10000000;
        scriptFees = 5000000;
        collateralPercentage = 200;

        totalFee = if plutusMode
                   then tx_fee + scriptFees * inputs_per_tx
                   else tx_fee;
        safeCollateral = max ((scriptFees + tx_fee) * collateralPercentage / 100) min_utxo_value;
        minTotalValue = min_utxo_value * outputs_per_tx + totalFee;
        minValuePerInput = minTotalValue / inputs_per_tx + 1;
      in
        if !plutusMode
          then createChangeRecursive cfg minValuePerInput (tx_count * inputs_per_tx)
        else
          [
          # this is a hack !
          # PayToCollateral will create outputs which are interally tagged as collateral and not available for splitting etc.
          # If PayToCollateral returns a change value that value will also be tagged as collateral and lost.
          # Therefor this first creates a matching regular output
          # and turns that into a collateral right in the next step.
            { CreateChange = [
                { LocalSocket = []; }
                { PayToAddr = "pass-partout"; }
                (safeCollateral + tx_fee)
                1
              ];
            }
            { CreateChange = [
                { LocalSocket = []; }
                { PayToCollateral = "pass-partout"; }
                safeCollateral
                count
              ];
            }
          ]
          ++ createChangePlutus cfg minValuePerInput (tx_count * inputs_per_tx)
    )
    ++
    [
      { RunBenchmark = [
          ( if !debugMode
            then { NodeToNode = []; }
            else { LocalSocket = []; }
          )
          ( if plutusAutoMode
            then { SpendAutoScript = plutusScriptFile cfg "loop.plutus"; }
            else if plutusMode
            then { SpendScript = [
                     (plutusScript cfg)
                     ( if debugMode
                       then { CheckScriptBudget = { memory = executionMemory; steps = executionSteps; }; }
                       else { StaticScriptBudget = { memory = executionMemory; steps = executionSteps; }; }
                     )
                     plutusData
                     plutusRedeemer
                   ]; }
            else { SpendOutput = []; }
          )
          "tx-submit-benchmark"
          tx_count
          tps
        ];
      }
    ]
    ++
    (
      if !debugMode
      then [ { WaitBenchmark = "tx-submit-benchmark"; } ]
      else [ ]
    )
    ;

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
    [ { CreateChange = [
          { LocalSocket = []; }
          { PayToAddr = "pass-partout"; }
          value
          count
        ];
      }
      { Delay = cfg.init_cooldown; }
    ];

  createChangeScriptPlutus = cfg: value: count:
    [ { CreateChange = [
          { LocalSocket = []; }
          { PayToScript = if cfg.plutusAutoMode
                          then [ (plutusScriptFile cfg "loop.plutus") 0 ]
                          else [ (plutusScript cfg) cfg.plutusData ];
          }
          value
          count
        ];
      }
      { Delay = cfg.init_cooldown; }
    ];

  createChangeRecursive = cfg: value: count: if count <= 30
    then createChangeScript cfg value count
    else createChangeRecursive cfg (value * 30 + cfg.tx_fee) (count / 30 + 1) ++ createChangeScript cfg value count;

  createChangePlutus = cfg: value: count: if count <= 30
    then createChangeScriptPlutus cfg value count
    else createChangeRecursive cfg (value * 30 + cfg.tx_fee) (count / 30 + 1) ++ createChangeScriptPlutus cfg value count;

  plutusScript = cfg: plutusScriptFile cfg cfg.plutusScript;
  plutusScriptFile = cfg: filename: "${pkgs.plutus-scripts}/generated-plutus-scripts/${filename}";
  
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
        continuousMode  = opt bool false     "Whether to use continuous generation, without the full UTxO pre-splitting phase.";

        ## TODO: the defaults should be externalised to a file.
        ##
        plutusMode      = opt bool false     "Whether to benchmark Plutus scripts.";
        plutusScript    = opt str  "sum.plutus" "Path to the Plutus script.";
        plutusData      = opt int          3 "Data passed to the Plutus script (for now only an int).";
        plutusRedeemer  = opt int          6 "Redeemer data passed to the Plutus script (for now only an int).";
        plutusAutoMode  = opt bool false     "Choose all Plutus settings to max out per Tx script budget.";
        executionMemory = opt int    1000000 "Max memory available for the Plutus script.";
        executionSteps  = opt int  700000000 "Max execution steps available for the Plutus script.";

        debugMode       = opt bool false     "Set debug mode: Redirect benchmarkting txs to localhost.";

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
            let jsonFile =
                  if runScriptFile != null then runScriptFile
                  else "${pkgs.writeText "generator-config-run-script.json"
                                         (decideRunScript cfg)}";
            in ["json" jsonFile];

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
