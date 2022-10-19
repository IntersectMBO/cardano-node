pkgs:
let
  cleanNixServiceOptions = cfg: with cfg;
    {
                  plutusScript = cfg.plutusScript;
                  targetNodes = targetNodesList cfg.targetNodes;
                  era = capitalise cfg.era;
                  plutusLoopScript = plutusScriptFile cfg "loop.plutus";
                  inherit
                  plutusMode
                  plutusData
                  plutusRedeemer
                  plutusAutoMode
                  executionMemory
                  executionSteps
                  debugMode
                  tx_count
                  add_tx_size
                  inputs_per_tx
                  outputs_per_tx
                  tx_fee
                  tps
                  init_cooldown
                  sigKey
                  min_utxo_value
                  nodeConfigFile
                  localNodeSocketPath;
     };
  defaultGeneratorScriptFn = cleanNixServiceOptions;
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

  plutusScript = cfg: plutusScriptFile cfg cfg.plutusScript;
  plutusScriptFile = cfg: filename: "${pkgs.plutus-scripts}/generated-plutus-scripts/${filename}";
  targetNodesList = targets: __attrValues (__mapAttrs
                                       (name: { ip, port }: { addr = ip; port = port; })
                                       targets);
in pkgs.commonLib.defServiceModule
  (lib: with lib;
    { svcName = "tx-generator";
      svcDesc = "configurable transaction generator";

      svcPackageSelector =
        pkgs: ## Local:
              pkgs.cardanoNodePackages.tx-generator
              ## Imported by another repo, that adds an overlay:
                or pkgs.tx-generator;
              ## TODO:  that's actually a bit ugly and could be improved.
      ## This exe has to be available in the selected package.
      exeName = "tx-generator";

      extraOptionDecls = {
        highLevelConfig = opt bool false     "Pass high-level config to the tx-generator";
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

        debugMode       = opt bool false     "Set debug mode: Redirect benchmarking txs to localhost.";

        tx_count        = opt int 1000       "How many Txs to send, total.";
        add_tx_size     = opt int 100        "Extra Tx payload, in bytes.";
        inputs_per_tx   = opt int 4          "Inputs per Tx.";
        outputs_per_tx  = opt int 4          "Outputs per Tx.";
        tx_fee          = opt int 10000000   "Tx fee, in Lovelace.";
        tps             = opt (either float int) 100
                                             "Strength of generated load, in TPS.";
        init_cooldown   = opt int 50         "Delay between init and main submissions.";
        min_utxo_value  = opt int 10000000   "Minimum value allowed per UTxO entry";
        runScriptFn     = opt (functionTo attrs) defaultGeneratorScriptFn
          "Function accepting this service config and producing the generator run script (a list of command attrsets).  Takes effect unless runScript or runScriptFile are specified.";
        runScript       = mayOpt (listOf attrs)
          "Generator run script (a list of command attrsets).  Takes effect unless runScriptFile is specified.";
        # runScriptFile is used in the workbench, it is broken
        runScriptFile   = mayOpt str         "Generator config script file.";

        nodeConfigFile  = mayOpt str         "Node-style config file path.";
        nodeConfig      = mayOpt attrs       "Node-style config, overrides the default.";

        sigKey          = mayOpt str         "Key with funds";

        tracerSocketPath =
                          mayOpt str         "Socket path of cardano-tracer";
        localNodeSocketPath =
                           mayOpt str        "Local node socket path";
        localNodeConf   = mayOpt attrs       "Config of the local node";

        targetNodes     = mayOpt attrs       "Targets: { name = { ip, port } }";

        era             = opt (enum [ "shelley"
                                      "allegra"
                                      "mary"
                                      "alonzo"
                                      "babbage"
                                    ])
                              "mary"
                              "Cardano era to generate transactions for.";
        ## Internals: not user-serviceable.
        ## broken/ignored options !!
        decideRunScript = opt (functionTo str) defaultDecideRunScript
          "Decision procedure for the run script content.";
      };

      configExeArgsFn = cfg: [
          "json_highlevel"
          "${pkgs.writeText "tx-gen-config.json" (cfg.decideRunScript cfg)}"
      ] ++ optionals (cfg.tracerSocketPath != null) [
          "--cardano-tracer" cfg.tracerSocketPath
      ];

      configSystemdExtraConfig = _: {};

      configSystemdExtraServiceConfig =
        cfg: with cfg; {
          Type = "exec";
          User = "cardano-node";
          Group = "cardano-node";
          Restart = "no";
          RuntimeDirectory = localNodeConf.runtimeDir 0;
          WorkingDirectory = localNodeConf.stateDir 0;
        };
    })
