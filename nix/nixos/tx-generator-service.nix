pkgs:
let
  cleanNixServiceOptions = cfg: with cfg;
    {
      govAct = if (!cfg.drep_voting or null) == null then null else
        {
          # Default values until they land in the workbench profile.
          gapBatchSize = 5000;
          gapProposalBatches = 1;
          gapQuorum = 51;
          # These need to be fixed.
          gapDRepKeys = 100;
          gapStakeKeys = 100;
        };
      plutus = if (cfg.plutus.type or null) == null then null else
        {
          inherit (cfg.plutus) type;
          ## Basically do something like:
          ## script = "${pkgs.plutus-scripts}/generated-plutus-scripts/${cfg.plutus.script}";
          ## except for having to weave the Either through things
          ## To refer to a plutus script file, do something like:
          ## { Right = pkgs.plutus-scripts + "/generated-plutus-scripts/" + cfg.plutus.script; }
          script   = { Left = cfg.plutus.script; };
          ## For cardano-ops backwards compatibility the redeemer and datum
          ## files are file paths to the Nix Store UNLESS cfg.plutusRedeemerFile
          ## and cfg.plutusDatumFile are present (these should be file paths to
          ## where they are going to be deployed).
          redeemer = if cfg.plutus.redeemer == null
                     then null
                     else if cfg.plutusRedeemerFile == null
                       # File path to the Nix Store
                       then pkgs.writeText
                              "plutus-redeemer.json"
                              (__toJSON cfg.plutus.redeemer)
                       # Config supplied file path.
                       else cfg.plutusRedeemerFile
                     ;
          datum    = if cfg.plutus.datum == null
                     then null
                     else if cfg.plutusDatumFile == null
                       # File path to the Nix Store
                       then pkgs.writeText
                              "plutus-datum.json"
                              (__toJSON cfg.plutus.datum)
                       # Config supplied file path.
                       else cfg.plutusDatumFile
                     ;
          inherit (cfg.plutus) limitExecutionMem limitExecutionSteps;
        };
      targetNodes = targetNodesList cfg.targetNodes;
      era = capitalise cfg.era;
      inherit
        add_tx_size
        debugMode
        drep_voting
        init_cooldown
        inputs_per_tx
        localNodeSocketPath
        min_utxo_value
        nodeConfigFile
        outputs_per_tx
        sigKey
        tps
        tx_count
        tx_fee;
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

  targetNodesList = targets: __attrValues (__mapAttrs
                                       (name: { ip, port, name }: { addr = ip; port = port; name = name; })
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
        plutus = {
          type                = mayOpt str   "Plutus script type.";
          script              = mayOpt str   "Name of the Plutus script from plutus-apps, prefixed with either of v1/v2.";
          limitExecutionMem   = mayOpt int   "Limit for saturation tuning: mem;  null means per-Tx limit from ProtocolParameters.";
          limitExecutionSteps = mayOpt int   "Limit for saturation tuning: steps;  null means per-Tx limit from ProtocolParameters.";
          datum               = mayOpt attrs "Plutus script datum.";
          redeemer            = mayOpt attrs "Plutus script redeemer.";
        };

        drep_voting           = mayOpt bool "Activate DRep voting workload (mutually excl. with plutus)";
        govAct                = {
          # My suspicion is that batches may be meant to correspond to
          # epochs. The staging is then:
          # 1. Issue all proposals to ever use at the beginning.
          # 2. During each epoch (gapProposalBatches epochs?):
          #    A. vote on the gapBatchSize proposals in the batch
          #    B. stripe the votes cyclically across available DReps
          #    C. keep doing it for long enough to fill the epoch
          # 3. When the grand total proposal pool runs out, stop
          # trying to keep running the tx-generator in new epochs.
          gapBatchSize        = mayOpt int   "Governance action batch size.";
          gapProposalBatches  = mayOpt int   "Number of batches of proposals.";

          # Bug, does the Conway genesis value need to be used?
          # I likely had in mind the ShelleyGenesis sgUpdateQuorum
          # It may not have been intended to be used for govAct votes.
          # Is quorum logic even used for govActs?
          # dRepVotingThresholds in the sample Conway genesis files seems
          # to be closer to the fraction seen elsewhere.
          # The controlling parameter seems to be the batch size.
          # Voting thresholds would be fractions, which are meaningless
          # because the only votes ever issued are Yes votes.
          # There is no reason to use a quorum as a limit to the number
          # of votes to issue, because the number of votes to issue is
          # the gapBatchSize.
          gapQuorum           = opt int 51   "Number of votes needed to ratify proposals.";
          # could be inconsistent with the number elsewhere
          gapDRepKeys         = mayOpt int   "Number of DRep keys.";
          gapStakeKeys        = mayOpt int   "Number of stake keys.";
          gapTPS              = opt (either float int) 0.2
                                             "TPS of governeance actions";
        };

        # Overrides the usage of Nix Store paths by default.
        plutusRedeemerFile = mayOpt str "Plutus redeemer file path.";
        plutusDatumFile    = mayOpt str "Plutus datum file path.";

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
        keepalive       = opt int 30         "Default timeout for keep-alive mini-protocol";

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
                                      "conway"
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
        (if   cfg.runScriptFile != null
         then cfg.runScriptFile
         else "${pkgs.writeText "tx-gen-config.json" (cfg.decideRunScript cfg)}")
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
