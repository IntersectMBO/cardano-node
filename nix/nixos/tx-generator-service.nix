(import ../. {}).commonLib.defServiceModule
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
        ## TODO: the defaults should be externalised to a file.
        ##
        tx_count        =  intOpt 1000       "How many Txs to send, total.";
        add_tx_size     =  intOpt 100        "Extra Tx payload, in bytes.";
        inputs_per_tx   =  intOpt 4          "Inputs per Tx.";
        outputs_per_tx  =  intOpt 4          "Outputs per Tx.";
        tx_fee          =  intOpt 10000000   "Tx fee, in Lovelace.";
        tps             =  intOpt 100        "Strength of generated load, in TPS.";
        init_cooldown   =  intOpt 100        "Delay between init and main submissions.";

        nodeConfigFile  =  strOpt null       "Node-style config file path.";
        nodeConfig      = attrOpt {}         "Node-style config, overrides the default.";
        sigKey          =  strOpt null       "Key with funds";

        localNodeSocketPath =
                           strOpt null       "Local node socket path";
        localNodeConf   = attrOpt null       "Config of the local node";

        targetNodes     = attrOpt null       "Targets: { name = { ip, port } }";

        era             = enumOpt [ "shelley"
                                    "allegra"
                                    "mary"
                                    "alonzo"
                                  ]
                                  "shelley"
                                  "Cardano era to generate transactions for.";
      };

      configExeArgsFn =
        cfg: with cfg;
          [ "cliArguments"

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
              targetNodes);

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
