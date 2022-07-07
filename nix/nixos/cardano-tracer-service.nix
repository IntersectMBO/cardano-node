pkgs:
let serviceConfigToJSON =
      cfg:
      {
        inherit (cfg) networkMagic;
        # loRequestNum = 100;
        network =
          if        cfg.acceptingSocket != null
          then {
            tag      = "AcceptAt";
            contents = cfg.acceptingSocket;
          } else if cfg.connectToSocket != null
          then {
            tag      = "ConnectTo";
            contents = cfg.connectToSocket;
          } else
            throw "cardano-tracer-service:  either acceptingSocket or connectToSocket must be provided.";
        logging = [{
          inherit (cfg) logRoot;

          logMode   = "FileMode";
          logFormat = "ForMachine";
        }];
        rotation = {
          rpFrequencySecs = 15;
          rpKeepFilesNum  = 10;
          rpLogLimitBytes = 500000000;
          rpMaxAgeHours   = 24;
        };

        hasRTView = {
          epHost    = "127.0.0.1";
          epPort    = 3300;
        };
        hasEKG = [
          { epHost    = "127.0.0.1";
            epPort    = 3100; ## supervisord.portShiftPrometheus
          }
          { epHost    = "127.0.0.1";
            epPort    = 3101; ## supervisord.portShiftPrometheus
          }
        ];
        ekgRequestFreq = 1;
        hasPrometheus = {
          epHost    = "127.0.0.1";
          epPort    = 3200; ## supervisord.portShiftPrometheus
        };
      };
in pkgs.commonLib.defServiceModule
  (lib: with lib;
    { svcName = "cardano-tracer";
      svcDesc = "Cardano trace processor";

      svcPackageSelector =
        pkgs: ## Local:
              pkgs.cardanoNodePackages.cardano-tracer
              ## Imported by another repo, that adds an overlay:
                or pkgs.cardano-tracer;
              ## TODO:  that's actually a bit ugly and could be improved.
      ## This exe has to be available in the selected package.
      exeName = "cardano-tracer";

      extraOptionDecls = {
        ### You can actually change those!
        networkMagic    = opt    int 764824073 "Network magic (764824073 for Cardano mainnet).";
        acceptingSocket = mayOpt str           "Socket path: as acceptor.";
        connectToSocket = mayOpt str           "Socket path: connect to.";
        logRoot         = opt    str null      "Log storage root directory.";

        ### Here be dragons, on the other hand..
        configFile      = mayOpt str
          "Config file path override -- only set if you know what you're doing. Shudder. Your 'eminence'..";
        configJSONfn    = opt (functionTo attrs) serviceConfigToJSON
          "This is NOT meant to be overridden, at all -- we only expose it so it's externally accessible.";
      };

      configExeArgsFn = cfg: [
        "--config" (if cfg.configFile != null then cfg.configFile
                    else "${pkgs.writeText "cardano-tracer-config.json"
                                           (__toJSON (serviceConfigToJSON cfg))}")
        ];

      configSystemdExtraConfig = _: {};

      configSystemdExtraServiceConfig =
        cfg: with cfg; {
          Type = "exec";
          User = "cardano-node";
          Group = "cardano-node";
          Restart = "yes";
          # RuntimeDirectory = localNodeConf.runtimeDir;
          # WorkingDirectory = localNodeConf.stateDir;
        };
    })
