# The legacy cardano-tracer module requires commonLib from the overlay
# which makes a simple non-flake service module import convoluted
# and also requires knowledge of the cardano-node commonLib functions to use.
# This makes this legacy module suitable for internal purposes, but not ideal
# for a general purpose single import cardano-tracer module.
#
# The module at nix/nixos/cardano-tracer-service.nix is the preferred module
# for general purpose use.
pkgs:
let serviceConfigToJSON =
      cfg:
      {
        inherit (cfg) networkMagic resourceFreq metricsHelp;
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
          rpLogLimitBytes = 1000000000;
          rpMaxAgeHours   = 24;
        } // (cfg.rotation or {});

        hasEKG = {
          epHost  = "127.0.0.1";
          epPort  = cfg.ekgPortBase;
        };
        ekgRequestFreq = 1;
        hasPrometheus = {
          epHost    = "127.0.0.1";
          epPort    = 3200; ## supervisord.portShiftPrometheus
        } // (cfg.prometheus or {});
        # Just an example for metrics compatibility mapping.
        # An entry means the first entry has the second entry as alias.
        # The Metrics is then avalable, both with the original and the mapped name.
        # Only one mapping per message is supported.
        # metricsComp = {
        #     "Mempool.TxsInMempool" = "Mempool.TxsInMempool.Mapped";
        #     "ChainDB.SlotNum" = "ChainDB.SlotNum.Mapped";
        # };
      } // pkgs.lib.optionalAttrs ((cfg.RTView or {}) != {})
      {
        hasRTView = cfg.RTView;
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
        rotation        = opt    attrs {}      "Log rotation overrides: see cardano-tracer documentation.";
        RTView          = opt    attrs {}      "RTView config overrides: see cardano-tracer documentation.";
        ekgPortBase     = opt    int 3100      "EKG port base.";
        ekgRequestFreq  = opt    int 1         "EKG request frequency";
        prometheus      = opt    attrs {}      "Prometheus overrides: see cardano-tracer documentation.";
        resourceFreq    = mayOpt int           "Frequency (1/ms) for tracing resource usage.";
        metricsHelp     = mayOpt str           "JSON file containing metrics help annotations for Prometheus";

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
          Restart = "always";
          # RuntimeDirectory = localNodeConf.runtimeDir;
          # WorkingDirectory = localNodeConf.stateDir;
        };
    })
