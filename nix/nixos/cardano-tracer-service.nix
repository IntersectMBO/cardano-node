{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with builtins; let
  inherit (types) ints nullOr str;

  cfg = config.services.cardano-tracer;

  configFile =
    if !isNull cfg.configFile
    then cfg.configFile
    else toFile "cardano-tracer-config.json" (toJSON tracerConfig);

  tracerConfig =
    {
      ekgRequestFreq = 1;

      logging = [
        {
          logFormat = "ForHuman";
          logMode = "JournalMode";
          logRoot = "/tmp/cardano-node-logs";
        }
      ];

      network = {
        contents = "/tmp/forwarder.sock";
        tag = "AcceptAt";
      };

      networkMagic = cfg.networkMagic;

      resourceFreq = null;

      rotation = {
        rpFrequencySecs = 15;
        rpKeepFilesNum = 10;
        rpLogLimitBytes = 1000000000;
        rpMaxAgeHours = 24;
      };
    }
    // optionalAttrs cfg.ekgEnable {
      hasEKG = {
        epHost = cfg.ekgHost;
        epPort = cfg.ekgPort;
      };
    }
    // optionalAttrs cfg.prometheusEnable {
      hasPrometheus = {
        epHost = cfg.prometheusHost;
        epPort = cfg.prometheusPort;
      };
    }
    // optionalAttrs cfg.rtviewEnable {
      hasRTView = {
        epHost = cfg.rtviewHost;
        epPort = cfg.rtviewPort;
      };
    };

  mkScript = let
    cmd =
      filter (x: x != "")
      [
        "${cfg.executable} run"
        "--config ${configFile}"
      ]
      ++ cfg.extraArgs
      ++ cfg.rtsArgs;
  in ''
    echo "Starting: ${concatStringsSep "\"\n   echo \"" cmd}"
    echo "..or, once again, in a single line:"
    echo "${toString cmd}"
    ${toString cmd}
  '';
  # runtimeDir = i : if cfg.runtimeDir i == null then cfg.stateDir i else "${cfg.runDirBase}${removePrefix cfg.runDirBase (cfg.runtimeDir i)}";
  # suffixDir = base: i: "${base}${optionalString (i != 0) "-${toString i}"}";
in {
  options = {
    services.cardano-tracer = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable cardano-tracer, a service for logging and monitoring of
          Cardano nodes. After it is connected to the node(s), it periodically
          asks for different information, receives it, and handles it.
        '';
      };

      #####################################
      #                                   #
      # Alphabetical nixos module options #
      #                                   #
      #####################################

      asserts = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to use an executable with asserts enabled.
        '';
      };

      cardanoNodePackages = mkOption {
        type = types.attrs;
        default = pkgs.cardanoNodePackages or (import ../. {inherit (pkgs) system;}).cardanoNodePackages;
        defaultText = "cardano-node packages";
        description = ''
          The cardano-node packages and library that should be used. The main
          use case is for a sharing optimization which reduces eval time when
          cardano node packages are instantiated multiple times.
        '';
      };

      configFile = mkOption {
        type = nullOr str;
        default = null;
        description = ''
          The actual cardano-tracer configuration file. If this option is set
          to null, a default configuration file will be built based on the nix
          options.
        '';
      };

      ekgEnable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to enable an EKG http interface for process monitoring.
        '';
      };

      ekgHost = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = ''
          The host to bind if EKG is enabled.
        '';
      };

      ekgPort = mkOption {
        type = types.port;
        default = 12788;
        description = ''
          The port to listen on if EKG is enabled.
        '';
      };

      environment = mkOption {
        type = types.enum (attrNames cfg.environments);
        default = "preview";
        description = ''
          The environment cardano-tracer will connect to.
        '';
      };

      environments = mkOption {
        type = types.attrs;
        default = cfg.cardanoNodePackages.cardanoLib.environments;
        description = ''
          The environments cardano-tracer will possibly utilize.
        '';
      };

      eventlog = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable eventlog profiling.
        '';
      };

      executable = mkOption {
        type = types.str;
        default = "exec ${cfg.package}/bin/cardano-tracer";
        defaultText = "cardano-node";
        description = ''
          The cardano-tracer executable invocation to use.
        '';
      };

      extraArgs = mkOption {
        type = types.listOf types.str;
        default = [];
        description = ''
          Extra CLI args for cardano-tracer.
        '';
      };

      networkMagic = mkOption {
        type = ints.positive;
        default = (fromJSON (readFile cfg.environments.${cfg.environment}.nodeConfig.ShelleyGenesisFile)).networkMagic;
        description = ''
          The network magic of the cardano environment which will be connected
          with cardano-tracer.
        '';
      };

      package = mkOption {
        type = types.package;
        default =
          if (cfg.profiling != "none")
          then cfg.cardanoNodePackages.cardano-tracer.passthru.profiled
          else if cfg.eventlog
          then cfg.cardanoNodePackages.cardano-tracer.passthru.eventlogged
          else if cfg.asserts
          then cfg.cardanoNodePackages.cardano-tracer.passthru.asserted
          else cfg.cardanoNodePackages.cardano-tracer;
        defaultText = "cardano-tracer";
        description = ''
          The cardano-tracer package that should be used.
        '';
      };

      profiling = mkOption {
        type = types.enum [
          "none"
          "space"
          "space-bio"
          "space-closure"
          "space-cost"
          "space-heap"
          "space-module"
          "space-retainer"
          "space-type"
          "time"
          "time-detail"
        ];
        default = "none";
        description = ''
          Haskell profiling types which are available and will be applied to
          the cardano-tracer binary if declared.
        '';
      };

      profilingArgs = mkOption {
        type = types.listOf types.str;
        default = let
          commonProfilingArgs =
            ["--machine-readable" "-tcardano-tracer.stats" "-pocardano-tracer"]
            ++ optional (cfg.eventlog) "-l";
        in
          if cfg.profiling == "time"
          then ["-p"] ++ commonProfilingArgs
          else if cfg.profiling == "time-detail"
          then ["-P"] ++ commonProfilingArgs
          else if cfg.profiling == "space"
          then ["-h"] ++ commonProfilingArgs
          else if cfg.profiling == "space-cost"
          then ["-hc"] ++ commonProfilingArgs
          else if cfg.profiling == "space-module"
          then ["-hm"] ++ commonProfilingArgs
          else if cfg.profiling == "space-closure"
          then ["-hd"] ++ commonProfilingArgs
          else if cfg.profiling == "space-type"
          then ["-hy"] ++ commonProfilingArgs
          else if cfg.profiling == "space-retainer"
          then ["-hr"] ++ commonProfilingArgs
          else if cfg.profiling == "space-bio"
          then ["-hb"] ++ commonProfilingArgs
          else if cfg.profiling == "space-heap"
          then ["-hT"] ++ commonProfilingArgs
          else [];
        description = ''
          RTS profiling options.
        '';
      };

      prometheusEnable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to enable a prometheus export of EKG metrics.
        '';
      };

      prometheusHost = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = ''
          The host to bind if prometheus is enabled.
        '';
      };

      prometheusPort = mkOption {
        type = types.port;
        default = 12798;
        description = ''
          The port to listen on if prometheus is enabled.
        '';
      };

      rts_flags_override = mkOption {
        type = types.listOf types.str;
        default = [];
        description = ''
          RTS flags override from profile content.
        '';
      };

      rtsArgs = mkOption {
        type = types.listOf types.str;
        default = [];
        apply = args:
          if (args != [] || cfg.profilingArgs != [] || cfg.rts_flags_override != [])
          then ["+RTS"] ++ cfg.profilingArgs ++ args ++ cfg.rts_flags_override ++ ["-RTS"]
          else [];
        description = ''
          Extra CLI args for cardano-node, to be surrounded by "+RTS"/"-RTS"
        '';
      };

      rtviewEnable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable an RTView client.

          As of node release 9.1 this option has no effect unless node was
          built with `-f +rtview`.

          Ref:
          https://github.com/IntersectMBO/cardano-node/pull/5846
        '';
      };

      rtviewHost = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = ''
          The host to bind if RTView is enabled.
        '';
      };

      rtviewPort = mkOption {
        type = types.port;
        default = 3300;
        description = ''
          The port to listen on if RTView is enabled.
        '';
      };

      # hostAddr = mkOption {
      #   type = types.str;
      #   default = "127.0.0.1";
      #   description = ''
      #     The host address to bind to
      #   '';
      # };

      # stateDirBase = mkOption {
      #   type = types.str;
      #   default = "/var/lib/";
      #   description = ''
      #     Base directory to store blockchain data, for each instance.
      #   '';
      # };

      # stateDir = mkOption {
      #   type = funcToOr types.str;
      #   default = "${cfg.stateDirBase}cardano-node";
      #   apply = x : if (isFunction x) then x else i: x;
      #   description = ''
      #     Directory to store blockchain data, for each instance.
      #   '';
      # };

      # runDirBase = mkOption {
      #   type = types.str;
      #   default = "/run/";
      #   description = ''
      #     Base runtime directory, for each instance.
      #   '';
      # };

      # runtimeDir = mkOption {
      #   type = funcToOr nullOrStr;
      #   default = i: ''${cfg.runDirBase}${suffixDir "cardano-node" i}'';
      #   apply = x : if isFunction x then x else if x == null then _: null else "${cfg.runDirBase}${suffixDir "cardano-node" x}";
      #   description = ''
      #     Runtime directory relative to ${cfg.runDirBase}, for each instance
      #   '';
      # };

      # databasePath = mkOption {
      #   type = funcToOr types.str;
      #   default = i : "${cfg.stateDir i}/${cfg.dbPrefix i}";
      #   apply = x : if isFunction x then x else _ : x;
      #   description = ''Node database path, for each instance.'';
      # };

      # socketPath = mkOption {
      #   type = funcToOr types.str;
      #   default = i : "${runtimeDir i}/node.socket";
      #   apply = x : if isFunction x then x else _ : x;
      #   description = ''Local communication socket path, for each instance.'';
      # };

      # tracerSocketPathAccept = mkOption {
      #   type = funcToOr nullOrStr;
      #   default = null;
      #   apply = x : if isFunction x then x else _ : x;
      #   description = ''
      #     Listen for incoming cardano-tracer connection on a local socket,
      #     for each instance.
      #   '';
      # };

      # tracerSocketPathConnect = mkOption {
      #   type = funcToOr nullOrStr;
      #   default = null;
      #   apply = x : if isFunction x then x else _ : x;
      #   description = ''
      #     Connect to cardano-tracer listening on a local socket,
      #     for each instance.
      #   '';
      # };

      # socketGroup = mkOption {
      #   type = types.str;
      #   default = "cardano-node";
      #   description = ''
      #     systemd socket group owner.
      #     Note: only applies to sockets created by systemd
      #     (ie. when `systemdSocketActivation` is turned on).
      #   '';
      # };

      # systemdSocketActivation = mkOption {
      #   type = types.bool;
      #   default = false;
      #   description = ''Use systemd socket activation'';
      # };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.cardano-tracer = {
      description = "cardano-tracer service";
      wantedBy = ["multi-user.target"];

      environment.HOME = "/var/lib/cardano-tracer";

      path = [cfg.package];

      # Allow up to 10 failures with 30 second restarts in a 15 minute window
      # before entering failure state and alerting
      startLimitBurst = 10;
      startLimitIntervalSec = 900;

      serviceConfig = {
        LimitNOFILE = "65535";

        StateDirectory = "cardano-tracer";
        WorkingDirectory = "/var/lib/cardano-tracer";

        # Ensure quick restarts on any condition
        Restart = "always";
        RestartSec = 30;

        ExecStart = getExe (pkgs.writeShellApplication {
          name = "cardano-tracer";
          text = mkScript;
        });
      };
    };
  };

  #   systemd.sockets = genInstanceConf (n: i: mkIf cfg.systemdSocketActivation (recursiveUpdate {
  #     description = "Socket of the ${n} service.";
  #     wantedBy = [ "sockets.target" ];
  #     partOf = [ "${n}.service" ];
  #     socketConfig = {
  #       ListenStream = [ "${cfg.hostAddr}:${toString (if cfg.shareIpv4port then cfg.port else cfg.port + i)}" ]
  #         ++ optional (cfg.ipv6HostAddr i != null) "[${cfg.ipv6HostAddr i}]:${toString (if cfg.shareIpv6port then cfg.port else cfg.port + i)}"
  #         ++ (cfg.additionalListenStream i)
  #         ++ [(cfg.socketPath i)];
  #       RuntimeDirectory = removePrefix cfg.runDirBase (cfg.runtimeDir i);
  #       NoDelay = "yes";
  #       ReusePort = "yes";
  #       SocketMode = "0660";
  #       SocketUser = "cardano-node";
  #       SocketGroup = cfg.socketGroup;
  #       FreeBind = "yes";
  #     };
  #   } (cfg.extraSocketConfig i)));
  # }
}
# pkgs:
# let serviceConfigToJSON =
#       cfg:
#       {
#         inherit (cfg) networkMagic resourceFreq metricsHelp;
#         # loRequestNum = 100;
#         network =
#           if        cfg.acceptingSocket != null
#           then {
#             tag      = "AcceptAt";
#             contents = cfg.acceptingSocket;
#           } else if cfg.connectToSocket != null
#           then {
#             tag      = "ConnectTo";
#             contents = cfg.connectToSocket;
#           } else
#             throw "cardano-tracer-service:  either acceptingSocket or connectToSocket must be provided.";
#         logging = [{
#           inherit (cfg) logRoot;
#
#           logMode   = "FileMode";
#           logFormat = "ForMachine";
#         }];
#         rotation = {
#           rpFrequencySecs = 15;
#           rpKeepFilesNum  = 10;
#           rpLogLimitBytes = 1000000000;
#           rpMaxAgeHours   = 24;
#         } // (cfg.rotation or {});
#
#         hasEKG = {
#           epHost  = "127.0.0.1";
#           epPort  = cfg.ekgPortBase;
#         };
#         ekgRequestFreq = 1;
#         hasPrometheus = {
#           epHost    = "127.0.0.1";
#           epPort    = 3200; ## supervisord.portShiftPrometheus
#         } // (cfg.prometheus or {});
#         # Just an example for metrics compatibility mapping.
#         # An entry means the first entry has the second entry as alias.
#         # The Metrics is then avalable, both with the original and the mapped name.
#         # Only one mapping per message is supported.
#         # metricsComp = {
#         #     "Mempool.TxsInMempool" = "Mempool.TxsInMempool.Mapped";
#         #     "ChainDB.SlotNum" = "ChainDB.SlotNum.Mapped";
#         # };
#       } // pkgs.optionalAttrs ((cfg.RTView or {}) != {})
#       {
#         hasRTView = cfg.RTView;
#       };
# in pkgs.commondefServiceModule
#   (lib: with lib;
#     { svcName = "cardano-tracer";
#       svcDesc = "Cardano trace processor";
#
#       svcPackageSelector =
#         pkgs: ## Local:
#               pkgs.cardanoNodePackages.cardano-tracer
#               ## Imported by another repo, that adds an overlay:
#                 or pkgs.cardano-tracer;
#               ## TODO:  that's actually a bit ugly and could be improved.
#       ## This exe has to be available in the selected package.
#       exeName = "cardano-tracer";
#
#       extraOptionDecls = {
#         ### You can actually change those!
#         networkMagic    = opt    int 764824073 "Network magic (764824073 for Cardano mainnet).";
#         acceptingSocket = mayOpt str           "Socket path: as acceptor.";
#         connectToSocket = mayOpt str           "Socket path: connect to.";
#         logRoot         = opt    str null      "Log storage root directory.";
#         rotation        = opt    attrs {}      "Log rotation overrides: see cardano-tracer documentation.";
#         RTView          = opt    attrs {}      "RTView config overrides: see cardano-tracer documentation.";
#         ekgPortBase     = opt    int 3100      "EKG port base.";
#         ekgRequestFreq  = opt    int 1         "EKG request frequency";
#         prometheus      = opt    attrs {}      "Prometheus overrides: see cardano-tracer documentation.";
#         resourceFreq    = mayOpt int           "Frequency (1/ms) for tracing resource usage.";
#         metricsHelp     = mayOpt str           "JSON file containing metrics help annotations for Prometheus";
#
#         ### Here be dragons, on the other hand..
#         configFile      = mayOpt str
#           "Config file path override -- only set if you know what you're doing. Shudder. Your 'eminence'..";
#         configJSONfn    = opt (functionTo attrs) serviceConfigToJSON
#           "This is NOT meant to be overridden, at all -- we only expose it so it's externally accessible.";
#       };
#
#       configExeArgsFn = cfg: [
#         "--config" (if cfg.configFile != null then cfg.configFile
#                     else "${pkgs.writeText "cardano-tracer-config.json"
#                                            (__toJSON (serviceConfigToJSON cfg))}")
#         ];
#
#       configSystemdExtraConfig = _: {};
#
#       configSystemdExtraServiceConfig =
#         cfg: with cfg; {
#           Type = "exec";
#           User = "cardano-node";
#           Group = "cardano-node";
#           Restart = "always";
#           # RuntimeDirectory = localNodeConf.runtimeDir;
#           # WorkingDirectory = localNodeConf.stateDir;
#         };
#     })

