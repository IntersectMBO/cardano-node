{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with builtins; let
  inherit (types) attrs attrsOf bool either enum ints listOf package path port nullOr str submodule;

  cfg = config.services.cardano-tracer;

  configFile =
    if !isNull cfg.configFile
    then cfg.configFile
    else if isNull cfg.configFilePath
    then prettyConfig
    else cfg.configFilePath;

  tracerConfig =
    {
      inherit
        (cfg)
        ekgRequestFreq
        ekgRequestFull
        loRequestNum
        logging
        metricsHelp
        metricsNoSuffix
        networkMagic
        resourceFreq
        verbosity
        ;

      network =
        optionalAttrs (!isNull cfg.acceptAt) {
          tag = "AcceptAt";
          contents = cfg.acceptAt;
        }
        // optionalAttrs (!isNull cfg.connectTo) {
          tag = "ConnectTo";
          contents = cfg.connectTo;
        };

      rotation =
        if isNull cfg.rotation
        then null
        else
          {
            inherit
              (cfg.rotation)
              rpFrequencySecs
              rpKeepFilesNum
              rpLogLimitBytes
              ;
          }
          // optionalAttrs (!isNull cfg.rotation.rpMaxAgeHours) {
            inherit (cfg.rotation) rpMaxAgeHours;
          }
          // optionalAttrs (!isNull cfg.rotation.rpMaxAgeMinutes) {
            inherit (cfg.rotation) rpMaxAgeMinutes;
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
    }
    // cfg.extraConfig;

  prettyConfig =
    (pkgs.runCommandNoCCLocal "cardano-tracer-config.json" {} ''
      ${getExe pkgs.jq} --sort-keys \
        < ${toFile "cardano-tracer-unpretty-config.json" (toJSON tracerConfig)} \
        > $out
    '')
    .out;

  mkScript = let
    cmd =
      filter (x: x != "")
      [
        "${cfg.executable}"
        "--config ${configFile}"
      ]
      ++ optionals (!isNull cfg.minLogSeverity) [
        "--min-log-severity ${cfg.minLogSeverity}"
      ]
      ++ optionals (!isNull cfg.stateDir) [
        "--state-dir ${cfg.stateDir}"
      ]
      ++ cfg.extraArgs
      ++ cfg.rtsArgs;
  in ''
    echo "Starting: ${concatStringsSep "\"\n   echo \"" cmd}"

    echo "..or, once again, in a single line:"
    echo "${toString cmd}"

    ${toString cmd}
  '';

  runtimeDir = if cfg.runtimeDir == null then cfg.stateDir else "${cfg.runDirBase}${removePrefix cfg.runDirBase cfg.runtimeDir}";
in {
  imports = [
    (mkRenamedOptionModule [ "services" "cardano-tracer" "acceptingSocket" ] [ "services" "cardano-tracer" "acceptAt" ])
    (mkRenamedOptionModule [ "services" "cardano-tracer" "connectToSocket" ] [ "services" "cardano-tracer" "connectTo" ])
  ];

  options = {
    services.cardano-tracer = {
      enable = mkOption {
        type = bool;
        default = false;
        description = ''
          Enable cardano-tracer, a service for logging and monitoring of log
          and metrics providers such as cardano-node.
        '';
      };

      #####################################
      #                                   #
      # Alphabetical nixos module options #
      #                                   #
      #####################################

      acceptAt = mkOption {
        type = nullOr (either str path);
        default = "${runtimeDir}/tracer.socket";
        description = ''
          Declaring this option means that cardano-tracer will operate in an
          `AcceptAt` tag mode where cardano-tracer works as a server: it
          receives network connections from providers such as node via a single
          local socket provided by cardano-tracer or by a network listener at
          HOST:PORT.

          Except for special use cases, declaring this `acceptAt` option
          instead of the `connectTo` option is recommended, as the
          `AcceptAt` tag mode supports dynamic provider addition or removal
          without requiring cardano-tracer reconfiguration and restart.

          Either this option, or the connectTo option must be declared.
        '';
      };

      asserts = mkOption {
        type = bool;
        default = false;
        description = ''
          Whether to use an executable with asserts enabled.
        '';
      };

      cardanoNodePackages = mkOption {
        type = attrs;
        default = pkgs.cardanoNodePackages or (import ../. {inherit (pkgs) system;}).cardanoNodePackages;
        defaultText = "cardano-node packages";
        description = ''
          The cardano-node packages and library that should be used. The main
          use case is for a sharing optimization which reduces eval time when
          cardano node packages are instantiated multiple times.
        '';
      };

      configFile = mkOption {
        type = nullOr (either str path);
        default = null;
        description = ''
          The actual cardano-tracer configuration file. If this option is set
          to null, a configuration file will be built based on the nixos
          options.  The target location for a default configuration file will
          depend on the configFilePath option.
        '';
      };

      configFilePath = mkOption {
        type = nullOr (either str path);
        default = null;
        example = "/etc/cardano-tracer/config.json";
        description = ''
          If a default config file is to be used, ie the configFile option is null,
          the configFile will be located in:
            * The nix store if this option is null, or
            * Symlinked to an explicitly declared `/etc` prefixed path

          The `/etc` prefix is a requirement of the nixos implementation.
        '';
      };

      connectTo = mkOption {
        type = nullOr (listOf (either str path));
        default = null;
        description = ''
          Declaring this option means that cardano-tracer will operate in a
          `ConnectTo` tag mode where cardano-tracer works as a client: it
          establishes network connections to local socket(s) or HOST(s):PORT(s)
          provided by the provider(s). In this case a socket or HOST:PORT is
          used for each provider.

          Except for special use cases, declaring `acceptAt` instead of
          this option is recommended, as the `AcceptAt` tag mode supports
          dynamic provider addition or removal without requiring cardano-tracer
          reconfiguration and restart.

          Either this option, or the acceptAt option must be declared.
        '';
      };

      ekgEnable = mkOption {
        type = bool;
        default = true;
        description = ''
          Whether to enable an EKG http interface for process monitoring.
        '';
      };

      ekgHost = mkOption {
        type = str;
        default = "127.0.0.1";
        description = ''
          The host to bind if EKG is enabled.
        '';
      };

      ekgPort = mkOption {
        type = port;
        default = 12788;
        description = ''
          The port to listen on if EKG is enabled.
        '';
      };

      ekgRequestFull = mkOption {
        type = nullOr bool;
        default = null;
        description = ''
          When receiving forwarded metrics, cardano-tracer can request a full
          set of EKG metrics, or a delta from the previous request.

          If true, a full set of metrics will be sent on each request.
          Otherwise, on false, only a metrics delta to the previous request
          will be sent.

          If null cardano-tracer will set a default: false.
        '';
      };

      ekgRequestFreq = mkOption {
        type = nullOr ints.positive;
        default = null;
        description = ''
          This optional attribute specifies the period of how often EKG metrics
          will be requested, in seconds. For example, if ekgRequestFreq is 10,
          cardano-tracer will ask for new EKG metrics every ten seconds. There
          is no limit as loRequestNum, so every request returns all the metrics
          the provider has at that moment of time.

          If null cardano-tracer will set a default: 1.
        '';
      };

      environment = mkOption {
        type = enum (attrNames cfg.environments);
        default = "preview";
        description = ''
          The environment cardano-tracer will connect to.
        '';
      };

      environments = mkOption {
        type = attrs;
        default = cfg.cardanoNodePackages.cardanoLib.environments;
        description = ''
          The environments cardano-tracer will possibly utilize.
        '';
      };

      eventlog = mkOption {
        type = bool;
        default = false;
        description = ''
          Whether to enable eventlog profiling.
        '';
      };

      executable = mkOption {
        type = str;
        default = "exec ${cfg.package}/bin/cardano-tracer";
        description = ''
          The cardano-tracer executable invocation to use.
        '';
      };

      extraArgs = mkOption {
        type = listOf str;
        default = [];
        description = ''
          Extra CLI args for cardano-tracer.
        '';
      };

      extraConfig = mkOption {
        type = attrs;
        default = {};
        description = ''
          Extra configuration attributes for cardano-tracer which will be
          merged into default cardano-tracer configuration when option
          `configFile` is null.
        '';
      };

      group = mkOption {
        type = str;
        default = "cardano-node";
        description = ''
          The default group to run the systemd service as.

          This group is assumed to already exist.
        '';
      };

      logging = mkOption {
        type = listOf (submodule {
          options = {
            logFormat = mkOption {
              type = enum ["ForHuman" "ForMachine"];
              default = "ForHuman";
              description = ''
                The logFormat option specifies the format of logs. There are two
                possible modes: `ForMachine` and `ForHuman`. ForMachine is for JSON
                format, ForHuman is for human-friendly text format. Since the logging
                option accepts a list, more than one logging section can be declared.
              '';
            };

            logMode = mkOption {
              type = enum ["FileMode" "JournalMode"];
              default = "JournalMode";
              description = ''
                The logMode option specifies logging mode. There are two possible
                modes: `FileMode` and `JournalMode`. FileMode is for storing logs to
                the files, JournalMode is for storing them in systemd's journal. If
                you choose JournalMode, the logRoot option, will be ignored.
              '';
            };

            logRoot = mkOption {
              type = str;
              default = cfg.stateDir;
              description = ''
                The logRoot option specifies the path to the root directory. This
                directory will contain all the subdirectories with the log files
                inside. Remember that each subdirectory corresponds to the particular
                provider. If the root directory does not exist, it will be created.
              '';
            };
          };
        });
        default = [{}];
        description = ''
          The logging option describes the logging operation of cardano-tracer
          and is a list of a submodule of which each contains a `logFormat`,
          `logMode` and `logRoot`.

          See the descriptions for each available submodule option.
        '';
      };

      loRequestNum = mkOption {
        type = nullOr ints.positive;
        default = null;
        description = ''
          This optional attribute specifies the number of log items that will
          be requested from the providing source. For example, if loRequestNum
          is 10, cardano-tracer will periodically ask for 10 log items in one
          request. This value is useful for fine-tuning network traffic: it is
          possible to ask 50 log items in one request, or ask for them in 50
          requests one at a time. loRequestNum is the maximum number of log
          items. For example, if cardano-tracer requests 50 log items but the
          provider has only 40 at that moment, these 40 items will be returned,
          and the request won't block to wait for an additional 10 items.

          If null cardano-tracer will set a default: 100.
        '';
      };

      minLogSeverity = mkOption {
        type = nullOr (enum ["Debug" "Info" "Notice" "Warning" "Error" "Critical" "Alert" "Emergency"]);
        default = null;
        description = ''
          Setting this will cause cardano-tracer to drop its own log messages
          that are less severe than the level declared.

          If null cardano-tracer will set a default: Info.
        '';
      };

      metricsHelp = mkOption {
        type = nullOr (either str (attrsOf str));
        default = null;
        description = ''
          Passing metrics help annotations to cardano-tracer can be done as an
          attribute set of strings from metric's name to help text where
          cardano-tracer's internal metric names have to be used as the
          attribute names.

          If such a set is already available as JSON in a file, this option can
          be declared as a string of the path to such a file.

          Any metrics prefix declared in provider config, such as
          `TraceOptionMetricsPrefix` in cardano-node, should not be included in
          the attribute name. Similarly, metric type suffixes such as `.int` or
          `.real`, should also not be included.

          The effect of this option applies to prometheus metrics only, ie: not
          EKG.
        '';
        example = {
          "Mem.resident" = "Kernel-reported RSS (resident set size)";
          "RTS.gcMajorNum" = "Major GCs";
        };
      };

      metricsNoSuffix = mkOption {
        type = nullOr bool;
        default = null;
        description = ''
          If set true, metrics name suffixes, like "_int", will be dropped,
          thus increasing the similarity with legacy tracing system names.

          The effect of this option applies to prometheus metrics only, ie: not
          EKG.

          If null cardano-tracer will set a default: false.
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
        type = package;
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
        type = enum [
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
        type = listOf str;
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
        type = bool;
        default = true;
        description = ''
          Whether to enable a prometheus export of metrics.
        '';
      };

      prometheusHost = mkOption {
        type = str;
        default = "127.0.0.1";
        description = ''
          The host to bind if prometheus is enabled.
        '';
      };

      prometheusPort = mkOption {
        type = port;
        default = 12808;
        description = ''
          The port to listen on if prometheus is enabled.

          Defaults to the legacy prometheus listening port, 12798, plus 10.
          This avoids a conflict with the cardano-node default metrics port binding
          when PrometheusSimple backend is in use.
        '';
      };

      resourceFreq = mkOption {
        type = nullOr ints.positive;
        default = null;
        description = ''
          The period for tracing cardano-tracer's own resource usage in
          milliseconds. For example, if set to 60000, resources will traced
          every 60 seconds. If null cardano-tracer will not display resource
          usage.
        '';
      };

      rotation = mkOption {
        type = nullOr (submodule ({config, ...}: {
          options = {
            rpFrequencySecs = mkOption {
              type = ints.positive;
              description = ''
                The rpFrequencySecs option specifies the rotation check period,
                in seconds.
              '';
            };

            rpKeepFilesNum = mkOption {
              type = ints.positive;
              description = ''
                The rpKeepFilesNum option specifies the number of the log
                files that will be kept.  The last (newest) log files will
                always be the ones kept, whereas the first (oldest) log files
                will be purged.
              '';
            };

            rpLogLimitBytes = mkOption {
              type = ints.positive;
              description = ''
                The rpLogLimitBytes option specifies the maximum size of the
                log file, in bytes. Once the size of the current log file
                reaches this value, a new log file will be created.
              '';
            };

            rpMaxAgeMinutes = mkOption {
              type = nullOr ints.positive;
              default = null;
              description = ''
                The rpMaxAgeMinutes option specifies the lifetime of the log
                file in minutes. Once the log file reaches this value, it is
                treated as outdated and will be deleted.  Please note that N
                last log files, specified by option rpKeepFilesNum, will be
                kept even if they are outdated. If the rpMaxAgeHours option is
                also declared this option takes precedence.
              '';
            };

            rpMaxAgeHours = mkOption {
              type = nullOr ints.positive;
              default = null;
              description = ''
                The rpMaxAgeHours option specifies the lifetime of the log
                file in hours. Once the log file reaches this value, it is
                treated as outdated and will be deleted.  Please note that N
                last log files, specified by option rpKeepFilesNum, will be
                kept even if they are outdated. If the rpMaxAgeMinutes option
                is also declared then it takes precedence.
              '';
            };
          };
        }));
        default = {
          # Provide some sane defaults
          rpFrequencySecs = 60;
          rpKeepFilesNum = 14;
          rpMaxAgeHours = 24;
          rpLogLimitBytes = 10 * 1000 * 1000;
        };
        apply = rot:
          if isNull rot
          then rot
          else if (isNull rot.rpMaxAgeHours && isNull rot.rpMaxAgeMinutes)
          then
            throw ''
              In services.cardano-tracer.rotation at least one of
              rpMaxAgeMinutes and rpMaxAgeHours must be declared.
            ''
          else if (!isNull rot.rpMaxAgeHours && !isNull rot.rpMaxAgeMinutes)
          then
            warn ''
              In services.cardano-tracer.rotation both rpMaxAgeMinutes and
              rpMaxAgeHours have been declared.  The latter will be ignored by
              cardano-tracer.
            ''
            rot
          else rot;
        description = ''
          The rotation option describes the log rotation operation of
          cardano-tracer and is either null or a submodule with options of
          `rpFrequencySecs`, `rpKeepFilesNum`, `rpLogLimitBytes`,
          `rpMaxAgeMinutes`.

          Please note that if the rotation declaration is skipped, all log items will
          be stored in a single file, and usually that's not what is desired.

          This option will be ignored if all logging has `logMode` configured
          as `JournalMode`.

          See the descriptions for each available submodule option.
        '';
      };

      rts_flags_override = mkOption {
        type = listOf str;
        default = [];
        description = ''
          RTS flags override from profile content.
        '';
      };

      rtsArgs = mkOption {
        type = listOf str;
        default = [];
        apply = args:
          if (args != [] || cfg.profilingArgs != [] || cfg.rts_flags_override != [])
          then ["+RTS"] ++ cfg.profilingArgs ++ args ++ cfg.rts_flags_override ++ ["-RTS"]
          else [];
        description = ''
          Extra CLI args for cardano-tracer, to be surrounded by "+RTS"/"-RTS"
        '';
      };

      rtviewEnable = mkOption {
        type = bool;
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
        type = str;
        default = "127.0.0.1";
        description = ''
          The host to bind if RTView is enabled.
        '';
      };

      rtviewPort = mkOption {
        type = port;
        default = 3300;
        description = ''
          The port to listen on if RTView is enabled.
        '';
      };

      runtimeDir = mkOption {
        type = nullOr str;
        default = "${cfg.runDirBase}cardano-tracer";
        description = ''
          The directory to store any cardano-tracer runtime related data.

          If creating a cardano-tracer socket, it will default to this
          location.
        '';
      };

      runDirBase = mkOption {
        type = str;
        default = "/run/";
        description = ''
          The base runtime directory for cardano-tracer.
        '';
      };

      script = mkOption {
        type = str;
        default = mkScript;
        internal = true;
        description = ''
          The default nixos generated shell script used in the
          scripts.$ENV.tracer package attribute and workbench profile
          generation.
        '';
      };

      stateDir = mkOption {
        type = nullOr str;
        default = "${cfg.stateDirBase}cardano-tracer";
        description = ''
          The directory to store any cardano-tracer process related data.

          RTView if enabled will save its state in this directory.

          For non-systemd use cases, this can be set to null or any other
          string path.
        '';
      };

      stateDirBase = mkOption {
        type = str;
        default = "/var/lib/";
        description = ''
          The base state directory for cardano-tracer.
        '';
      };

      tracerConfig = mkOption {
        type = attrs;
        default = tracerConfig;
        internal = true;
        description = ''
          The default nixos tracerConfig attribute set used in workbench
          profile generation.
        '';
      };

      user = mkOption {
        type = str;
        default = "cardano-node";
        description = ''
          The default user to run the systemd service as.

          This user is assumed to already exist.
        '';
      };

      verbosity = mkOption {
        type = nullOr (enum ["Minimum" "ErrorsOnly" "Maximum"]);
        default = null;
        description = ''
          The verbosity optional attribute specifies the level for
          cardano-tracer itself.  There are 3 levels:

            Minimum - cardano-tracer will work as silently as possible.
            ErrorsOnly - messages about problems will be shown in standard output.
            Maximum - all the messages will be shown in standard output. Caution: the number of messages can be huge.

          If null cardano-tracer will set a default: ErrorsOnly.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    environment.etc."cardano-tracer/config.json".source = mkIf (isNull cfg.configFile && isNull cfg.configFilePath) prettyConfig;

    systemd.services.cardano-tracer = {
      description = "cardano-tracer service";
      wantedBy = ["multi-user.target"];

      # If cardano-tracer implements SIGHUP config reload support in the
      # future, this can be changed to reloadTriggers.
      restartTriggers = [
        (
          if isNull cfg.configFile
          then prettyConfig
          else configFile
        )
      ];

      environment.HOME = cfg.stateDir;

      path = [cfg.package];

      # Allow up to 10 failures with 30 second restarts in a 15 minute window
      # before entering failure state which may trigger alerts if set up.
      startLimitBurst = 10;
      startLimitIntervalSec = 900;

      serviceConfig = {
        User = cfg.user;
        Group = cfg.group;

        LimitNOFILE = "65535";

        WorkingDirectory = cfg.stateDir;
        StateDirectory = removePrefix cfg.stateDirBase cfg.stateDir;
        RuntimeDirectory = removePrefix cfg.runDirBase runtimeDir;

        # Ensure quick restarts on any condition
        Restart = "always";
        RestartSec = 30;

        ExecStart = getExe (pkgs.writeShellApplication {
          name = "cardano-tracer";
          text = mkScript;
        });
      };
    };

    assertions = [
      {
        assertion = (!isNull cfg.acceptAt) != (!isNull cfg.connectTo);
        message = "In services.cardano-tracer, exactly one of acceptAt or connectTo must be declared";
      }
      {
        assertion = isNull cfg.configFilePath || hasPrefix "/etc/" cfg.configFilePath;
        message = "In services.cardano-tracer.configFilePath a non-null path must be prefixed with `/etc/`";
      }
    ];
  };
}
