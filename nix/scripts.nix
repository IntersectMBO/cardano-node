{ pkgs
, customConfig
 }:
with pkgs.commonLib;
let
  mkNodeScript = envConfig: let
    defaultConfig = {
      cardanoNodePkgs = pkgs;
      hostAddr = "0.0.0.0";
      port = 3001;
      signingKey = null;
      delegationCertificate = null;
      kesKey = null;
      vrfKey = null;
      operationalCertificate = null;
      nodeId = 0;
      stateDir = "state-node-${envConfig.name}";
      socketPath = "${config.stateDir}/node.socket";
      # defaults to proxy if env has no relays
      edgeHost = "127.0.0.1";
      edgeNodes = [];
      edgePort = 3001;
      useProxy = false;
      proxyPort = 7777;
      proxyHost = "127.0.0.1";
      loggingExtras = null;
      tracingVerbosity = "normal";
      dbPrefix = "db-${envConfig.name}";
      extraArgs = [];
      profiling = "none";
      asserts = false;
      rtsArgs = [ "-N2" "-A16m" "-qg" "-qb" "--disable-delayed-os-memory-return" ];
      ipv6HostAddr = null;
    } // (builtins.removeAttrs envConfig ["nodeConfig"]);

    nodeConfig = (envConfig.nodeConfig or environments.mainnet.nodeConfig)
      // (customConfig.nodeConfig or {});

    config = defaultConfig
      // (builtins.removeAttrs customConfig ["nodeConfig"])
      // { inherit nodeConfig; };

    serviceConfig = {
      inherit environments;
      inherit (config)
        cardanoNodePkgs
        stateDir
        socketPath
        signingKey
        delegationCertificate
        kesKey
        vrfKey
        operationalCertificate
        hostAddr
        port
        nodeConfig
        nodeId
        dbPrefix
        tracingVerbosity
        extraArgs
        rtsArgs
        profiling
        asserts
        ;
      runtimeDir = null;
      environment = envConfig.name;
    } // (optionalAttrs (envConfig ? topology || customConfig ? topology) {
      topology = customConfig.topology or envConfig.topology;
    });
    nodeConf = { config.services.cardano-node = serviceConfig; };
    nodeScript = (modules.evalModules {
      prefix = [];
      modules = [
        ./nixos/cardano-node-service.nix
        systemdCompatModule
        nodeConf
      ];
      args = { inherit pkgs; };
      check = false;
    }).config.services.cardano-node.script;
  in pkgs.writeScript "cardano-node-${envConfig.name}" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail
    mkdir -p "${config.stateDir}"
    ${nodeScript} $@
  '';
  scripts = forEnvironments (environment: recurseIntoAttrs {
    node = mkNodeScript environment;
  });
in scripts
