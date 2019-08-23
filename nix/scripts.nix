{ commonLib, customConfig }:
with commonLib.pkgs.lib;
let
  pkgs = commonLib.pkgs;
  pkgsModule = {
    config._module.args.pkgs = mkDefault pkgs;
  };
  mkNodeScript = envConfig: let
    defaultConfig = {
      consensusProtocol = "real-pbft";
      hostAddr = "127.0.0.1";
      port = 3001;
      signingKey = null;
      delegationCertificate = null;
      pbftThreshold = null;
      nodeId = 0;
      stateDir = "./";
      # defaults to proxy if env has no relays
      edgeHost = "127.0.0.1";
      edgePort = 3001;
      useProxy = false;
      proxyPort = "7777";
      proxyHost = "127.0.0.1";
      loggingConfig = ../configuration/log-configuration.yaml;
    };
    config = defaultConfig // envConfig // customConfig;
    topologyFile = let
      edgePort = if config.useProxy then config.proxyPort else config.edgePort;
      edgeHost = if config.useProxy then config.proxyHost else config.edgeHost;
    in config.topologyFile or commonLib.mkEdgeTopology {
      inherit (config) hostAddr port nodeId;
      inherit edgeHost edgePort;
    };
    serviceConfig = {
      inherit (config)
        genesisFile
        genesisHash
        stateDir
        signingKey
        delegationCertificate
        consensusProtocol
        pbftThreshold
        hostAddr
        port
        nodeId;
      dbPrefix = "db-${envConfig.name}";
      logger.configFile = config.loggingConfig;
      topology = topologyFile;
    };
    nodeConf = { config.services.cardano-node = serviceConfig; };
    nodeScript = (modules.evalModules {
      modules = [
        ./nixos/cardano-node-options.nix
        nodeConf
        pkgsModule
      ];
    }).config.services.cardano-node.script;
  in pkgs.writeScript "cardano-node-${envConfig.name}" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail
    mkdir -p "state-node-${envConfig.name}"
    cd "state-node-${envConfig.name}"
    ${nodeScript} $@
  '';
  scripts = commonLib.forEnvironments (environment:
  {
    node = mkNodeScript environment;
  });
in scripts
