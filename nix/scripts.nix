{ commonLib, customConfig }:
with commonLib.pkgs.lib;
let
  pkgs = commonLib.pkgs;
  pkgsModule = {
    config._module.args.pkgs = mkDefault pkgs;
  };
  systemdCompat.options = {
    systemd.services = mkOption {};
    assertions = [];
    users = mkOption {};
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
      edgeNodes = [];
      edgePort = 3001;
      useProxy = false;
      proxyPort = 7777;
      proxyHost = "127.0.0.1";
      loggingConfig = ../configuration/log-configuration.yaml;
      loggingExtras = null;
    };
    config = defaultConfig // envConfig // customConfig;
    topologyFile = let
      edgePort = if config.useProxy then config.proxyPort else config.edgePort;
      edgeHost = if config.useProxy then config.proxyHost else config.edgeHost;
      edgeNodes = if config.useProxy then [] else config.edgeNodes;
    in config.topologyFile or commonLib.mkEdgeTopology {
      inherit (config) hostAddr port nodeId;
      inherit edgeNodes edgeHost edgePort;
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
      runtimeDir = null;
      dbPrefix = "db-${envConfig.name}";
      topology = topologyFile;
    };
    nodeConf = { config.services.cardano-node = serviceConfig; };
    nodeScript = (modules.evalModules {
      prefix = [];
      modules = [
        ./nixos/cardano-node-service.nix
        systemdCompat
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
  mkChairmanScript = { chairman-config ? { enable = true; } }:
  let
    cardano-cluster-config = {
      enable = true;
      node-count = 3;
    };
    injectServiceConfigs = {
      config.services.chairman = chairman-config;
      config.services.cardano-cluster = cardano-cluster-config;
    };
    script = (modules.evalModules {
      modules = [
        ./nixos/cardano-node-service.nix
        ./nixos/cardano-cluster-service.nix
        ./nixos/chairman-as-a-service.nix
        injectServiceConfigs
        pkgsModule
        systemdCompat
      ];
    }).config.services.chairman.script;
  in pkgs.writeScript "chairman" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail
    ${script} $@
  '';
  scripts = commonLib.forEnvironments (environment:
  {
    node = mkNodeScript environment;
    chairman = mkChairmanScript;
  });
in scripts
