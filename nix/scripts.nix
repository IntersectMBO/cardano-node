{ commonLib, customConfig }:
with commonLib.pkgs.lib;
let
  pkgs = commonLib.pkgs;
  svcLib = import ./nixos/svclib.nix { inherit pkgs; };
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
      nodeId = 0;
      stateDir = "./";
      # defaults to proxy if env has no relays
      edgeHost = "127.0.0.1";
      edgeNodes = [];
      edgePort = 3001;
      useProxy = false;
      proxyPort = 7777;
      proxyHost = "127.0.0.1";
      nodeConfig = import ../configuration/default-node-config.nix;
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
        hostAddr
        port
        nodeConfig
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
  scripts = commonLib.forEnvironments (environment:
  {
    node = mkNodeScript environment;
    chairman = svcLib.mkChairmanScript;
  });
in scripts
