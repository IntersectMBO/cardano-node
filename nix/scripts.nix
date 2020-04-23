{ pkgs
, config
, customConfig
 }:
with pkgs.commonLib;
let
  inherit (pkgs) svcLib;
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
      hostAddr = "127.0.0.1";
      port = 3001;
      operationalCertificate = null;
      kesKey = null;
      vrfKey = null;
      signingKey = null;
      delegationCertificate = null;
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
    } // (builtins.removeAttrs envConfig ["nodeConfig"]);

    nodeConfig = (envConfig.nodeConfig or environments.mainnet.nodeConfig)
      // (customConfig.nodeConfig or {});

    config = defaultConfig
      // (builtins.removeAttrs customConfig ["nodeConfig"])
      // { inherit nodeConfig; };

    topologyFile = let
      edgePort = if config.useProxy then config.proxyPort else config.edgePort;
      edgeHost = if config.useProxy then config.proxyHost else config.edgeHost;
      hasCustomEdgeNodes = __hasAttr "edgeNodes" customConfig;
      hasRelaysNew = __hasAttr "relaysNew" config;
      edgeNodes = let
        relaysNodes = [ config.relaysNew ];
        edgeNodes' = if (hasCustomEdgeNodes || !hasRelaysNew) then config.edgeNodes else relaysNodes;
      in if config.useProxy then [] else edgeNodes';
    in config.topologyFile or mkEdgeTopology {
      inherit (config) hostAddr port;
      inherit edgeNodes edgeHost edgePort;
    };
    serviceConfig = {
      inherit environments;
      inherit (config)
        stateDir
        socketPath
        operationalCertificate
        kesKey
        vrfKey
        signingKey
        delegationCertificate
        hostAddr
        port
        nodeConfig
        nodeId
        dbPrefix
        tracingVerbosity;
      runtimeDir = null;
      environment = envConfig.name;
      topology = topologyFile;
      nodeConfigFile = "${__toFile "config-${toString config.nodeId}.json" (__toJSON (svcLib.mkNodeConfig config config.nodeId))}";
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
    mkdir -p "${config.stateDir}"
    ${nodeScript} $@
  '';
  scripts = forEnvironments (environment:
  {
    node = mkNodeScript environment;
    chairman = svcLib.mkChairmanScript;
  });
in scripts
