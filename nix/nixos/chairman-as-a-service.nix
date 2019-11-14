{ config
, lib
, ... }:

with import ../../lib.nix; with lib; with builtins;
let
  cfg  = config.services.chairman;
  ncfg = config.services.cardano-node;
  ccfg      = config.services.cardano-cluster;
  envConfig = environments.${cfg.environment};
  configFile = toFile "config.json" (toJSON nodeConfig);
  nodeConfig = (import ncfg.configFile) { inherit cfg; };

  mkChairmanConfig = nodeConfig: chairmanConfig: {
    inherit (nodeConfig) package genesisFile genesisHash stateDir pbftThreshold consensusProtocol;
    inherit (chairmanConfig) timeout maxBlockNo k node-ids topology dbPrefix;
  };
  mkScript = cfg:
    let nodeIdArgs = builtins.concatStringsSep " "
                       (map (i: "--core-node-id ${toString i}")
                         cfg.node-ids);
        exec = "chairman";
        cmd = builtins.filter (x: x != "") [
          "${ncfg.package}/bin/chairman"
          "--${ncfg.consensusProtocol}"
          (nodeIdArgs)
          "--timeout ${toString cfg.timeout}"
          "--database-path ${cfg.stateDir}/${cfg.dbPrefix}"
          "--max-block-no ${toString cfg.maxBlockNo}"
          "--security-param ${toString cfg.k}"
          "--genesis-file ${cfg.genesisFile}"
          "--genesis-hash ${cfg.genesisHash}"
          "--socket-dir ${ if (ncfg.runtimeDir == null) then "${ncfg.stateDir}/socket" else "/run/${ncfg.runtimeDir}"}"
          "${lib.optionalString (cfg.pbftThreshold != null) "--pbft-signature-threshold ${cfg.pbftThreshold}"}"
          "--topology ${cfg.topology}"
          "--port 1234"
          "--database-path ${cfg.stateDir}/${cfg.dbPrefix}"
          "--genesis-file ${cfg.genesisFile}"
          "--socket-dir ${ if (ncfg.runtimeDir == null) then "${ncfg.stateDir}/socket" else "/run/${ncfg.runtimeDir}"}"
          "--config ${configFile}"




        ];
    in ''
        echo "Starting ${exec}: '' + concatStringsSep "\"\n   echo \"" cmd + ''"
        echo "..or, once again, in a signle line:"
        echo "''                   + concatStringsSep " "              cmd + ''"
        exec ''                    + concatStringsSep " "              cmd;
in {
  options = with types; {
    services.chairman = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable chairman, a consensus checker service.
        '';
      };
      script = mkOption {
        type = types.str;
        default = mkScript (mkChairmanConfig ncfg cfg);
      };
      node-ids = mkOption {
        type = listOf int;
        default = range 0 (ccfg.node-count - 1);
        description = ''Node IDs to watch.'';
      };
      timeout = mkOption {
        type = int;
        default = 360;
        description = ''How long to wait for consensus of maxBlockNo blocks.'';
      };
      topology = mkOption {
        type = types.path;
        default = mkEdgeTopology {
          inherit (cfg) hostAddr nodeId port;
          edgeHost = envConfig.edgeHost or "127.0.0.1";
        };
        description = ''
          Cluster topology
        '';
      };
      dbPrefix = mkOption {
        type = types.str;
        default = "db-${cfg.environment}";
        description = ''
          Prefix of database directories inside `stateDir`.
          (eg. for "db", there will be db-0, etc.).
        '';
      };
      environment = mkOption {
        type = types.enum (builtins.attrNames environments);
        default = "testnet";
        description = ''
          environment node will connect to
        '';
      };
      configFile = mkOption {
        type = types.path;
        default = ../../configuration/default-node-config.nix;
        description = ''Node's configuration file, as a Nix expression.'';
      };
      maxBlockNo = mkOption {
        type = int;
        default = 5;
        description = ''How many blocks of consensus should we require before succeeding.'';
      };
      k = mkOption {
        type = int;
        default = 2160;
        description = ''Should come from genesis instead.'';
      };
      genesisHash = mkOption {
        type = types.str;
        default = envConfig.genesisHash;
        description = ''
          Hash of the genesis file
        '';
      };
    };
  };
  config = mkIf cfg.enable ( let stateDirBase = "/var/lib/"; in {
    systemd.services.chairman =
    let node-services = map (id: "cardano-node@${toString id}.service") cfg.node-ids;
    in {
      enable        = true;
      description   = "Chairman, the consensus checker.";
      after         = node-services;
      wantedBy      = [ "multi-user.target" ];
      script        = cfg.script;
      serviceConfig = {
        User = "cardano-node";
        Group = "cardano-node";
        Restart = "no";
        WorkingDirectory = ncfg.stateDir;
        # This assumes /var/lib/ is a prefix of cfg.stateDir.
        # This is checked as an assertion below.
        StateDirectory =  lib.removePrefix stateDirBase ncfg.stateDir;
      };
    };
    assertions = [{
      assertion = lib.hasPrefix stateDirBase ncfg.stateDir;
      message =
        "The option services.chairman.stateDir should have ${stateDirBase} as a prefix!";
    }];
  });
}
