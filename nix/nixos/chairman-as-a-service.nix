{ config
, lib
, ... }:

with import ../../lib.nix; with lib; with builtins;
let
  cfg  = config.services.chairman;
  ncfg = config.services.cardano-node;
  envConfig = environments.${cfg.environment};
  configFile = toFile "config.json" (toJSON nodeConfig);
  nodeConfig = ncfg.nodeConfig // { GenesisHash = ncfg.genesisHash; };

  mkChairmanConfig = nodeConfig: chairmanConfig: {
    inherit (nodeConfig) package genesisFile genesisHash stateDir pbftThreshold consensusProtocol;
    inherit (chairmanConfig) timeout maxBlockNo k slot-length node-ids topology dbPrefix;
  };
  mkScript = cfg:
    let nodeIdArgs = builtins.concatStringsSep " "
                       (map (i: "--core-node-id ${toString i}")
                         cfg.node-ids);
        exec = "chairman";
        cmd = [
          "${ncfg.package}/bin/chairman"
          (nodeIdArgs)
          "--timeout ${toString cfg.timeout}"
          "--database-path ${cfg.stateDir}/${cfg.dbPrefix}"
          "--max-block-no ${toString cfg.maxBlockNo}"
          "--security-param ${toString cfg.k}"
          "--genesis-file ${cfg.genesisFile}"
          "--genesis-hash ${cfg.genesisHash}"
          "--slot-duration ${toString cfg.slot-length}"
          "--socket-dir ${ if (ncfg.runtimeDir == null) then "${ncfg.stateDir}/socket" else "/run/${ncfg.runtimeDir}"}"
          "--topology ${cfg.topology}"
          "--port 1234"
          "--database-path ${cfg.stateDir}/${cfg.dbPrefix}"
          "--genesis-file ${cfg.genesisFile}"
          "--socket-dir ${ if (ncfg.runtimeDir == null) then "${ncfg.stateDir}/socket" else "/run/${ncfg.runtimeDir}"}"
          "--config ${configFile}"
        ];
    in ''
        set +e
        echo "Starting ${exec}: '' + concatStringsSep "\"\n   echo \"" cmd + ''"
        echo "..or, once again, in a single line:"
        echo "''                   + concatStringsSep " "              cmd + ''"
        ''                    + concatStringsSep " "              cmd + ''

        status=$?
        echo chairman returned status: $status
        exit $status'';
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
      nodeConfig = mkOption {
        type = types.attrs;
        default = import ../../configuration/default-node-config.nix;
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
      slot-length = mkOption {
        type = int;
        default = 20;
        description = ''Duration of a slot, in seconds.'';
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
