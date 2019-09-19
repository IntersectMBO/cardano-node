{ config
, lib
, ... }:

with lib; with builtins;
let
  cfg  = config.services.chairman;
  ncfg = config.services.cardano-node;
  ccfg      = config.services.cardano-cluster;
  envConfig = environments.${cfg.environment};

  mkChairmanConfig = nodeConfig: chairmanConfig: {
    inherit (nodeConfig) package genesisFile genesisHash stateDir pbftThreshold consensusProtocol;
    inherit (chairmanConfig) timeout maximum-fork-length slots-within-tolerance k node-ids;
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
          "--maximum-fork-length ${toString cfg.maximum-fork-length}"
          "--slots-within-tolerance ${toString cfg.slots-within-tolerance}"
          "--security-param ${toString cfg.k}"
          "--genesis-file ${cfg.genesisFile}"
          "--genesis-hash ${cfg.genesisHash}"
          "--socket-dir ${ if (ncfg.runtimeDir == null) then "${ncfg.stateDir}/socket" else "/run/${ncfg.runtimeDir}"}"
          "${lib.optionalString (cfg.pbftThreshold != null) "--pbft-signature-threshold ${cfg.pbftThreshold}"}"
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
      maximum-fork-length = mkOption {
        type = int;
        default = 1;
        description = ''What is the longest allowed fork that still passes consensus validation.'';
      };
      slots-within-tolerance = mkOption {
        type = int;
        default = 5;
        description = ''How many blocks of forks under threshold length should the cluster deliver for success.'';
      };
      k = mkOption {
        type = int;
        default = 2160;
        description = ''Should come from genesis instead.'';
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
