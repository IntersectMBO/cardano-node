{ config
, lib
, pkgs
, ... }:

with import ../../lib.nix; with lib; with builtins;
let
  cfg  = config.services.chairman;
  ncfg = config.services.cardano-node;
  svcLib = (import ../svclib.nix { inherit pkgs cardano-node; });
  envConfig = environments.${cfg.environment};
  mkChairmanConfig = nodeConfig: chairmanConfig: {
    inherit (nodeConfig) package genesisFile genesisHash stateDir pbftThreshold consensusProtocol;
    inherit (chairmanConfig) timeout maxBlockNo k slot-length node-ids nodeConfigFile nodeConfig timeoutIsSuccess;
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
          "--max-block-no ${toString cfg.maxBlockNo}"
          "--security-param ${toString cfg.k}"
          "--genesis-file ${cfg.genesisFile}"
          "--socket-dir ${if (ncfg.runtimeDir == null) then "${ncfg.stateDir}/socket" else "/run/${ncfg.runtimeDir}"}"
          "--config ${cfg.nodeConfigFile}"
          "${optionalString cfg.timeoutIsSuccess "--timeout-is-success"}"
        ];
    in ''
        set +e
        echo "Starting ${exec}: '' + concatStringsSep "\"\n   echo \"" cmd + ''"
        echo "..or, once again, in a single line:"
        echo "''                   + concatStringsSep " "              cmd + ''"
        ''                         + concatStringsSep " "              cmd + ''

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
      environment = mkOption {
        type = types.enum (builtins.attrNames environments);
        default = "testnet";
        description = ''
          environment node will connect to
        '';
      };
      timeoutIsSuccess = mkOption {
        type = types.bool;
        default = true;
        description = ''Consider timing out as success.'';
      };
      nodeConfig = mkOption {
        type = types.attrs;
        default = envConfig.nodeConfig;
        description = ''Internal representation of the config.'';
      };
      nodeConfigFile = mkOption {
        type = types.str;
        default = "${toFile "config-0.json" (toJSON (svcLib.mkNodeConfig cfg 0))}";
        description = ''Actual configuration file (shell expression).'';
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
