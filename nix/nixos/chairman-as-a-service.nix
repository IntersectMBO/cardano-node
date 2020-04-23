{ config
, lib
, pkgs
, ... }:

with pkgs.commonLib; with lib; with builtins;
let
  inherit (pkgs) svcLib;
  cfg  = config.services.chairman;
  ncfg = config.services.cardano-node;
  chairman = pkgs.cardanoNodeHaskellPackages.cardano-node.components.exes.chairman;
  envConfig = environments.${cfg.environment};
  mkChairmanConfig = nodeConfig: chairmanConfig: {
    inherit (nodeConfig) package genesisFile genesisHash genesisHashPath stateDir pbftThreshold;
    inherit (chairmanConfig) timeout k slot-length node-ids nodeConfigFile nodeConfig;
  };
  mkScript = cfg:
    let runtimeDir = if ncfg.runtimeDir == null then ncfg.stateDir else "/run/${ncfg.runtimeDir}";
        socketArgs = builtins.toString
                       (map (i: "--socket-path ${runtimeDir}/node-${toString i}.socket")
                         cfg.node-ids);
        exec = "chairman";
        cmd = [
          "${chairman}/bin/chairman"
          (socketArgs)
          "--timeout ${toString cfg.timeout}"
          "--config ${cfg.nodeConfigFile}"
        ];
    in ''
        GENESIS_HASH=${if (cfg.genesisHash == null) then "$(cat ${cfg.genesisHashPath})" else cfg.genesisHash}
        set +e
        echo "Starting ${exec}: '' + concatStringsSep "\"\n   echo \"" cmd + ''"
        echo "..or, once again, in a single line:"
        echo "''                   + toString                          cmd + ''"
        ''                         + toString                          cmd + ''

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
        description = ''How long to wait for consensus.'';
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
        default = envConfig.nodeConfig;
        description = ''Internal representation of the config.'';
      };
      nodeConfigFile = mkOption {
        type = types.str;
        default = "${toFile "config-0.json" (toJSON (svcLib.mkNodeConfig cfg 0))}";
        description = ''Actual configuration file (shell expression).'';
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
