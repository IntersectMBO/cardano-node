{ config
, lib
, ... }:

with import ../../lib.nix; with lib; with builtins;
let
  cfg = config.services.cardano-node;
  envConfig = cfg.environments.${cfg.environment};
  systemdServiceName = "cardano-node${optionalString cfg.instanced "@"}";
  configFile = toFile "config.json" (toJSON (cfg.nodeConfig // (optionalAttrs (cfg.genesisHash != null) { GenesisHash = cfg.genesisHash; })));
  mkScript = cfg:
    let exec = "cardano-node";
        cmd = builtins.filter (x: x != "") [
          "${cfg.package}/bin/${exec}"
          "--genesis-file ${cfg.genesisFile}"
          "--config ${configFile}"
          "--database-path ${cfg.stateDir}/${cfg.dbPrefix}"
          "--socket-dir ${ if (cfg.runtimeDir == null) then "${cfg.stateDir}/socket" else "/run/${cfg.runtimeDir}"}"
          "--topology ${cfg.topology}"
          # "--${cfg.consensusProtocol}"
          # "--node-id ${toString cfg.nodeId}"
          "--host-addr ${cfg.hostAddr}"
          "--port ${toString cfg.port}"
          "${lib.optionalString (cfg.signingKey != null) "--signing-key ${cfg.signingKey}"}"
          "${lib.optionalString (cfg.delegationCertificate != null) "--delegation-certificate ${cfg.delegationCertificate}"}"
          "${cfg.extraArgs}"
        ];
    in ''
        echo "Starting ${exec}: '' + concatStringsSep "\"\n   echo \"" cmd + ''"
        echo "..or, once again, in a signle line:"
        echo "''                   + concatStringsSep " "              cmd + ''"
        exec ''                    + concatStringsSep " "              cmd;
in {
  options = {
    services.cardano-node = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable cardano-node, a node implementing ouroboros protocols
          (the blockchain protocols running cardano).
        '';
      };
      instanced = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable systemd service instancing.
          For details see https://fedoramagazine.org/systemd-template-unit-files/
        '';
      };
      script = mkOption {
        type = types.str;
        default = mkScript cfg;
      };
      extraOptions = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "extra command line arguments for cardano-node";
      };

      package = mkOption {
        type = types.package;
        default = (import ../nix-tools.nix {}).nix-tools.exes.cardano-node;
        defaultText = "cardano-node";
        description = ''
          The cardano-node package that should be used
        '';
      };

      environments = mkOption {
        type = types.attrs;
        default = environments;
        description = ''
          environment node will connect to
        '';
      };

      environment = mkOption {
        type = types.enum (builtins.attrNames cfg.environments);
        default = "testnet";
        description = ''
          environment node will connect to
        '';
      };

      genesisFile = mkOption {
        type = types.path;
        default = envConfig.genesisFile;
        description = ''
          Genesis json file
        '';
      };

      genesisHash = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          Hash of the genesis file
        '';
      };

      signingKey = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          Signing key
        '';
      };

      delegationCertificate = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          Delegation certificate
        '';
      };

      consensusProtocol = mkOption {
        default = "real-pbft";
        type = types.enum ["bft" "praos" "mock-pbft" "real-pbft"];
        description = ''
          Consensus initialy used by the node:
            - bft: BFT consensus algorithm
            - praos: Praos consensus algorithm
            - mock-pbft: Permissive BFT consensus algorithm using a mock ledger
            - real-pbft: Permissive BFT consensus algorithm using the real ledger
        '';
      };

      hostAddr = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = ''
          The host address to bind to
        '';
      };

      stateDir = mkOption {
        type = types.str;
        default = "/var/lib/cardano-node";
        description = ''
          Directory to store blockchain data.
        '';
      };

      runtimeDir = mkOption {
        type = types.nullOr types.str;
        default = "cardano-node";
        description = ''
          Runtime directory relative to /run
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

      port = mkOption {
        type = types.either types.int types.str;
        default = 3001;
        description = ''
          The port number
        '';
      };

      nodeId = mkOption {
        type = types.either types.int types.str;
        default = 0;
        description = ''
          The ID for this node
        '';
      };

      topology = mkOption {
        type = types.path;
        default = mkEdgeTopology {
          inherit (cfg) nodeId port;
          inherit (envConfig) edgeNodes;
        };
        description = ''
          Cluster topology
        '';
      };

      nodeConfig = mkOption {
        type = types.attrs;
        default = envConfig.nodeConfig;
        description = ''Node's configuration'';
      };
      extraArgs = mkOption {
        type = types.str;
        default = "";
        description = ''Extra CLI args for 'cardano-node'.'';
      };
    };
  };

  config = mkIf cfg.enable ( let stateDirBase = "/var/lib/"; in {
    users.groups.cardano-node.gid = 10016;
    users.users.cardano-node = {
      description = "cardano-node node daemon user";
      uid = 10016;
      group = "cardano-node";
    };

    ## TODO:  use http://hackage.haskell.org/package/systemd for:
    ##   1. only declaring success after we perform meaningful init (local state recovery)
    ##   2. heartbeat & watchdog functionality
    systemd.services."${systemdServiceName}" = {
      description   = "cardano-node node service";
      after         = [ "network.target" ];
      script = cfg.script;
      serviceConfig = {
        User = "cardano-node";
        Group = "cardano-node";
        Restart = "always";
        RuntimeDirectory = cfg.runtimeDir;
        WorkingDirectory = cfg.stateDir;
        # This assumes /var/lib/ is a prefix of cfg.stateDir.
        # This is checked as an assertion below.
        StateDirectory =  lib.removePrefix stateDirBase cfg.stateDir;
      };
    } // optionalAttrs (! cfg.instanced) {
      wantedBy = [ "multi-user.target" ];
    };
    assertions = [{
      assertion = lib.hasPrefix stateDirBase cfg.stateDir;
      message =
        "The option services.cardano-node.stateDir should have ${stateDirBase} as a prefix!";
    }];
  });
}
