{ config
, lib
, ... }:

with import ../../lib.nix;
with lib;
let
  cfg = config.services.cardano-node;
  envConfig = environments.${cfg.environment};
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
      script = mkOption {
        type = types.str;
        default = ''
          exec ${cfg.package}/bin/cardano-node \
            --genesis-file ${cfg.genesisFile} \
            --genesis-hash ${cfg.genesisHash} \
            --log-config ${cfg.logger.configFile} \
            --database-path ${cfg.stateDir}/${cfg.dbPrefix} \
            --socket-dir ${ if (cfg.runtimeDir == null) then "${cfg.stateDir}/socket" else "/run/${cfg.runtimeDir}"} \
            node \
            --topology ${cfg.topology} \
            --${cfg.consensusProtocol} \
            --node-id ${builtins.toString cfg.nodeId} \
            --host-addr ${cfg.hostAddr} \
            --port ${builtins.toString cfg.port} \
            ${lib.optionalString (cfg.pbftThreshold != null) "--pbft-signature-threshold ${cfg.pbftThreshold}"} \
            ${lib.optionalString (cfg.signingKey != null) "--signing-key ${cfg.signingKey}"} \
            ${lib.optionalString (cfg.delegationCertificate != null) "--delegation-certificate ${cfg.delegationCertificate}"} \
        '';
      };

      package = mkOption {
        type = types.package;
        default = (import ../nix-tools.nix {}).nix-tools.exes.cardano-node;
        defaultText = "cardano-node";
        description = ''
          The cardano-node package that should be used
        '';
      };

      environment = mkOption {
        type = types.enum (builtins.attrNames environments);
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
        default = envConfig.genesisHash;
        description = ''
          Hash of the genesis file
        '';
      };

      pbftThreshold = mkOption {
        type = types.nullOr types.str;
        default = envConfig.pbftThreshold or null;
        description = ''
          PBFT Threshold
        '';
      };

      signingKey = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = ''
          Signing key
        '';
      };

      delegationCertificate = mkOption {
        type = types.nullOr types.path;
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
        type = types.int;
        default = 3001;
        description = ''
          The port number
        '';
      };

      nodeId = mkOption {
        type = types.int;
        default = 0;
        description = ''
          The ID for this node
        '';
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

      logger.configFile = mkOption {
        type = types.path;
        default = ../../configuration/log-configuration.yaml;
        description = ''
          Logger configuration file
        '';
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
    systemd.services.cardano-node = {
      description   = "cardano-node node service";
      after         = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
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
    };
    assertions = [{
      assertion = lib.hasPrefix stateDirBase cfg.stateDir;
      message =
        "The option services.cardano-node.stateDir should have ${stateDirBase} as a prefix!";
    }];
  });
}
