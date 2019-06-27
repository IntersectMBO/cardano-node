{ config
, lib
, ... }:

with lib;
let
  cfg = config.services.cardano-node;
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

      package = mkOption {
        type = types.package;
        default = (import ../../release.nix {}).nix-tools.exes.cardano-node.x86_64-linux;
        defaultText = "cardano-node";
        description = ''
          The cardano-node package that should be used.
        '';
      };

      stateDir = mkOption {
        type = types.str;
        default = "cardano-node";
        description = ''
          Directory below /var/lib to store blockchain data.
          This directory will be created automatically using systemd's StateDirectory mechanism.
        '';
      };

      system-start-time = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "2018-12-10 15:58:06";
        description = ''
          The start time of the system. Will use current date if not defined.
        '';
      };

      consensus-protocol = mkOption {
        default = "bft";
        type = types.enum ["bft" "praos" "mock-pbft" "real-pbft"];
        description = ''
          Consensus initialy used by the node:
            - bft: BFT consensus algorithm
            - praos: Praos consensus algorithm
            - mock-pbft: Permissive BFT consensus algorithm using a mock ledger
            - real-pbft: Permissive BFT consensus algorithm using the real ledger
        '';
      };

      slot-duration = mkOption {
        type = types.int;
        default = 5;
        description = ''
          The slot duration (seconds).
        '';
      };

      host = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = ''
          The host name.
        '';
      };

      port = mkOption {
        type = types.int;
        default = 3000;
        description = ''
          The port number.
        '';
      };

      node-id = mkOption {
        type = types.int;
        default = 0;
        description = ''
          The ID for this node.
        '';
      };

      producers-peers = mkOption {
        type = types.listOf types.str;
        default = [];
        description = ''
          List of peers nodes (addr:port).
        '';
      };

      logger.config-file = mkOption {
        type = types.path;
        default = ../../configuration/log-configuration.yaml;
        description = ''
          Logger configuration file.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
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
      script = let 
        # TODO: generate logger config:
        loggerConfig = cfg.logger.config-file;

        topology = builtins.toFile "topology.json" (builtins.toJSON [{
          nodeId = cfg.node-id;
          nodeAddress = {
            addr = cfg.host;
            port = builtins.toString cfg.port;
          };
          producers = map (p: let s = builtins.split ":" p; in {
            addr = builtins.head s;
            port = builtins.head (builtins.tail (builtins.tail s));
          }) cfg.producers-peers;
        }]);

      in ''
        START_TIME=${if (cfg.system-start-time != null) then cfg.system-start-time else "`date \"+%Y-%m-%d 00:00:00\"`"}
        ${cfg.package}/bin/cardano-node --log-config ${cfg.logger.config-file} --system-start "$START_TIME" --slot-duration ${builtins.toString cfg.slot-duration} node --topology ${topology} --${cfg.consensus-protocol} --node-id ${builtins.toString cfg.node-id} --host ${cfg.host} --port ${builtins.toString cfg.port}
      '';
      serviceConfig = {
        User = "cardano-node";
        Group = "cardano-node";
        Restart = "always";
        WorkingDirectory = "/var/lib/" + cfg.stateDir;
        StateDirectory = cfg.stateDir;
      };
    };
  };
}