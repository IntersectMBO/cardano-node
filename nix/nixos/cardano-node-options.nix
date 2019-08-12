{ config
, lib
, pkgs
, ... }:

with lib;
let
  cfg = config.services.cardano-node;
in {
  options = {
    services.cardano-node = {
      script = mkOption {
        type = types.str;
        default = ''
          GENESIS_START_TIME=$(${pkgs.jq}/bin/jq --raw-output '.startTime' < ${cfg.genesis-file})
          START_TIME=$(date -d "@$GENESIS_START_TIME" --utc --rfc-3339=seconds)
          SLOT_DURATION=$(${pkgs.jq}/bin/jq --raw-output '.blockVersionData.slotDuration' < ${cfg.genesis-file})
          ${cfg.package}/bin/cardano-node \
            --genesis-file ${cfg.genesis-file} \
        '' + (if (cfg.genesis-hash != null) then 
        ''  --genesis-hash ${cfg.genesis-hash} \
        '' else "") + ''
            --log-config ${cfg.logger.config-file} \
            --system-start "$START_TIME" \
            --slot-duration "$SLOT_DURATION" \
            node \
            --topology ${cfg.topology} \
            --${cfg.consensus-protocol} \
            --node-id ${builtins.toString cfg.node-id} \
            --host-addr ${cfg.host-addr} \
            --port ${builtins.toString cfg.port} \
        '' + (if (cfg.signing-key != null) then 
        ''  --signing-key ${cfg.signing-key} \
        '' else "")
          + (if (cfg.delegation-certificate != null) then 
        ''  --delegation-certificate ${cfg.delegation-certificate} \
        '' else "");
      };

      package = mkOption {
        type = types.package;
        default = (import ../nix-tools.nix {}).nix-tools.exes.cardano-node;
        defaultText = "cardano-node";
        description = ''
          The cardano-node package that should be used.
        '';
      };

      genesis-file = mkOption {
        type = types.path;
        description = ''
          Genesis json file.
        '';
      };

      genesis-hash = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          Hash of the genesis file.
        '';
      };

      signing-key = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = ''
          Signing key
        '';
      };

      delegation-certificate = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = ''
          Delegation certificate.
        '';
      };

      consensus-protocol = mkOption {
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

      slot-duration = mkOption {
        type = types.int;
        default = 5;
        description = ''
          The slot duration (seconds).
        '';
      };

      host-addr = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = ''
          The host address to bind to.
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

      topology = mkOption {
        type = types.path;
        description = ''
          Cluster topology.
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
}
