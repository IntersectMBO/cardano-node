{ config
, lib
, pkgs
, ... }:

with import ../../lib.nix; with builtins;
let
  cfg       = config.services.chairman;
  ncfg      = config.services.cardano-node;
  ccfg      = config.services.cardano-cluster;
  envConfig = environments.${cfg.environment};

  mkChairmanConfig = nodeConfig: chairmanConfig: {
    inherit (nodeConfig) package genesisFile genesisHash stateDir pbftThreshold consensusProtocol;
    inherit (chairmanConfig) timeout maxBlockNo k node-ids;
  };
  mkChairmanScript = cfg:
    let nodeIdArgs = builtins.concatStringsSep " "
                       (map (i: "--core-node-id ${toString i}")
                        cfg.node-ids);
    in ''
        echo "Starting chairman: ${cfg.package}/bin/chairman"
        echo "   ${nodeIdArgs}"
        echo "   --timeout ${toString cfg.timeout}"
        echo "   --max-block-no ${toString cfg.maxBlockNo}"
        echo "   --security-param ${toString cfg.k}"
        echo "   --genesis-file ${cfg.genesisFile}"
        echo "   --genesis-hash ${cfg.genesisHash}"
        echo "   --socket-path ${cfg.stateDir}"
        echo "   ${lib.optionalString (cfg.pbftThreshold != null) "--pbft-signature-threshold ${cfg.pbftThreshold}"}"
        exec ${cfg.package}/bin/chairman \
          --${cfg.consensusProtocol} \
          ${nodeIdArgs} \
          --timeout ${toString cfg.timeout} \
          --max-block-no ${toString cfg.maxBlockNo} \
          --security-param ${toString cfg.k} \
          --genesis-file ${cfg.genesisFile} \
          --genesis-hash ${cfg.genesisHash} \
          --socket-path ${cfg.stateDir} \
          ${lib.optionalString (cfg.pbftThreshold != null) "--pbft-signature-threshold ${cfg.pbftThreshold}"}
    '';
in {
  options = with types; {
    services.chairman = {
      script = mkOption {
        type = types.str;
        default = mkChairmanScript (mkChairmanConfig ncfg cfg);
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
    };
  };
}
