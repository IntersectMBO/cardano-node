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
      stateDir = mkOption {
        type = types.str;
        default = "cardano-node";
        description = ''
          Directory below /var/lib to store blockchain data.
          This directory will be created automatically using systemd's StateDirectory mechanism.
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
      script = cfg.script;
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
