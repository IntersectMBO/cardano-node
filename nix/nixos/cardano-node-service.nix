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
