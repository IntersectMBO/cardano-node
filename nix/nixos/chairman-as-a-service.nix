{ config
, lib
, ... }:

with lib;
let
  cfg  = config.services.chairman;
  ncfg = config.services.cardano-node;
in {

  options = {
    services.chairman = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable chairman, a consensus checker service.
        '';
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
