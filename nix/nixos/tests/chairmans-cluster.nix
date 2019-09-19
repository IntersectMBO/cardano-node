{ pkgs, commonLib, chairmanScript, ... }:

let chairman-runner = chairmanScript {
      chairman-config = {
        enable     = true;
        k          = 2160;
        timeout    = 300;
        maximum-fork-length = 1;
        slots-within-tolerance = 30;
      };
    };
in {
  name = "chairmans-cluster-test";
  nodes = {
    machine = { config, pkgs, ... }: {
      imports = [
        ../.
      ];
      services.cardano-cluster = {
        enable = true;
        node-count = 3; ## This must match nixos/scripts.nix:mkChairmanScript
      };
    };
  };
  testScript = ''
    startAll
    $machine->waitForOpenPort(3001);
    $machine->waitForOpenPort(3002);
    $machine->waitForOpenPort(3003);
    $machine->succeed("${chairman-runner} 2>&1 | systemd-cat --identifier=chairman --priority=crit");
  '';

}
