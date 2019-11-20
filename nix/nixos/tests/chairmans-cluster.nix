{ pkgs, commonLib, ... }:

{
  name = "chairmans-cluster-test";
  nodes = {
    machine = { lib, config, pkgs, ... }: {
      imports = [
        ../.
      ];
      services.cardano-cluster = {
        enable = true;
        node-count = 3; ## This must match nixos/scripts.nix:mkChairmanScript
      };
      services.chairman = {
        enable = true;
        k          = 2160;
        timeout    = 300;
        maxBlockNo = 30;
        topology = commonLib.mkEdgeTopology {};
      };
      systemd.services.chairman.wantedBy = lib.mkForce [];
    };
  };
  testScript = ''
    startAll
    $machine->waitForOpenPort(3001);
    $machine->waitForOpenPort(3002);
    $machine->waitForOpenPort(3003);
    $machine->succeed("netstat -pltn | systemd-cat --identifier=netstat --priority=crit");
    $machine->systemctl("start chairman.service");
    $machine->sleep(30);
    $machine->requireActiveUnit("chairman.service");

  '';

}
