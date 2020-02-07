{ pkgs, ... }:
with pkgs;
{
  name = "cardano-node-edge-test";
  nodes = {
    machine = { config, pkgs, ... }: {
      nixpkgs.overlays = pkgsOverlays;
      imports = [
        ../.
      ];
      services.cardano-node = {
        enable = true;
        port = 3001;
        inherit (commonLib.environments.staging) genesisFile genesisHash;
        topology = commonLib.mkEdgeTopology {};
      };
    };
  };
  testScript = ''
    startAll
    $machine->waitForUnit("cardano-node.service");
    $machine->waitForOpenPort(3001);
    $machine->succeed("stat /run/cardano-node");
    $machine->succeed("stat /run/cardano-node/node-core-0.socket");
  '';

}
