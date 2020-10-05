{ pkgs, ... }:
with pkgs;
{
  name = "cardano-node-edge-test";
  nodes = {
    machine = { config, ... }: {
      nixpkgs.pkgs = pkgs;
      imports = [
        ../.
      ];
      services.cardano-node = {
        enable = true;
        systemdSocketActivation = true;
        port = 3001;
        hostAddr = "127.0.0.1";
        environment = "mainnet";
        cardanoNodePkgs = pkgs;
        nodeConfig = config.services.cardano-node.environments.${config.services.cardano-node.environment}.nodeConfig // {
          hasPrometheus = [ config.services.cardano-node.hostAddr 12798 ];
          # Use Journald output:
          setupScribes = [{
            scKind = "JournalSK";
            scName = "cardano";
            scFormat = "ScText";
          }];
          defaultScribes = [
            [
              "JournalSK"
              "cardano"
            ]
          ];
        };
      };
      systemd.services.cardano-node.serviceConfig.Restart = lib.mkForce "no";
    };
  };
  testScript = ''
    start_all()
    machine.wait_for_unit("cardano-node.service")
    machine.sleep(3)
    machine.succeed("systemctl status cardano-node")
    machine.succeed("stat /run/cardano-node")
    machine.succeed("stat /run/cardano-node/node.socket")
    machine.sleep(1)
    machine.succeed("nc -z 127.0.0.1 12798")
    machine.sleep(1)
    machine.succeed("nc -z 127.0.0.1 3001")
  '';

}
