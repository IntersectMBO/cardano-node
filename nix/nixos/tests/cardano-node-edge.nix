{pkgs, ...}:
with pkgs; let
  environment = "mainnet";
in {
  name = "cardano-node-edge-test";
  nodes = {
    machine = {config, ...}: {
      nixpkgs.pkgs = pkgs;
      imports = [
        ../.
      ];

      services = {
        cardano-node = {
          inherit environment;

          enable = true;
          port = 3001;
          hostAddr = "127.0.0.1";
          topology = commonLib.mkEdgeTopologyP2P {
            edgeNodes = [
              {
                addr = "127.0.0.1";
                port = 3001;
              }
            ];
          };

          # Default tracing system logging is to stdout and default prometheus
          # metrics are exported to localhost on port 12798.
          nodeConfig = config.services.cardano-node.environments.${environment}.nodeConfig;
        };

        cardano-submit-api = {
          enable = true;
          port = 8101;
          network = environment;
          socketPath = config.services.cardano-node.socketPath 0;
        };
      };

      systemd.services = {
        cardano-node.serviceConfig.Restart = lib.mkForce "no";
        cardano-submit-api.serviceConfig.SupplementaryGroups = "cardano-node";
      };
    };
  };
  testScript = ''
    start_all()
    machine.wait_for_unit("cardano-node.service")
    machine.wait_for_file("/run/cardano-node/node.socket")

    # Re-enable once PrometheusSimple with node >= 10.3 is available
    # machine.wait_for_open_port(12798)

    machine.wait_for_open_port(3001)
    machine.succeed("systemctl status cardano-node")

    machine.succeed(
      "${cardanoNodePackages.cardano-cli}/bin/cardano-cli ping -h 127.0.0.1 -c 1 -q --json | ${jq}/bin/jq -c"
    )

    machine.wait_for_open_port(8101)
    machine.succeed("systemctl status cardano-submit-api")
  '';
}
