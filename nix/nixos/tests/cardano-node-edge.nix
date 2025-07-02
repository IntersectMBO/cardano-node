{pkgs, ...}:
with pkgs; let
  environment = "mainnet";

  # NixosTest script fns supporting a timeout have a default of 900 seconds.
  timeout = toString 30;
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

  # Only newer nixpkgs have have timeout args for all wait_for_.* fns.
  # Use the generic wait_until_succeeds w/ timeout arg until nixpkgs is bumped.
  testScript = ''
    start_all()
    machine.wait_for_unit("cardano-node.service", timeout=${timeout})
    machine.wait_until_succeeds("[ -S /run/cardano-node/node.socket ]", timeout=${timeout})
    machine.wait_until_succeeds("nc -z localhost 12798", timeout=${timeout})
    machine.wait_until_succeeds("nc -z localhost 3001", timeout=${timeout})
    machine.succeed("systemctl status cardano-node")
    out = machine.succeed(
      "${cardanoNodePackages.cardano-cli}/bin/cardano-cli ping -h 127.0.0.1 -c 1 -q --json | ${jq}/bin/jq -c"
    )
    print("ping:", out)
    machine.wait_until_succeeds("nc -z localhost 8101", timeout=${timeout})
    machine.succeed("systemctl status cardano-submit-api")
  '';
}
