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

      # `cardano-cli ping` builds a DNS resolver seed from /etc/resolv.conf
      # before it looks at its address argument, so it aborts with
      # `BadConfiguration` in a sandboxed VM with no nameserver even when the
      # address is a literal IP.  A nameserver only has to be declared, not
      # reachable, since a literal IP is never resolved.
      networking.nameservers = ["127.0.0.1"];

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
          tracerSocketPathConnect = i: "/run/cardano-tracer/tracer.socket";

          # Default tracing system logging is to stdout and default prometheus
          # metrics are exported to localhost on port 12798.
          nodeConfig = config.services.cardano-node.environments.${environment}.nodeConfig;
        };

        cardano-submit-api = {
          enable = true;
          port = 8101;
          # Deliberately not submit-api's CLI default of 8081 so the assertion
          # below proves metricsPort is actually plumbed through.
          metricsPort = 8102;
          network = environment;
          socketPath = config.services.cardano-node.socketPath 0;
        };

        cardano-tracer = {
          inherit environment;

          enable = true;

          # Default tracing system logging from nixos tracing service is
          # journal mode and default prometheus metrics are exported to
          # localhost on port 12808.
          #
          # Switch to file mode for non-zero file size logging check.
          logging = [
            {
              logRoot = config.services.cardano-tracer.stateDir;
              logMode = "FileMode";
              logFormat = "ForHuman";
            }
          ];
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

    # Cardano-node tests:
    machine.wait_for_unit("cardano-node.service", timeout=${timeout})
    machine.wait_until_succeeds("[ -S /run/cardano-node/node.socket ]", timeout=${timeout})
    machine.wait_until_succeeds("nc -z localhost 12798", timeout=${timeout})
    machine.wait_until_succeeds("nc -z localhost 3001", timeout=${timeout})
    machine.succeed("systemctl status cardano-node")
    # Cardano-cli 11.1 takes the target as a positional host:port; --host and
    # --port are gone and -h is now --help, which exits 0 after printing usage.
    # Pipefail keeps a cli failure from being masked by jq, and selecting
    # .sample fails the assertion on empty output or a changed json shape.
    out = machine.succeed(
      "set -o pipefail; ${cardanoNodePackages.cardano-cli}/bin/cardano-cli ping -c 1 -q --json 127.0.0.1:3001 | ${jq}/bin/jq -ec .sample"
    )
    print("ping:", out)

    # Cardano-submit-api tests:
    machine.wait_for_unit("cardano-submit-api.service", timeout=${timeout})
    machine.wait_until_succeeds("nc -z localhost 8101", timeout=${timeout})
    machine.succeed("systemctl status cardano-submit-api")

    # Assert the default tracing config keeps the EKG backend enabled and that
    # the metrics prefix is applied.
    machine.wait_until_succeeds(
      "${curl}/bin/curl -sf http://localhost:8102/ | grep -qF cardano_submit_api_metrics_tx_submit_counter",
      timeout=${timeout}
    )

    # Assert Info severity traces still reach the journal.
    machine.succeed(
      "journalctl -u cardano-submit-api --no-pager | grep -qF TxSubmitApi.Endpoint.ListeningOnPort"
    )

    # Cardano-tracer tests:
    machine.wait_for_unit("cardano-tracer.service", timeout=${timeout})
    machine.wait_until_succeeds("[ -S /run/cardano-tracer/tracer.socket ]", timeout=${timeout})
    machine.wait_until_succeeds("nc -z localhost 12808", timeout=${timeout})
    machine.succeed("systemctl status cardano-tracer")
    machine.succeed("[ -s /var/lib/cardano-tracer/machine_3001/node.log ]")
  '';
}
