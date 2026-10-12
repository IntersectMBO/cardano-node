{pkgs, ...}:
with pkgs; let
  environment = "preview";

  # NixosTest script fns supporting a timeout have a default of 900 seconds.
  timeout = toString 60;

  lsmPath = "/var/lib/cardano-node/lsm";

  # Distinctive and unset by the service, so finding it in the resolved
  # configuration can only mean the envelope-shaped key below was honoured.
  deadlineConcurrency = 7;
in {
  name = "cardano-node-enveloped-test";
  nodes = {
    machine = {config, ...}: {
      nixpkgs.pkgs = pkgs;
      imports = [
        ../.
      ];

      services.cardano-node = {
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
        nodeConfig = config.services.cardano-node.environments.${environment}.nodeConfig;

        # What this test exists for.
        configFormat = "enveloped";

        # The service writes `Backend = "V2LSM"` with a sibling
        # `LSMDatabasePath`, which is the legacy flat spelling of the tagged
        # `Backend.LSM` form.  Nothing but a host produces it, so nothing else
        # exercises the fold.
        withUtxoHdLsmt = true;
        lsmDatabasePath = lsmPath;

        # A caller writing part of its configuration in envelope shape, which is
        # where extraNodeConfig is headed once POM is dropped.  Section
        # qualified, so it reaches mkEnvelope as a `Configuration` key rather
        # than a flat one.
        extraNodeConfig.Configuration.Network.MaxConcurrencyDeadline = deadlineConcurrency;
      };

      systemd.services.cardano-node.serviceConfig.Restart = lib.mkForce "no";
    };
  };

  # Sandboxed like the other tests here: the node cannot sync, but it starts and
  # resolves its configuration, which is all these assertions need.
  testScript = ''
    start_all()

    machine.wait_for_unit("cardano-node.service", timeout=${timeout})
    machine.wait_until_succeeds("[ -S /run/cardano-node/node.socket ]", timeout=${timeout})
    machine.succeed("systemctl status cardano-node")

    journal = "journalctl -u cardano-node --no-pager"

    # Positive control: prove the node took the cardano-config path. Without
    # this every assertion below would also pass on a legacy configuration,
    # which is not what is under test.
    machine.succeed(
      journal + " | grep -qF 'own parser was not run'"
    )

    # The check this test exists for. cardano-config emits this whenever
    # migrate would still change the document, so a generated envelope that is
    # not canonical says so here and nowhere else. The node runs either way,
    # which is why it went unnoticed until a host was deployed.
    machine.fail(
      journal + " | grep -qF 'not in the current canonical format'"
    )

    # A key cardano-config does not resolve is kept outside every section and
    # ignored, silently changing behaviour to a default.
    machine.fail(
      journal + " | grep -qF 'unrecognised configuration key(s)'"
    )

    # The flat V2LSM pair survived enveloping as a setting, not just as a shape:
    # an unfolded backend resolves to the in-memory default with no path at all.
    machine.succeed(
      journal + " | grep -qF 'V2LSM (Just \"${lsmPath}\")'"
    )

    # The envelope-shaped extraNodeConfig key reached the resolved
    # configuration. A dropped `Configuration` block leaves this at its default.
    machine.succeed(
      journal + " | grep -qF 'ncMaxConcurrencyDeadline = Just ${toString deadlineConcurrency}'"
    )

    # Tier 3, the same question asked of the file directly rather than of the
    # startup log. On an envelope `resolve` has no second parser to cross-check
    # against, so it prints the resolved configuration and anything on stderr is
    # a complaint about the file.
    cfg = machine.succeed(
      journal + " | grep -om1 '/nix/store/[^\"]*config-0-0.json'"
    ).strip()
    print("resolving:", cfg)
    machine.succeed(
      "${cardanoNodePackages.cardano-node}/bin/cardano-node config resolve --config "
      + cfg
      + " 2>/tmp/resolve.stderr >/tmp/resolve.stdout"
    )
    machine.succeed("[ -s /tmp/resolve.stdout ]")
    stderr = machine.succeed("cat /tmp/resolve.stderr")
    if stderr.strip():
        raise Exception("resolve reported on the generated config:\n" + stderr)
  '';
}
