{pkgs, ...}: let
  inherit (lib) getExe;
  inherit (pkgs) cardanoNodePackages lib;
  synthAsserted = cardanoNodePackages.db-synthesizer.passthru.asserted;

  # NixosTest script fns supporting a timeout have a default of 900 seconds.
  #
  # There is no pre-existing history for chain synthesis, and default
  # cardano-testnet genesis parameters set epochs to be short and fast, so a 30
  # second global timeout should be more than sufficient.
  globalTimeout = 30;

  testDir = "testnet";
in {
  inherit globalTimeout;

  name = "cardano-node-dbtools-test";
  nodes = {
    machine = _: {
      nixpkgs.pkgs = pkgs;

      environment = {
        systemPackages = with cardanoNodePackages; [
          cardano-cli
          cardano-node
          cardano-testnet
          db-analyser
          db-synthesizer
          db-truncater
        ];

        variables = {
          CARDANO_CLI = getExe cardanoNodePackages.cardano-cli;
          CARDANO_NODE = getExe cardanoNodePackages.cardano-node;
          KES_KEY = "${testDir}/pools-keys/pool1/kes.skey";
          OPCERT = "${testDir}/pools-keys/pool1/opcert.cert";
          VRF_KEY = "${testDir}/pools-keys/pool1/vrf.skey";
        };
      };
    };
  };

  testScript = ''
    import re
    countRegex = r'Counted (\d+) blocks\.'

    start_all()
    print(machine.succeed("cardano-node --version"))
    print(machine.succeed("cardano-cli --version"))
    print(machine.succeed("cardano-testnet version"))

    # For create-env the default security parameter is 5, active slot coeff = 0.05.
    # Epoch length should be >= 2 * stability_window = 2 * 3 * k / f = 600 slots
    # Epoch length ideally should also be divisible by 10k = 500 slots
    print(machine.succeed("cardano-testnet create-env --epoch-length 1000 --output ${testDir}"))

    print(machine.succeed("echo Synthesize one epoch"))
    print(machine.succeed("db-synthesizer \
      --config ${testDir}/configuration.yaml \
      --db db \
      --shelley-operational-certificate $OPCERT \
      --shelley-vrf-key $VRF_KEY \
      --shelley-kes-key $KES_KEY \
      --epochs 1 \
      2>&1")
    )

    print(machine.succeed("echo Analyze synthesized chain"))
    out = machine.succeed("db-analyser \
      --db db \
      --count-blocks \
      --in-mem \
      --config ${testDir}/configuration.yaml \
      2>&1"
    )
    print(out)
    match = re.search(countRegex, out)
    assert match is not None, f"Could not find block count in post-synthesis output: {out}"
    blocks_before = int(match.group(1))
    print(f"Found {blocks_before} blocks post synthesis")

    assert blocks_before > 0, f"No blocks were synthesized: {blocks_before}"

    print(machine.succeed("echo Truncate synthesized chain"))
    print(machine.succeed("db-truncater \
      --db db \
      --truncate-after-block 1 \
      --verbose \
      --config ${testDir}/configuration.yaml \
      2>&1")
    )

    print(machine.succeed("echo Analyze truncated chain"))
    out = machine.succeed("db-analyser \
      --db db \
      --count-blocks \
      --in-mem \
      --config ${testDir}/configuration.yaml \
      2>&1"
    )
    print(out)
    match = re.search(countRegex, out)
    assert match is not None, f"Could not find block count in post-truncation output: {out}"
    blocks_after = int(match.group(1))
    print(f"Found {blocks_after} blocks post truncation")

    # Blocks are zero indexed, so truncation after 1 leaves block 0 and block 1 expected to remain.
    assert blocks_after == 2, f"Expected exactly 2 blocks after truncation, got {blocks_after}"
    assert blocks_before > blocks_after, f"Pre-truncation blockHeight of {blocks_before} should be larger than post-truncation blockHeight of {blocks_after}"

    # Run with GHC asserts enabled -- a non-zero exit here indicates an assertion violation
    print(machine.succeed("echo Check chain synthesis for assertion failures"))
    print(machine.succeed("${synthAsserted}/bin/db-synthesizer \
      --config ${testDir}/configuration.yaml \
      --db db-asserted \
      --shelley-operational-certificate $OPCERT \
      --shelley-vrf-key $VRF_KEY \
      --shelley-kes-key $KES_KEY \
      --epochs 1 \
      2>&1")
    )
  '';
}
