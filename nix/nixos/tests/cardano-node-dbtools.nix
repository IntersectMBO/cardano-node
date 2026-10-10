{pkgs, ...}: let
  inherit (lib) getExe;
  inherit (pkgs) cardanoNodePackages lib;
  synthAsserted = cardanoNodePackages.db-synthesizer.passthru.asserted;

  # `cardano-testnet create-env` deliberately leaves the genesis hashes out of
  # its configuration file, so that the genesis files it writes alongside can be
  # edited without recomputing a hash every time (see `createEnvOptions` in
  # cardano-testnet's Parsers/Run.hs). cardano-node itself is fine with that --
  # POM.hs reads every `*GenesisHash` with `.:?` -- but the consensus db tools
  # resolve the configuration through cardano-config, which makes all five of
  # them mandatory, so they reject the environment as it comes out of create-env:
  #
  #   db-synthesizer: invalid node configuration: Error parsing the cardano-node
  #   configuration (section "Protocol") in the main configuration file:
  #     Error in $: key "ByronGenesisHash" not found
  #
  # Fill them in for the tools' benefit. Byron hashes its genesis differently
  # from the later eras, which take a plain blake2b-256 of the file bytes.
  #
  # TODO: this belongs in create-env itself -- it already computes exactly these
  # five for `cardano-testnet cardano` (Testnet.Components.Configuration's
  # `createConfigJson`), it just does not offer them here. Either give create-env
  # an opt-in flag for them, or make the db tools as lenient as the node is.
  addGenesisHashes = pkgs.writeShellApplication {
    name = "add-genesis-hashes";
    runtimeInputs = [cardanoNodePackages.cardano-cli pkgs.jq];
    text = ''
      if test $# -ne 1
      then echo "usage: add-genesis-hashes ENV-DIR" >&2; exit 1
      fi
      cd "$1"

      # create-env names the file .yaml, but writes JSON into it, so jq can edit
      # it in place. Should that ever change, this test fails loudly rather than
      # silently skipping the hashes.
      jq  --arg byron    "$(cardano-cli byron genesis print-genesis-hash \
                              --genesis-json byron-genesis.json)"        \
          --arg shelley  "$(cardano-cli hash genesis-file               \
                              --genesis shelley-genesis.json)"           \
          --arg alonzo   "$(cardano-cli hash genesis-file               \
                              --genesis alonzo-genesis.json)"            \
          --arg conway   "$(cardano-cli hash genesis-file               \
                              --genesis conway-genesis.json)"            \
          --arg dijkstra "$(cardano-cli hash genesis-file               \
                              --genesis dijkstra-genesis.json)"          \
          '. + { ByronGenesisHash:    $byron
               , ShelleyGenesisHash:  $shelley
               , AlonzoGenesisHash:   $alonzo
               , ConwayGenesisHash:   $conway
               , DijkstraGenesisHash: $dijkstra
               }'                                                       \
          configuration.yaml > configuration.yaml.hashed
      mv configuration.yaml.hashed configuration.yaml
    '';
  };

  # NixosTest script fns supporting a timeout have a default of 900 seconds.
  #
  # There is no pre-existing history for chain synthesis, and default
  # cardano-testnet genesis parameters set epochs to be short and fast, so a 45
  # second global timeout should be more than sufficient.
  globalTimeout = 45;

  testDir = "testnet";
in {
  inherit globalTimeout;

  name = "cardano-node-dbtools-test";
  nodes = {
    machine = _: {
      nixpkgs.pkgs = pkgs;

      environment = {
        systemPackages = [addGenesisHashes]
          ++ (with cardanoNodePackages; [
            cardano-cli
            cardano-node
            cardano-testnet
            db-analyser
            db-synthesizer
            db-truncater
          ]);

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

    # The db tools, unlike the node, require every genesis hash to be present.
    print(machine.succeed("echo Add genesis hashes to the node configuration"))
    print(machine.succeed("add-genesis-hashes ${testDir}"))

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
    # db-truncater only looks for db/leios.{vol,imm}.db when `--leios` is passed
    # (ouroboros-consensus 5.1.0.1 made that opt-in); the synthesized chain has
    # no LeiosDb beside it, so the flag is deliberately omitted here.
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
