{
  pkgs,
  ciJobs,
  supportedSystems ? ["x86_64-linux"],
}: let
  inherit (ciJobs.musl) cardano-node-linux;
  inherit (pkgs.lib) genAttrs hydraJob;

  forAllSystems = genAttrs supportedSystems;

  importTest = fn: args: system: let
    imported = import fn;
    test = import (pkgs.path + "/nixos/tests/make-test-python.nix") imported;
  in
    test ({
        inherit pkgs system;
        inherit (pkgs.commonLib) config;
      }
      // args);

  # These tests are sandboxed and don't transmit data across the network for
  # syncing but can still ensure services start and listen as expected.
  callTest = fn: args: forAllSystems (system: hydraJob (importTest fn args system));
in {
  # Tests the linux release binary envs with pre-bundled config.
  cardanoNodeArtifact = callTest ./cardano-node-artifact.nix {inherit cardano-node-linux;};

  # Tests db-tools (synthesizer, analyser, truncater) against a cardano-testnet create-env environment.
  cardanoNodeDbtools = callTest ./cardano-node-dbtools.nix {};

  # Tests a mainnet edge node with submit-api using nixos service config.
  cardanoNodeEdge = callTest ./cardano-node-edge.nix {};

  # Tests that a host-assembled enveloped config is canonical: the service's
  # own LedgerDB keys and an envelope-shaped extraNodeConfig both survive
  # enveloping, and cardano-config has nothing to migrate or ignore. Nothing
  # else checks a config as a host writes it; the iohk-nix lint and drift jobs
  # only see environments.
  cardanoNodeEnveloped = callTest ./cardano-node-enveloped.nix {};

  # Tests the OCI images (node, tracer, submit-api), mostly under a read-only
  # root, a non-root UID in group 0 and a private /tmp. One subtest per
  # concern: startup and the runtime dir, merged-config path rewriting, the
  # /tmp lock and fail-fast, cli mode dispatch, operator RTS flags, submit-api
  # startup to a metrics scrape, and graceful shutdown on SIGTERM and SIGINT
  # for both ways into the image.
  cardanoOci = callTest ./cardano-oci.nix {
    dockerImage = pkgs.dockerImage;
    tracerDockerImage = pkgs.tracerDockerImage;
    submitApiDockerImage = pkgs.submitApiDockerImage;
  };
}
