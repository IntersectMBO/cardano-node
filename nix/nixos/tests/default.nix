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

  # Tests a mainnet edge node with submit-api using nixos service config.
  cardanoNodeEdge = callTest ./cardano-node-edge.nix {};
}
