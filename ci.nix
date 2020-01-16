let
  commonLib = import ./lib.nix;
  getArchDefault = system: let
    table = {
      x86_64-linux = import ./. { target = "x86_64-linux"; };
      x86_64-darwin = import ./. { target = "x86_64-darwin"; };
      x86_64-windows = import ./. { target = "x86_64-windows"; };
    };
  in table.${system};
  default = getArchDefault builtins.currentSystem;
  makeScripts = cluster: let
    getScript = name: {
      x86_64-linux = (getArchDefault "x86_64-linux").scripts.${cluster}.${name};
      x86_64-darwin = (getArchDefault "x86_64-darwin").scripts.${cluster}.${name};
    };
  in {
    node = getScript "node";
  };
  # TODO: add docker images
  #wrapDockerImage = cluster: let
  #  images = (getArchDefault "x86_64-linux").dockerImages;
  #  wrapImage = image: commonLib.pkgs.runCommand "${image.name}-hydra" {} ''
  #    mkdir -pv $out/nix-support/
  #    cat <<EOF > $out/nix-support/hydra-build-products
  #    file dockerimage ${image}
  #    EOF
  #  '';
  makeRelease = cluster: {
    name = cluster;
    value = {
      scripts = makeScripts cluster;
      #dockerImage = wrapDockerImage cluster;
    };
  };
in
{ cardano-node ? { outPath = ./.; rev = "abcdef"; }, ... }@args:
(commonLib.nix-tools.release-nix {
  package-set-path = ./nix/nix-tools.nix;
  _this = cardano-node;

  # packages from our stack.yaml or plan file (via nix/pkgs.nix) we
  # are interested in building on CI via nix-tools.
  packages = [ "cardano-node" ];
  # TODO: use forEnvironments
  extraBuilds = let
    # only build nixos tests for linux
    default = getArchDefault "x86_64-linux";
  in {
    inherit (default) nixosTests;
  } // (builtins.listToAttrs (map makeRelease [
    "mainnet"
    "staging"
    "shelley_staging_short"
    "shelley_staging"
    "testnet"
  ]));

  # The set of jobs we consider crutial for each CI run.
  # if a single one of these fails, the build will be marked
  # as failed.
  #
  # The names can be looked up on hydra when in doubt.
  #
  # custom jobs will follow their name as set forth in
  # other-packages.
  #
  # nix-tools packages are prefixed with `nix-tools` and
  # follow the following naming convention:
  #
  #   namespace                      optional cross compilation prefix                 build machine
  #   .-------.                              .-----------------.                .--------------------------.
  #   nix-tools.{libs,exes,tests,benchmarks}.{x86_64-pc-mingw-,}$pkg.$component.{x86_64-linux,x86_64-darwin}
  #             '--------------------------'                    '-------------'
  #                 component type                          cabal pkg and component*
  #
  # * note that for `libs`, $component is empty, as cabal only
  #   provides a single library for packages right now.
  # * note that for `exes`, $component is also empty, because it
  #   it provides all exes under a single result directory.
  #   To  specify a single executable component to build, use
  #   `cexes` as component type.
  #
  # Example:
  #
  #   nix-tools.libs.ouroboros-consensus.x86_64-darwin -- will build the ouroboros-consensus library on and for macOS
  #   nix-tools.libs.x86_64-pc-mingw32-ouroboros-network.x86_64-linux -- will build the ouroboros-network library on linux for windows.
  #   nix-tools.tests.ouroboros-consensus.test-crypto.x86_64-linux -- will build and run the test-crypto from the
  #                                                          ouroboros-consensus package on linux.

  # The required jobs that must pass for ci not to fail:
  required-name = "cardano-node-required-checks";
  required-targets = jobs: [
    # targets are specified using above nomenclature:
    jobs.nix-tools.libs.cardano-node.x86_64-darwin
    jobs.nix-tools.libs.cardano-node.x86_64-linux
    jobs.nix-tools.exes.cardano-node.x86_64-darwin
    jobs.nix-tools.exes.cardano-node.x86_64-linux
    jobs.nix-tools.tests.cardano-node.cardano-node-test.x86_64-linux

    # windows cross compilation targets
    jobs.nix-tools.libs.x86_64-pc-mingw32-cardano-node.x86_64-linux
    jobs.nix-tools.exes.x86_64-pc-mingw32-cardano-node.x86_64-linux
    jobs.nix-tools.tests.x86_64-pc-mingw32-cardano-node.cardano-node-test.x86_64-linux

    # Chairman consensus test TODO: fix the chairman test
    jobs.nixosTests.chairmansCluster.x86_64-linux
  ];
} (builtins.removeAttrs args ["cardano-node"]))
