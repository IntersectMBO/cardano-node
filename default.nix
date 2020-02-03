{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
, profiling ? false
, commonLib ? import ./lib.nix { inherit system crossSystem config profiling; }
, pkgs ? commonLib.pkgs
, customConfig ? {}
, interactive ? false
, gitrev ? commonLib.iohkNix.commitIdFromGitRepoOrZero ./.git
, withHoogle ? true
}:

let
  lib = commonLib.pkgs.lib;
  inherit (commonLib) environments haskellPackages niv;
  cardano-node = haskellPackages.cardano-node.components.exes.cardano-node;

  scripts = commonLib.pkgs.callPackage ./nix/scripts.nix {
      inherit commonLib customConfig;
  };
  # NixOS tests run a proxy and validate it listens
  nixosTests = import ./nix/nixos/tests {
    inherit (commonLib) pkgs;
    inherit commonLib interactive;
  };

  # we are only intersted in listing the project packages
  projectHaskellPackages = pkgs.haskell-nix.haskellLib.selectProjectPackages haskellPackages;

  self = with commonLib; {
    inherit scripts nixosTests environments cardano-node;

    allHaskellPackages = haskellPackages;
    haskellPackages = projectHaskellPackages;
    inherit (iohkNix) check-hydra;

    # `tests` are the test suites which have been built.
    tests = collectComponents "tests" isCardanoNode haskellPackages;
    # `checks` are the result of executing the tests.
    checks = pkgs.recurseIntoAttrs (getPackageChecks (filterCardanoPackages haskellPackages));
    # `benchmarks` are only built, not run.
    benchmarks = collectComponents "benchmarks" isCardanoNode haskellPackages;

    shell = haskellPackages.shellFor {

      packages = ps: with ps; [
        ps.cardano-node
        ps.cardano-config
        ps.cardano-sl-x509
        ps.ekg-prometheus-adapter
        ps.ouroboros-consensus
        ps.ouroboros-network
      ];

      # Builds a Hoogle documentation index of all dependencies,
      # and provides a "hoogle" command to search the index.
      inherit withHoogle;

      # You might want some extra tools in the shell (optional).
      buildInputs = (with pkgs; [
        cabal-install
        ghcid
        hlint
        pkgs.haskellPackages.weeder
        nix
        niv
        pkgconfig
        sqlite-interactive
        tmux
        git
        iohkNix.stack-hpc-coveralls
        openssl
        libffi
        gmp
        zlib
        systemd
        pkgs.haskellPackages.happy
      ]);

      # Prevents cabal from choosing alternate plans, so that
      # *all* dependencies are provided by Nix.
      exactDeps = true;

      # https://github.com/commercialhaskell/stack/issues/5008
      STACK_IN_NIX_SHELL = true;
    };

  };

in self
