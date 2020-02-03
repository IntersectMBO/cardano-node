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
  projectHaskellPackages = commonLib.selectProjectPackages haskellPackages;

  self = with commonLib; {
    inherit scripts nixosTests environments cardano-node;

    haskellPackages = projectHaskellPackages;
    inherit (iohkNix) check-hydra;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" projectHaskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" projectHaskellPackages;
    # `checks` collect results of executing the benchmarks and tests:
    checks = {
      benchmarks = collectChecks self.benchmarks;
      tests = collectChecks self.tests;
    } // { recurseForDerivations = true; };

    shell = haskellPackages.shellFor {

      packages = ps: with ps; [
        ps.cardano-node
        ps.cardano-config
        # in theory we should only have the above two packages (or better, they should be auto-detected),
        # but due to source-repository-package declarations being considered as local packages by cabal, we need the following packages as well.
        # cf. https://github.com/haskell/cabal/issues/6249 and https://github.com/haskell/cabal/issues/5444
        ps.cardano-sl-x509
        ps.ekg-prometheus-adapter
        ps.ouroboros-consensus
        ps.ouroboros-network
      ];

      # Builds a Hoogle documentation index of all dependencies,
      # and provides a "hoogle" command to search the index.
      inherit withHoogle;

      # You might want some extra tools in the shell (optional).
      buildInputs = with pkgs; [
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
      ];

      # Prevents cabal from choosing alternate plans, so that
      # *all* dependencies are provided by Nix.
      exactDeps = true;
    };

  };

in self
