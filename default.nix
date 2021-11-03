let defaultCustomConfig = import ./nix/custom-config.nix defaultCustomConfig;
# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
in
{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, withHoogle ? defaultCustomConfig.withHoogle
, profileName ? defaultCustomConfig.localCluster.profileName
, autoStartCluster ? defaultCustomConfig.localCluster.autoStartCluster
, workbenchDevMode ? defaultCustomConfig.localCluster.workbenchDevMode
, customConfig ? {
    inherit withHoogle;
    localCluster =  {
      inherit autoStartCluster profileName workbenchDevMode;
    };
  }
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-node --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix {
    inherit system crossSystem config customConfig sourcesOverride gitrev;
  }
# Git sha1 hash, to be passed when not building from a git work tree.
, gitrev ? null
}:
with pkgs; with commonLib;
let
  inherit (pkgs) customConfig;
  inherit (customConfig) localCluster;

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages cardanoNodeHaskellPackages);


  shell = import ./shell.nix {
    inherit pkgs;
    withHoogle = true;
  };

  packages = {
    inherit haskellPackages shell
      cardano-node cardano-node-profiled cardano-node-eventlogged
      cardano-cli db-converter cardano-ping
      locli locli-profiled
      tx-generator tx-generator-profiled
      scripts environments dockerImage submitApiDockerImage bech32;

    clusterCabal = mkSupervisordCluster { inherit profileName; useCabalRun = true; };
    clusterNix   = mkSupervisordCluster { inherit profileName; useCabalRun = false; };

    devopsShell = shell.devops;

    devShell = shell.dev;

    nixosTests = recRecurseIntoAttrs nixosTests;

    # so that eval time gc roots are cached (nix-tools stuff)
    inherit (cardanoNodeProject) roots plan-nix;

    inherit (haskellPackages.cardano-node.identifier) version;

    exes = collectComponents' "exes" haskellPackages;

    inherit plutus-scripts;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;

      hlint = callPackage hlintCheck {
        inherit (cardanoNodeProject) src;
      };
    };

};
in packages
