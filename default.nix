{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
# and scripts (see ./nix/scripts.nix):
, config ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-node --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.iohkNix.commitIdFromGitRepoOrZero ./.git
}:
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages cardanoNodeHaskellPackages);

  scripts = callPackage ./nix/scripts.nix {};
  # NixOS tests run a proxy and validate it listens
  nixosTests = import ./nix/nixos/tests {
    inherit pkgs;
  };

  self = {
    inherit haskellPackages scripts nixosTests environments check-hydra;

    inherit (haskellPackages.cardano-node.identifier) version;
    # Grab the executable component of our package.
    inherit (haskellPackages.cardano-node.components.exes)
      cardano-node;

    inherit (pkgs.iohkNix) checkCabalProject;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
};
in self
