{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, customConfig ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-node --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix/default.nix { inherit system crossSystem config sourcesOverride gitrev; }
# Git sha1 hash, to be passed when not building from a git work tree.
, gitrev ? null
}:
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages cardanoNodeHaskellPackages);

  scripts = callPackage ./nix/scripts.nix { inherit customConfig; };
  # NixOS tests run a proxy and validate it listens
  nixosTests = import ./nix/nixos/tests {
    inherit pkgs;
  };

  dockerImage = let
    defaultConfig = rec {
      stateDir = "/data";
      dbPrefix = "db";
      socketPath = "/ipc/node.socket";
    };
    customConfig' = defaultConfig // customConfig;
  in pkgs.callPackage ./nix/docker.nix {
    inherit (self) cardano-node;
    scripts = callPackage ./nix/scripts.nix { customConfig = customConfig'; };
  };

  rewrite-static = _: p: if (pkgs.stdenv.hostPlatform.isDarwin) then
    pkgs.runCommandCC p.name {
      nativeBuildInputs = [ pkgs.haskellBuildUtils.package pkgs.buildPackages.binutils pkgs.buildPackages.nix ];
    } ''
      cp -R ${p} $out
      chmod -R +w $out
      rewrite-libs $out/bin $out/bin/*
    '' else if (pkgs.stdenv.hostPlatform.isMusl) then
    pkgs.runCommandCC p.name { } ''
      cp -R ${p} $out
      chmod -R +w $out
      $STRIP $out/bin/*
    '' else p;

  self = {
    inherit haskellPackages scripts nixosTests environments dockerImage;

    inherit (haskellPackages.cardano-node.identifier) version;
    # Grab the executable component of our package.
    inherit (haskellPackages.cardano-node.components.exes) cardano-node;
    cardano-node-profiled = cardanoNodeProfiledHaskellPackages.cardano-node.components.exes.cardano-node;
    inherit (haskellPackages.cardano-cli.components.exes) cardano-cli;
    inherit (haskellPackages.cardano-node.components.exes) chairman;
    # expose the db-converter from the ouroboros-network we depend on
    inherit (cardanoNodeHaskellPackages.ouroboros-consensus-byron.components.exes) db-converter;

    inherit (pkgs.iohkNix) checkCabalProject;

    exes = mapAttrsRecursiveCond (as: !(isDerivation as)) rewrite-static (collectComponents' "exes" haskellPackages);

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
