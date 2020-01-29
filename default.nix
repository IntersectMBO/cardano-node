{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, commonLib ? import ./lib.nix { inherit system crossSystem config; }
, pkgs ? commonLib.pkgs
, customConfig ? {}
, interactive ? false
, gitrev ? commonLib.iohkNix.commitIdFromGitRepoOrZero ./.git
}:

let
  lib = commonLib.pkgs.lib;
  inherit (commonLib) environments haskellPackages;
  cardano-node = haskellPackages.cardano-node.components.exes.cardano-node;

  scripts = commonLib.pkgs.callPackage ./nix/scripts.nix {
      inherit commonLib customConfig;
  };
  # NixOS tests run a proxy and validate it listens
  nixosTests = import ./nix/nixos/tests {
    inherit (commonLib) pkgs;
    inherit commonLib interactive;
  };

  self = with commonLib; {
    inherit scripts nixosTests environments cardano-node haskellPackages;
    inherit (iohkNix) check-hydra;

    # `tests` are the test suites which have been built.
    tests = collectComponents "tests" isCardanoNode haskellPackages;
    # `checks` are the result of executing the tests.
    checks = pkgs.recurseIntoAttrs (getPackageChecks (filterCardanoPackages haskellPackages));
    # `benchmarks` are only built, not run.
    benchmarks = collectComponents "benchmarks" isCardanoNode haskellPackages;

    # TODO: fix the shell
    #shell = haskellPackages.shellFor {
    #  name = "cardano-node";
    #  packages = ps: with ps; [
    #    cardano-node-cli
    #    cardano-node
    #  ];
    #  buildInputs = (with pkgs.haskell-nix.haskellPackages; [
    #      weeder.components.exes.weeder
    #      hlint.components.exes.hlint
    #    ])
    #    ++ [ pkgs.pkgconfig pkgs.sqlite-interactive
    #         pkgs.cabal-install ];
    #  meta.platforms = pkgs.lib.platforms.unix;
    #};

  };

in self
