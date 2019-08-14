let
  lib = (import ./lib.nix).pkgs.lib;
  commitIdFromGitRepo = import ./nix/commit-id.nix { inherit lib; };
in { customConfig ? {}
, target ? builtins.currentSystem
, gitrev ? commitIdFromGitRepo ./.git
}:
#
# The default.nix file. This will generate targets for all
# buildables (see release.nix for nomenclature, excluding
# the "build machine" last part, specific to release.nix), eg.:
#
# Generated targets include anything from stack.yaml
# (via nix-tools:stack-to-nix and the nix/regenerate.sh script)
# or cabal.project (via nix-tools:plan-to-nix), including all
# version overrides specified there.
#
# Nix-tools stack-to-nix will generate the `nix/.stack-pkgs.nix`
# file which is imported from the `nix/pkgs.nix` where further
# customizations outside of the ones in stack.yaml/cabal.project
# can be specified as needed for nix/ci.
#
# Please run `nix/regenerate.sh` after modifying stack.yaml
# or relevant part of cabal configuration files.
# When switching to recent stackage or hackage package version,
# you might also need to update the iohk-nix common lib. You
# can do so by running the `nix/update-iohk-nix.sh` script.
#
# More information about iohk-nix and nix-tools is available at:
# https://github.com/input-output-hk/iohk-nix/blob/master/docs/nix-toolification.org#for-a-stackage-project
#


# We will need to import the iohk-nix common lib, which includes
# the nix-tools tooling.
let
  system = if target != "x86_64-windows" then target else builtins.currentSystem;
  crossSystem = if target == "x86_64-windows" then lib.systems.examples.mingwW64 else null;
  commonLib = import ./lib.nix;
  nixTools = import ./nix/nix-tools.nix {};
  # TODO: move environemnts and genesis files from SL somewhere else (network maybe?)
  oldCardanoRev = (builtins.fromJSON (builtins.readFile ./nix/old-cardano-sl-src.json)).rev;
  oldCardanoSrc = import ./nix/old-cardano.nix {
    inherit commonLib;
  };
  inherit (commonLib) environments;
  scripts = import ./nix/scripts.nix {
    inherit commonLib nixTools customConfig;
  };
  # NixOS tests run a proxy and validate it listens
  #nixosTests = import ./nix/nixos/tests { inherit (commonLib) pkgs; };

in {
  inherit (nixTools) nix-tools;
  inherit (commonLib.iohkNix) check-nix-tools check-hydra;
  inherit scripts environments;
}
