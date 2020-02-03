{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, overlays ? []
, sourcesOverride ? {}
, profiling ? false
}:

let
  sources = (import ./nix/sources.nix) // sourcesOverride;
  iohkNix = import sources.iohk-nix {
    inherit config system crossSystem;
    # will override iohk-nix default haskell.nix or custom_nixpkgs if needed:
    sourcesOverride = builtins.removeAttrs sources [ "nixpkgs" ];
    nixpkgsOverlays = overlays;
  };
  pkgs = iohkNix.pkgs;
  haskellPackages = import ./nix/pkgs.nix {
    inherit pkgs profiling;
    src = ./.;
  };
  # TODO: add haskellPackages and fix svclib
  svcLib = import ./nix/svclib.nix { inherit pkgs; cardano-node-packages = haskellPackages.cardano-node.components.exes; };
  lib = pkgs.lib;
  isCardanoNode = with lib; package:
    (package.isHaskell or false) &&
      ((hasPrefix "cardano-node" package.identifier.name) ||
       (elem package.identifier.name [ "text-class" "bech32" ]));
  filterCardanoPackages = pkgs.lib.filterAttrs (_: package: isCardanoNode package);
  getPackageChecks = pkgs.lib.mapAttrs (_: package: package.checks);
in lib // iohkNix.cardanoLib // {
  inherit (pkgs.haskell-nix.haskellLib) collectComponents;
  inherit (iohkNix) niv;
  inherit
    sources
    haskellPackages
    pkgs
    iohkNix
    svcLib
    isCardanoNode
    getPackageChecks
    filterCardanoPackages;
}
