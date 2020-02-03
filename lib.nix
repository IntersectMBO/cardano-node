{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, overlays ? []
, sourcesOverride ? {}
, profiling ? false
}:

let
  # use default stable nixpkgs from iohk-nix instead of our own:
  sources = removeAttrs (import ./nix/sources.nix) [ "nixpkgs" ] // sourcesOverride;
  iohkNix = import sources.iohk-nix {
    inherit config system crossSystem;
    sourcesOverride = sources;
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

  collectChecks = lib.mapAttrsRecursiveCond (p: !(lib.isDerivation p))
    (_: p: if (lib.isAttrs p) then pkgs.haskell-nix.haskellLib.check p else p);

  collectComponents' = group: pkgs.haskell-nix.haskellLib.collectComponents group (_:true);

in lib // iohkNix.cardanoLib // {
  inherit (pkgs.haskell-nix.haskellLib) collectComponents selectProjectPackages;
  inherit (iohkNix) niv;
  inherit
    sources
    haskellPackages
    pkgs
    iohkNix
    svcLib
    collectChecks
    collectComponents';
}
