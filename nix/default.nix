{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
}:
let
  # use default stable nixpkgs from iohk-nix instead of our own:
  sources = removeAttrs (import ./sources.nix) [ "nixpkgs" ]
    // sourcesOverride;

  # for inclusion in pkgs:
  nixpkgsOverlays = [
    (pkgs: _: with pkgs; {

      # mix of pkgs.lib with iohk-nix utils and our own:
      commonLib = lib // iohkNix // iohkNix.cardanoLib //
        import ./util.nix { inherit haskell-nix; };

      svcLib = import ./svclib.nix { inherit pkgs; };
    })
    # Our haskell-nix-ified cabal project:
    (import ./pkgs.nix)
  ];

  # IOHK pkgs that include haskell-nix overlays, using our sources as override:
in (import sources.iohk-nix {
    inherit system crossSystem config nixpkgsOverlays;
    sourcesOverride = sources;
  }).pkgs
