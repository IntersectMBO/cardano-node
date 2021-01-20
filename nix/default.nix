{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
  # use our own nixpkgs if it exists in our sources,
  # otherwise use iohkNix default nixpkgs.
, nixpkgs ? if (sources ? nixpkgs)
    then (builtins.trace "Not using IOHK default nixpkgs (use 'niv drop nixpkgs' to use default for better sharing)"
      sources.nixpkgs)
    else iohkNix.nixpkgs
, sourcesOverride ? {}
, sources ? import ./sources.nix { pkgs = import nixpkgs { inherit system; }; } // sourcesOverride
, gitrev ? null
, iohkNix ? import sources.iohk-nix { inherit system; }
, haskellNix ? (import sources."haskell.nix" { inherit system sourcesOverride; }).nixpkgsArgs
}:
let
  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ iohkNix.overlays.haskell-nix-extra
    ++ iohkNix.overlays.crypto
    # iohkNix: nix utilities and niv:
    ++ iohkNix.overlays.iohkNix
    # our own overlays:
    ++ [
      (pkgs: _: with pkgs; {
        inherit gitrev;

        # commonLib: mix pkgs.lib with iohk-nix utils and our own:
        commonLib = lib // pkgs.iohkNix // pkgs.iohkNix.cardanoLib
          // import ./util.nix { inherit haskell-nix; }
          # also expose our sources, nixpkgs and overlays
          // { inherit overlays sources nixpkgs; };
      })
      # And, of course, our haskell-nix-ified cabal project:
      (import ./pkgs.nix)
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.config // config;
  };

in pkgs
