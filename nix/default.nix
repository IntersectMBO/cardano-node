{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
, gitrev ? null
}:
let
  sources = import ./sources.nix { inherit pkgs; }
    // sourcesOverride;
  iohkNixMain = import sources.iohk-nix {};
  haskellNix = (import sources."haskell.nix" { inherit system sourcesOverride; }).nixpkgsArgs;
  # use our own nixpkgs if it exists in our sources,
  # otherwise use iohkNix default nixpkgs.
  nixpkgs = if (sources ? nixpkgs)
    then (builtins.trace "Not using IOHK default nixpkgs (use 'niv drop nixpkgs' to use default for better sharing)"
      sources.nixpkgs)
    else (builtins.trace "Using IOHK default nixpkgs"
      iohkNixMain.nixpkgs);

  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ iohkNixMain.overlays.haskell-nix-extra
    ++ iohkNixMain.overlays.crypto
    # iohkNix: nix utilities and niv:
    ++ iohkNixMain.overlays.iohkNix
    # our own overlays:
    ++ [
      (pkgs: _: with pkgs; {
        inherit gitrev;

        # commonLib: mix pkgs.lib with iohk-nix utils and our own:
        commonLib = lib // iohkNix // iohkNix.cardanoLib
          // import ./util.nix { inherit haskell-nix; }
          # also expose our sources, nixpkgs and overlays
          // { inherit overlays sources nixpkgs; };

        svcLib = import ./svclib.nix { inherit pkgs; };
      })
      # And, of course, our haskell-nix-ified cabal project:
      (import ./pkgs.nix)
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.config // config;
  };

in pkgs
