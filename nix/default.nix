{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, customConfig ? import ./custom-config.nix
, sourcesOverride ? {}
, gitrev ? null
, workbenchConfig ? import ../workbench-config.nix
}:
let
  gitrev' = if gitrev == null
    then iohkNix.commitIdFromGitRepoOrZero ../.git
    else gitrev;
  flakeSources = let
    flakeLock = (builtins.fromJSON (builtins.readFile ../flake.lock)).nodes;
    compat = s: builtins.fetchGit {
      url = "https://github.com/${s.locked.owner}/${s.locked.repo}.git";
      inherit (s.locked) rev;
      ref = s.original.ref or "master";
    };
  in {
    "haskell.nix" = compat flakeLock.haskellNix;
    "iohk-nix" = compat flakeLock.iohkNix;
  };
  sources = flakeSources // sourcesOverride;
  haskellNix = import sources."haskell.nix" { inherit system sourcesOverride; };
  nixpkgs = haskellNix.sources.nixpkgs-unstable;
  iohkNix = import sources.iohk-nix { inherit system; };
  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.nixpkgsArgs.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ iohkNix.overlays.haskell-nix-extra
    ++ iohkNix.overlays.crypto
    # iohkNix: nix utilities:
    ++ iohkNix.overlays.iohkNix
    # our own overlays:
    ++ [
      (pkgs: _: with pkgs; {
        gitrev = gitrev';
        inherit customConfig workbenchConfig;

        inherit (iohkNix) cardanoLib;
        # commonLib: mix pkgs.lib with iohk-nix utils and our own:
        commonLib = lib // cardanoLib // iohk-nix.lib
          // import ./util.nix { inherit haskell-nix; }
          # also expose our sources, nixpkgs and overlays
          // { inherit overlays sources nixpkgs; };
      })
      # And, of course, our haskell-nix-ified cabal project:
      (import ./pkgs.nix)
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.nixpkgsArgs.config // config;
  };

in pkgs
