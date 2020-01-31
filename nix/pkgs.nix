{ pkgs
, src
, haskellCompiler ? "ghc865"
, profiling ? false
}:
let

  haskell = pkgs.haskell-nix;

  # TODO: move to iohk-nix
  # Chop out a subdirectory of the source, so that the package is only
  # rebuilt when something in the subdirectory changes.
  filterSubDir = dir:  with pkgs.lib; let
      isFiltered = src ? _isLibCleanSourceWith;
      origSrc = if isFiltered then src.origSrc else src;
    in cleanSourceWith {
      inherit src;
      filter = path: type:
        type == "directory" ||
        hasPrefix (toString origSrc + toString dir) path;
    } + dir;

  recRecurseIntoAttrs = with pkgs; pred: x: if pred x then recurseIntoAttrs (lib.mapAttrs (n: v: if n == "buildPackages" then v else recRecurseIntoAttrs pred v) x) else x;
  pkgSet = recRecurseIntoAttrs (x: with pkgs; lib.isAttrs x && !lib.isDerivation x)
    # we are only intersted in listing the project packages
    (pkgs.lib.filterAttrs (with pkgs.haskell-nix.haskellLib; (n: p: p != null && (isLocalPackage p && isProjectPackage p) || n == "shellFor"))
      # from our project which is based on a cabal project.
      (pkgs.haskell-nix.cabalProject {
          src = pkgs.haskell-nix.haskellLib.cleanGit { inherit src; };
          ghc = pkgs.haskell-nix.compiler.${haskellCompiler};
          modules = [

             {
              # Packages we wish to ignore version bounds of.
              # This is similar to jailbreakCabal, however it
              # does not require any messing with cabal files.
              doCheck = false;
              packages.katip.doExactConfig = true;
              packages.ekg.components.library.enableSeparateDataOutput = true;
              packages.cardano-node.configureFlags = [ "--ghc-option=-Werror" ];
              packages.cardano-node.doCheck = true;
              packages.cardano-config.configureFlags = [ "--ghc-option=-Werror" ];
              packages.cardano-config.doCheck = true;
              enableLibraryProfiling = profiling;
            }
          ];
      }));
 in pkgSet
