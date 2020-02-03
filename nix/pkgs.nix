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

  pkg-set = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { inherit src; };
    ghc = pkgs.buildPackages.haskell-nix.compiler.${haskellCompiler};
    modules = [

      # Allow reinstallation of Win32
      { nonReinstallablePkgs =
        [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
          "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
          # ghcjs custom packages
          "ghcjs-prim" "ghcjs-th"
          "ghc-boot"
          "ghc" "array" "binary" "bytestring" "containers"
          "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
          # "ghci" "haskeline"
          "hpc"
          "mtl" "parsec" "text" "transformers"
          "xhtml"
          # "stm" "terminfo"
        ];
      }
      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;
        packages.cardano-node.configureFlags = [ "--ghc-option=-Werror" ];
        packages.cardano-config.configureFlags = [ "--ghc-option=-Werror" ];
        enableLibraryProfiling = profiling;

        # some packages are missing identifier.name:
        packages.Win32.package.identifier.name = "Win32";
        packages.file-embed-lzma.package.identifier.name = "file-embed-lzma";
        packages.singletons.package.identifier.name = "singletons";
      }
      (pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isWindows {
        # Disable cabal-doctest tests by turning off custom setups
        packages.comonad.package.buildType = pkgs.lib.mkForce "Simple";
        packages.distributive.package.buildType = pkgs.lib.mkForce "Simple";
        packages.lens.package.buildType = pkgs.lib.mkForce "Simple";
        packages.nonempty-vector.package.buildType = pkgs.lib.mkForce "Simple";
        packages.semigroupoids.package.buildType = pkgs.lib.mkForce "Simple";

        # Make sure we use a buildPackages version of happy
        packages.pretty-show.components.library.build-tools = [ pkgs.buildPackages.haskell-nix.haskellPackages.happy ];

        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = pkgs.lib.mkForce [];
        packages.terminal-size.components.library.build-tools = pkgs.lib.mkForce [];
        packages.network.components.library.build-tools = pkgs.lib.mkForce [];
      })
    ];
    # TODO add flags to packages (like cs-ledger) so we can turn off tests that will
    # not build for windows on a per package bases (rather than using --disable-tests).
    configureArgs = pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isWindows "--disable-tests";
  };
in pkg-set
