############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
, haskell-nix
, buildPackages
, config ? {}
# GHC attribute name
, compiler ? config.haskellNix.compiler or "ghc865"
# Enable profiling
, profiling ? config.haskellNix.profiling or false
}:
let

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    src = haskell-nix.haskellLib.cleanGit {
      name = "cardano-node";
      src = ../.;
    };
    ghc = buildPackages.haskell-nix.compiler.${compiler};
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


        # Tell hydra to skip this test on windows (it does not build)
        # 1. Set them to all ...
        packages.cardano-cli.components.all.platforms =
          with stdenv.lib.platforms; lib.mkForce [ linux darwin windows ];
        # 2. then drop windows for the test
        packages.cardano-cli.components.tests.cardano-cli-test.platforms =
          with stdenv.lib.platforms; lib.mkForce [ linux darwin ];

        # Needed for the CLI tests.
        # Coreutils because we need 'paste'.
        packages.cardano-cli.components.tests.cardano-cli-test.build-tools =
          lib.mkForce [buildPackages.bc buildPackages.jq buildPackages.coreutils buildPackages.shellcheck];
      }
      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;
        packages.cardano-config.configureFlags = [ "--ghc-option=-Werror" ];

        # cardano-cli-tests depends on cardano-cli
        packages.cardano-cli.preCheck = "export CARDANO_CLI=${pkgSet.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli";
      }
      (lib.optionalAttrs profiling {
        enableLibraryProfiling = true;
        packages.cardano-node.components.exes.cardano-node.enableExecutableProfiling = true;
        profilingDetail = "default";
      })
      (lib.optionalAttrs (stdenv.hostPlatform != stdenv.buildPlatform) {
        # Disable cabal-doctest tests by turning off custom setups
        packages.comonad.package.buildType = lib.mkForce "Simple";
        packages.distributive.package.buildType = lib.mkForce "Simple";
        packages.lens.package.buildType = lib.mkForce "Simple";
        packages.nonempty-vector.package.buildType = lib.mkForce "Simple";
        packages.semigroupoids.package.buildType = lib.mkForce "Simple";

        # Make sure we use a buildPackages version of happy
        packages.pretty-show.components.library.build-tools = [ buildPackages.haskell-nix.haskellPackages.happy ];
      })
      (lib.optionalAttrs stdenv.hostPlatform.isWindows {
        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [];
        packages.terminal-size.components.library.build-tools = lib.mkForce [];
        packages.network.components.library.build-tools = lib.mkForce [];
      })
      (lib.optionalAttrs stdenv.hostPlatform.isMusl (let
        staticLibs = [ zlib openssl libffi gmp6 ];
        gmp6 = buildPackages.gmp6.override { withStatic = true; };
        zlib = buildPackages.zlib.static;
        openssl = (buildPackages.openssl.override { static = true; }).out;
        libffi = buildPackages.libffi.overrideAttrs (oldAttrs: {
          dontDisableStatic = true;
          configureFlags = (oldAttrs.configureFlags or []) ++ [
                    "--enable-static"
                    "--disable-shared"
          ];
        });

        # Module options which adds GHC flags and libraries for a fully static build
        fullyStaticOptions = {
          enableShared = lib.mkForce false;
          enableStatic = lib.mkForce true;
          configureFlags = map (drv: "--ghc-option=-optl=-L${drv}/lib") staticLibs;
        };
      in {
        # Apply fully static options to our Haskell executables
        packages.cardano-node.components.exes.cardano-node = fullyStaticOptions;
        packages.cardano-node.components.all = fullyStaticOptions;
        packages.cardano-node.components.exes.chairman = fullyStaticOptions;
        packages.ouroboros-consensus-byron.components.exes.db-converter = fullyStaticOptions;

        # systemd can't be statically linked - disable lobemo-scribe-journal
        packages.cardano-config.flags.systemd = false;
 
        # segfaults in Setup.hs
        # packages.pretty-show.package.buildType = lib.mkForce "Simple";

        # Haddock not working and not needed for cross builds
        doHaddock = false;
      }))
    ];

    # TODO add flags to packages (like cs-ledger) so we can turn off tests that will
    # not build for windows on a per package bases (rather than using --disable-tests).
    # configureArgs = lib.optionalString stdenv.hostPlatform.isWindows "--disable-tests";
  };
in
  pkgSet
