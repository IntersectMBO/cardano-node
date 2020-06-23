############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ pkgs
, lib
, stdenv
, haskell-nix
, buildPackages
, config ? {}
# GHC attribute name
, compiler ? config.haskellNix.compiler or "ghc865"
# Enable profiling
, profiling ? config.haskellNix.profiling or false
# Version info, to be passed when not building from a git work tree
, gitrev ? null
, libsodium ? pkgs.libsodium
}:
let

  src = haskell-nix.haskellLib.cleanGit {
      name = "cardano-node-src";
      src = ../.;
  };

  projectPackages = lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    (haskell-nix.cabalProject { inherit src; }));

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject  (lib.optionalAttrs stdenv.hostPlatform.isWindows {
    # FIXME: without this deprecated attribute, db-converter fails to compile directory with:
    # Encountered missing dependencies: unix >=2.5.1 && <2.9
    ghc = buildPackages.haskell-nix.compiler.${compiler};
  } // {
    inherit src;
    #ghc = buildPackages.haskell-nix.compiler.${compiler};
    pkg-def-extras = lib.optional stdenv.hostPlatform.isLinux (hackage: {
      packages = {
        "systemd" = (((hackage.systemd)."2.2.0").revisions).default;
      };
    });
    modules = [
      { compiler.nix-name = compiler; }
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

        # Stamp executables with the git revision
        packages.cardano-cli.components.exes.cardano-cli.postInstall = setGitRev;
        packages.cardano-node.components.exes.cardano-node.postInstall = setGitRev;
        # Work around Haskell.nix issue when setting postInstall on components
        packages.cardano-cli.components.all.postInstall = lib.mkForce setGitRev;
        packages.cardano-node.components.all.postInstall = lib.mkForce setGitRev;
      }
      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;

        # cardano-cli-tests depends on cardano-cli
        packages.cardano-cli.preCheck = "export CARDANO_CLI=${pkgSet.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli";
      }
      # TODO: Compile all local packages with -Werror:
      { packages.cardano-config.configureFlags = [ "--ghc-option=-Werror" ]; }
      #{
      #  packages = lib.genAttrs projectPackages
      #    (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
      #}
      (lib.optionalAttrs profiling {
        enableLibraryProfiling = true;
        packages.cardano-node.components.exes.cardano-node.enableExecutableProfiling = true;
      })
      (lib.optionalAttrs stdenv.hostPlatform.isLinux {
        # systemd can't be statically linked
        packages.cardano-config.flags.systemd = !stdenv.hostPlatform.isMusl;
        packages.cardano-node.flags.systemd = !stdenv.hostPlatform.isMusl;
      })
      # Musl libc fully static build
      (lib.optionalAttrs stdenv.hostPlatform.isMusl (let
        # Module options which adds GHC flags and libraries for a fully static build
        fullyStaticOptions = {
          enableShared = false;
          enableStatic = true;
        };
      in
        {
          packages = lib.genAttrs projectPackages (name: fullyStaticOptions);

          # Haddock not working and not needed for cross builds
          doHaddock = false;
        }
      ))

      (lib.optionalAttrs (stdenv.hostPlatform != stdenv.buildPlatform) {
        # Make sure we use a buildPackages version of happy
        packages.pretty-show.components.library.build-tools = [ buildPackages.haskell-nix.haskellPackages.happy ];

        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [];
        packages.terminal-size.components.library.build-tools = lib.mkForce [];
        packages.network.components.library.build-tools = lib.mkForce [];

        # Disable cabal-doctest tests by turning off custom setups
        packages.comonad.package.buildType = lib.mkForce "Simple";
        packages.distributive.package.buildType = lib.mkForce "Simple";
        packages.lens.package.buildType = lib.mkForce "Simple";
        packages.nonempty-vector.package.buildType = lib.mkForce "Simple";
        packages.semigroupoids.package.buildType = lib.mkForce "Simple";
      })
      (lib.optionalAttrs stdenv.hostPlatform.isWindows {
        # Make sure that libsodium DLLs are available for tests
        packages.cardano-api.components.all.postInstall = lib.mkForce "";
        packages.cardano-api.components.tests.cardano-api-test.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.cardano-cli.components.all.postInstall = lib.mkForce "";
        packages.cardano-cli.components.tests.cardano-cli-pioneers.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
        packages.cardano-config.components.all.postInstall = lib.mkForce "";
        packages.cardano-config.components.tests.cardano-config-test.postInstall = ''ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll'';
      })
    ];
    # TODO add flags to packages (like cs-ledger) so we can turn off tests that will
    # not build for windows on a per package bases (rather than using --disable-tests).
    # configureArgs = lib.optionalString stdenv.hostPlatform.isWindows "--disable-tests";
  });

  # setGitRev is a postInstall script to stamp executables with
  # version info. It uses the "gitrev" argument, if set. Otherwise,
  # the revision is sourced from the local git work tree.
  setGitRev = ''
    ${haskellBuildUtils}/bin/set-git-rev "${gitrev'}" $out/bin/* || true
  '';
  gitrev' = if (gitrev == null)
    then buildPackages.commonLib.commitIdFromGitRepoOrZero ../.git
    else gitrev;
  haskellBuildUtils = buildPackages.haskellBuildUtils.package;
in
  pkgSet
