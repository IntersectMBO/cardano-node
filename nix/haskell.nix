############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ pkgs
, lib
, stdenv
, haskell-nix
, buildPackages
# GHC attribute name
, compiler
# Enable profiling
, profiling ? false
# Enable asserts for given packages
, assertedPackages ? []
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
    (haskell-nix.cabalProject {
      inherit src;
      compiler-nix-name = "ghc8102";
    }));

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject ({
    inherit src;
    compiler-nix-name = compiler;
    cabalProjectLocal = ''
      allow-newer: terminfo:base
    '';
    modules = [
      # Allow reinstallation of Win32
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
       nonReinstallablePkgs =
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
      })
      {
        # Needed for the CLI tests.
        # Coreutils because we need 'paste'.
        packages.cardano-cli.components.tests.cardano-cli-test.build-tools =
          lib.mkForce [buildPackages.jq buildPackages.coreutils buildPackages.shellcheck];
        packages.cardano-cli.components.tests.cardano-cli-golden.build-tools =
          lib.mkForce [buildPackages.jq buildPackages.coreutils buildPackages.shellcheck];
      }
      {
        # Stamp executables with the git revision
        # And make sure that libsodium DLLs are available for windows binaries:
        packages = lib.genAttrs projectPackages (name: {
            postInstall = ''
              if [ -d $out/bin ]; then
                ${setGitRev}
                ${lib.optionalString stdenv.hostPlatform.isWindows
                  "ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll"
                }
              fi
            '';
          });
      }
      ({ pkgs, config, ... }: {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;

        # cardano-cli-test depends on cardano-cli
        packages.cardano-cli.preCheck = "export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}";

        # build-tool-depends are used in cardano-node-chairman but only
        # tasty-discover should be from the buildPackages.
        packages.cardano-node-chairman.components.tests.chairman-tests.build-tools =
          lib.mkForce [
            config.hsPkgs.buildPackages.tasty-discover.components.exes.tasty-discover
            config.hsPkgs.cardano-node.components.exes.cardano-node
            config.hsPkgs.cardano-cli.components.exes.cardano-cli
            config.hsPkgs.cardano-node-chairman.components.exes.cardano-node-chairman];
        # cardano-node-chairman depends on cardano-node and cardano-cli
        packages.cardano-node-chairman.preCheck = "
          export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
          export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
          export CARDANO_NODE_CHAIRMAN=${config.hsPkgs.cardano-node-chairman.components.exes.cardano-node-chairman}/bin/cardano-node-chairman${pkgs.stdenv.hostPlatform.extensions.executable}
          export CARDANO_NODE_SRC=${src}
        ";
      })
      {
        packages = lib.genAttrs projectPackages
          (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
      }
      (lib.optionalAttrs profiling {
        enableLibraryProfiling = true;
        packages.cardano-node.components.exes.cardano-node.enableExecutableProfiling = true;
      })
      {
        packages = lib.genAttrs assertedPackages
          (name: { flags.asserts = true; });
      }
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
        # systemd can't be statically linked
        packages.cardano-config.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
        packages.cardano-node.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
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

      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [];
        packages.terminal-size.components.library.build-tools = lib.mkForce [];
        packages.network.components.library.build-tools = lib.mkForce [];
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
