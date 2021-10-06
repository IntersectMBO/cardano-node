############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, haskell-nix
, buildPackages
# GHC attribute name
, compiler-nix-name
# Enable profiling
, profiling ? false
# Link with -eventlog
, eventlog ? false
# Enable asserts for given packages
, assertedPackages ? []
# Version info, to be passed when not building from a git work tree
, gitrev ? null
}:
let

  src = (haskell-nix.haskellLib.cleanGit {
    name = "cardano-node-src";
    src = ../.;
  });

  rawProject = haskell-nix.cabalProject' (mkProjectArgs []);

  projectPackages =  lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    rawProject.hsPkgs);

  # It is important this common options matches in both calls to cabalProject or `cabal configure`
  # will run twice.
  mkProjectArgs = modules: {pkgs, ...}: {
    inherit compiler-nix-name  src modules;
    cabalProjectLocal = ''
      allow-newer: terminfo:base
    '' + lib.optionalString pkgs.stdenv.hostPlatform.isWindows ''
      -- When cross compiling we don't have a `ghc` package
      package plutus-tx-plugin
        flags: +use-ghc-stub
    '';
  };

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject' (mkProjectArgs [
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
      # Disable plutus-example on Windows because Windows builds are cross-compiled which
      # does not work for plugins that are used by plutus-example.
      packages.plutus-example.package.buildable = false;
      packages.create-script-context.package.buildable = false;
      packages.plutus-example-test.package.buildable = false;
      packages.plutus-ledger.package.buildable = false;
      # When cross compfixesiling we don't have a `ghc` package
      packages.plutus-tx-plugin.flags.use-ghc-stub = true;
    })
    {
      # Tell `release-lib` what to exclude these from windows builds
      packages.plutus-example.components.library.platforms = with lib.platforms; [ linux darwin ];
      packages.plutus-example.components.exes.plutus-example.platforms = with lib.platforms; [ linux darwin ];
      packages.plutus-ledger.components.library.platforms = with lib.platforms; [ linux darwin ];
      packages.plutus-tx-plugin.components.library.platforms = with lib.platforms; [ linux darwin ];
    }
    ({pkgs, ...}: {
      # Needed for the CLI tests.
      # Coreutils because we need 'paste'.
      packages.cardano-cli.components.tests.cardano-cli-test.build-tools =
        lib.mkForce (with pkgs.buildPackages; [jq coreutils shellcheck]);
      packages.cardano-cli.components.tests.cardano-cli-golden.build-tools =
        lib.mkForce (with pkgs.buildPackages; [jq coreutils shellcheck]);
      packages.cardano-testnet.components.tests.cardano-testnet-tests.build-tools =
        lib.mkForce (with pkgs.buildPackages; [jq coreutils shellcheck]);
    })
    ({ pkgs, ...}: {
      # Use the VRF fork of libsodium
      packages = lib.genAttrs [ "cardano-crypto-praos" "cardano-crypto-class" ] (_: {
        components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
      });
    })
    ({ pkgs, options, ...}: {
      # make sure that libsodium DLLs are available for windows binaries,
      # stamp executables with the git revision, add shell completion, strip/rewrite:
      packages = lib.genAttrs projectPackages (name: {
        # For checks:
        postInstall = lib.mkIf pkgs.stdenv.hostPlatform.isWindows ''
          if [ -d $out/bin ]; then
            ${setLibSodium pkgs.libsodium-vrf}
          fi
        '';
        components.exes = lib.genAttrs (lib.attrNames rawProject.pkg-set.options.packages.value.${name}.components.exes) (exe: {
          postInstall = ''
            ${lib.optionalString pkgs.stdenv.hostPlatform.isWindows (setLibSodium pkgs.libsodium-vrf)}
            ${setGitRev pkgs}
            ${lib.optionalString (pkgs.stdenv.hostPlatform.isMusl) ''
              ${pkgs.buildPackages.binutils-unwrapped}/bin/*strip $out/bin/*
            ''}
            ${lib.optionalString (pkgs.stdenv.hostPlatform.isDarwin) ''
              export PATH=$PATH:${lib.makeBinPath [ pkgs.haskellBuildUtils pkgs.buildPackages.binutils pkgs.buildPackages.nix ]}
              ${pkgs.haskellBuildUtils}/bin/rewrite-libs $out/bin $out/bin/*
            ''}
             ${lib.optionalString (!pkgs.stdenv.hostPlatform.isWindows
              && lib.elem exe ["cardano-node" "cardano-cli" "cardano-topology" "locli"]) ''
              BASH_COMPLETIONS=$out/share/bash-completion/completions
              ZSH_COMPLETIONS=$out/share/zsh/site-functions
              mkdir -p $BASH_COMPLETIONS $ZSH_COMPLETIONS
              $out/bin/${exe} --bash-completion-script ${exe} > $BASH_COMPLETIONS/${exe}
              $out/bin/${exe} --zsh-completion-script ${exe} > $ZSH_COMPLETIONS/_${exe}
            ''}
          '';
        });
      });
    })
    ({ pkgs, config, ... }: {
      # Packages we wish to ignore version bounds of.
      # This is similar to jailbreakCabal, however it
      # does not require any messing with cabal files.
      packages.katip.doExactConfig = true;
      # we need the following shared libraries for the musl build to succeed
      # musl on x86_64, can load dynamic libraries, and we need it to use the
      # shared loader, as the one in GHC is not very stable.
      packages.cardano-api.components.library.enableShared = true;
      packages.cardano-config.components.library.enableShared = true;
      packages.cardano-node.components.library.enableShared = true;
      packages.cardano-cli.components.library.enableShared = true;
      # split data output for ekg to reduce closure size
      packages.ekg.components.library.enableSeparateDataOutput = true;
      # cardano-cli-test depends on cardano-cli
      packages.cardano-cli.preCheck = "
        export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
        export CARDANO_NODE_SRC=${src}
      ";
      packages.cardano-node-chairman.components.tests.chairman-tests.build-tools =
        lib.mkForce [
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
      # cardano-testnet needs access to the git repository source
      packages.cardano-testnet.preCheck = "
        export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
        export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
        export CARDANO_SUBMIT_API=${config.hsPkgs.cardano-submit-api.components.exes.cardano-submit-api}/bin/cardano-submit-api${pkgs.stdenv.hostPlatform.extensions.executable}
        export CREATE_SCRIPT_CONTEXT=${config.hsPkgs.plutus-example.components.exes.create-script-context}/bin/create-script-context${pkgs.stdenv.hostPlatform.extensions.executable}
        export CARDANO_NODE_SRC=${src}
      ";
    })
    ({ pkgs, ... }: lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
      # Needed for profiled builds to fix an issue loading recursion-schemes part of makeBaseFunctor
      # that is missing from the `_p` output.  See https://gitlab.haskell.org/ghc/ghc/-/issues/18320
      # This work around currently breaks regular builds on macOS with:
      # <no location info>: error: ghc: ghc-iserv terminated (-11)
      packages.plutus-core.components.library.ghcOptions = [ "-fexternal-interpreter" ];
    })
    {
      packages = lib.genAttrs projectPackages
        (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
    }
    (lib.optionalAttrs eventlog {
      packages = lib.genAttrs ["cardano-node"]
        (name: { configureFlags = [ "--ghc-option=-eventlog" ]; });
    })
    (lib.optionalAttrs profiling {
      enableLibraryProfiling = true;
      packages.cardano-node.components.exes.cardano-node.enableExecutableProfiling = true;
      packages.tx-generator.components.exes.tx-generator.enableExecutableProfiling = true;
      packages.locli.components.exes.locli.enableExecutableProfiling = true;
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
    ({ pkgs, ... }:  lib.mkIf pkgs.stdenv.hostPlatform.isMusl (let
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
    # TODO add flags to packages (like cs-ledger) so we can turn off tests that will
    # not build for windows on a per package bases (rather than using --disable-tests).
    # configureArgs = lib.optionalString stdenv.hostPlatform.isWindows "--disable-tests";
  ]);

  # setGitRev is a postInstall script to stamp executables with
  # version info. It uses the "gitrev" argument, if set. Otherwise,
  # the revision is sourced from the local git work tree.
  setGitRev = pkgs: ''${pkgs.buildPackages.haskellBuildUtils}/bin/set-git-rev "${gitrev}" $out/bin/*'';
  # package with libsodium:
  setLibSodium = libsodium-vrf: "ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll";
in
  pkgSet // {
    inherit src projectPackages;
  }
