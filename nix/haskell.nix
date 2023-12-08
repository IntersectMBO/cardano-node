############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ haskell-nix
, incl
, CHaP
}:
let

  inherit (haskell-nix) haskellLib;

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  project = haskell-nix.cabalProject' ({ pkgs
                                       , lib
                                       , config
                                       , buildProject
                                       , ...
                                       }:
    {
      src = ../.;
      name = "cardano-node";
      compiler-nix-name = lib.mkDefault "ghc928";
      # extra-compilers
      flake.variants = lib.genAttrs [ ] (x: {compiler-nix-name = x;});
      cabalProjectLocal = ''
        repository cardano-haskell-packages-local
          url: file:${CHaP}
          secure: True
        active-repositories: hackage.haskell.org, cardano-haskell-packages-local
        allow-newer: terminfo:base
      '' + lib.optionalString pkgs.stdenv.hostPlatform.isWindows ''
        -- When cross compiling we don't have a `ghc` package
        package plutus-tx-plugin
          flags: +use-ghc-stub
      '';
      inputMap = {
        "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
      };
      shell = {
        name = "cabal-dev-shell";

        # These programs will be available inside the nix-shell.
        nativeBuildInputs = with pkgs.pkgsBuildBuild; [
          nix-prefetch-git
          pkg-config
          hlint
          ghcid
          haskell-language-server
          cabal
          git
          glibcLocalesUtf8
        ];

        withHoogle = true;
      };


      # Additional configuration for the project package set
      # ----------------------------------------------------
      #
      # The following configuration influeces how the project package set
      # is built by nix. The configuration uses the same modular system as
      # NixOS.
      #
      # For additional background information see
      #
      # https://nixos.org/manual/nixos/stable/index.html#sec-writing-modules
      #
      # For a list of all configuration options supported by Haskell.nix see
      #
      # https://input-output-hk.github.io/haskell.nix/reference/modules.html
      #
      # Tips:
      #
      # 1. Nix is a lazy language and the resulting `project` is already
      # in scope. Avoid referring to any of `project` attributes from
      # within a configuration module. You can instead refer to the final
      # package set configuration in `config`. If you run into infinite
      # resursion issues, troubleshooting the latter will be much easier
      # than troubleshooting the former.
      #
      # 2. The same configuration option can be set to different values
      # from separate modules. How these values will be merged together
      # dependes on the type of the option. The type of each option is
      # listed in the haskell.nix modules reference pages.
      #
      # 3. The option `packages` is an attrset of submodules, i.e. each
      # package has its own modular configuration. Similarly, `.components`
      # is an attrset of submodules inside each package.
      #
      # 4. You can specify a configuration for every package by adding a
      # module to the definition of `packages` itself, i.e:
      #
      # {
      #   options.packages = lib.mkOption {
      #     type = lib.types.attrsOf (lib.types.submodule (
      #       { config, name, ... }:
      #       #         ^-- package name passed as a extra argument
      #       # ^-- this is now the final package configuration
      #       {
      #         # e.g.
      #         configureFlags = [ "--ghc-option=-Werror"];
      #       }
      #     ));
      # }
      #
      # The same applies to every component inside a package.
      #
      modules =
        [
          ({ lib, pkgs, ... }: {
            packages.cardano-tracer.package.buildable = with pkgs.stdenv.hostPlatform; lib.mkForce (!isMusl);
            packages.cardano-node-chairman.components.tests.chairman-tests.buildable = lib.mkForce pkgs.stdenv.hostPlatform.isUnix;
            packages.plutus-tx-plugin.components.library.platforms = with lib.platforms; [ linux darwin ];
            packages.tx-generator.package.buildable = with pkgs.stdenv.hostPlatform; !isMusl;

            packages.cardano-ledger-alonzo.components.library.doHaddock = false;
            packages.cardano-ledger-babbage.components.library.doHaddock = false;
            packages.cardano-ledger-conway.components.library.doHaddock = false;
            packages.cardano-protocol-tpraos.components.library.doHaddock = false;
          })
          ({ lib, pkgs, ...}: lib.mkIf (pkgs.stdenv.hostPlatform.isWindows) {
            # Remvoe this once mingwx is mapped to null in haskell.nix (haskell.nix#2032), and we bumped _past_ that.
            # we need to plugin in pthreads as force overrides https://github.com/input-output-hk/haskell.nix/blob/9823e12d5b6e66150ddeea146aea682f44ee4d44/overlays/windows.nix#L109.
            packages.unix-time.components.library.libs = lib.mkForce [ pkgs.windows.mingw_w64_pthreads ];

            # This fix seems fairly fishy; but somehow it's required to make this work :confused_parrot:
            packages.unix-compat.postPatch = ''
              sed -i 's/msvcrt//g' unix-compat.cabal
            '';
            packages.unix-time.postPatch = ''
              sed -i 's/mingwex//g' unix-time.cabal
            '';
            # For these two packages the custom setups fail, as we end up with multiple instances of
            # lib:Cabal. Likely a haskell.nix bug.
            packages.entropy.package.buildType = lib.mkForce "Simple";
            packages.HsOpenSSL.package.buildType = lib.mkForce "Simple";
            #packages.plutus-core.components.library.preBuild = ''
            #  export ISERV_ARGS="-v +RTS -Dl"
            #  export PROXY_ARGS=-v
            #'';
          })
          ({ lib, pkgs, config, ... }: lib.mkIf (builtins.compareVersions config.compiler.version "9.4" >= 0) {
            # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
            # to call out to all kinds of silly tools that GHC doesn't really provide.
            # For this reason, we try to get away without re-installing lib:ghc for now.
            reinstallableLibGhc = false;
          })
          ({ lib, pkgs, ... }: {
            # Needed for the CLI tests.
            # Coreutils because we need 'paste'.
            packages.cardano-testnet.components.tests.cardano-testnet-tests.build-tools =
              lib.mkForce (with pkgs.buildPackages; [ jq coreutils shellcheck lsof ]);
          })
          ({ lib, pkgs, ... }: {
            # Use the VRF fork of libsodium
            packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
            packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst ] ];
          })
          ({ lib, pkgs, ... }:
          let postInstall = exeName: ''
              BASH_COMPLETIONS=$out/share/bash-completion/completions
              ZSH_COMPLETIONS=$out/share/zsh/site-functions
              mkdir -p $BASH_COMPLETIONS $ZSH_COMPLETIONS
              $out/bin/${exeName} --bash-completion-script ${exeName} > $BASH_COMPLETIONS/${exeName}
              $out/bin/${exeName} --zsh-completion-script ${exeName} > $ZSH_COMPLETIONS/_${exeName}
            '';
          in lib.mkIf (!pkgs.stdenv.hostPlatform.isWindows)
          {
            # add shell completion:
            packages.cardano-node.components.exes.cardano-node.postInstall = postInstall "cardano-node";
            packages.cardano-cli.components.exes.cardano-cli.postInstall = postInstall "cardano-cli";
            packages.cardano-topology.components.exes.cardano-topology.postInstall = postInstall "cardano-topology";
            packages.locli.components.exes.locli.postInstall = postInstall "locli";
          })
          ({ lib, pkgs, config, ... }:
            let
              exportCliPath = "export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}";
              exportNodePath = "export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}";
              mainnetConfigFiles = [
                "configuration/cardano/mainnet-config.yaml"
                "configuration/cardano/mainnet-config.json"
                "configuration/cardano/mainnet-byron-genesis.json"
                "configuration/cardano/mainnet-shelley-genesis.json"
                "configuration/cardano/mainnet-alonzo-genesis.json"
                "configuration/cardano/mainnet-conway-genesis.json"
              ];
              goldenConfigFiles = [
                "configuration/defaults/byron-mainnet"
                "cardano-testnet/test/cardano-testnet-golden/files/golden/allegra_node_default_config.json"
                "cardano-testnet/test/cardano-testnet-golden/files/golden/alonzo_node_default_config.json"
                "cardano-testnet/test/cardano-testnet-golden/files/golden/babbage_node_default_config.json"
                "cardano-testnet/test/cardano-testnet-golden/files/golden/byron_node_default_config.json"
                "cardano-testnet/test/cardano-testnet-golden/files/golden/conway_node_default_config.json"
                "cardano-testnet/test/cardano-testnet-golden/files/golden/mary_node_default_config.json"
                "cardano-testnet/test/cardano-testnet-golden/files/golden/shelley_node_default_config.json"
                "cardano-testnet/files/data/alonzo/genesis.alonzo.spec.json"
                "cardano-testnet/files/data/conway/genesis.conway.spec.json"
              ];
            in
            {
              # split data output for ekg to reduce closure size
              packages.ekg.components.library.enableSeparateDataOutput = true;
              packages.cardano-node-chairman.components.tests.chairman-tests.build-tools =
                lib.mkForce [
                  pkgs.lsof
                  config.hsPkgs.cardano-node.components.exes.cardano-node
                  config.hsPkgs.cardano-cli.components.exes.cardano-cli
                  config.hsPkgs.cardano-node-chairman.components.exes.cardano-node-chairman
                ];
              # cardano-node-chairman depends on cardano-node and cardano-cli, and some config files
              packages.cardano-node-chairman.preCheck =
                let
                  # This define files included in the directory that will be passed to `H.getProjectBase` for this test:
                  filteredProjectBase = incl ../. goldenConfigFiles;
                in
                ''
                  ${exportCliPath}
                  ${exportNodePath}
                  export CARDANO_NODE_CHAIRMAN=${config.hsPkgs.cardano-node-chairman.components.exes.cardano-node-chairman}/bin/cardano-node-chairman${pkgs.stdenv.hostPlatform.extensions.executable}
                  export CARDANO_NODE_SRC=${filteredProjectBase}
                '';
              # cardano-testnet depends on cardano-node, cardano-cli, cardano-submit-api and some config files
              packages.cardano-node.components.tests.cardano-node-test.preCheck =
                let
                  # This define files included in the directory that will be passed to `H.getProjectBase` for this test:
                  filteredProjectBase = incl ../. (mainnetConfigFiles ++ [
                    "cardano-node/test-data/ledger_events.cddl"
                  ]);
                in
                ''
                  export CARDANO_NODE_SRC=${filteredProjectBase}
                '';
              packages.cardano-testnet.preCheck =
                let
                  # This define files included in the directory that will be passed to `H.getProjectBase` for this test:
                  filteredProjectBase = incl ../. (mainnetConfigFiles ++ goldenConfigFiles ++ [
                    "configuration/cardano/mainnet-topology.json"
                    "configuration/cardano/mainnet-conway-genesis.json"
                    "scripts/babbage/alonzo-babbage-test-genesis.json"
                    "scripts/babbage/conway-babbage-test-genesis.json"
                  ]);
                in
                ''
                  ${exportCliPath}
                  ${exportNodePath}
                  export CARDANO_SUBMIT_API=${config.hsPkgs.cardano-submit-api.components.exes.cardano-submit-api}/bin/cardano-submit-api${pkgs.stdenv.hostPlatform.extensions.executable}
                  export CARDANO_NODE_SRC=${filteredProjectBase}
                ''
                # the cardano-testnet-tests, use sockets stored in a temporary directory
                # however on macOS the socket path's max is 104 chars. The package name
                # is already long, and as such the constructed socket path
                #
                #   /private/tmp/nix-build-cardano-testnet-test-cardano-testnet-tests-1.36.0-check.drv-1/chairman-test-93c5d9288dd8e6bc/socket/node-bft1
                #
                # exceeds taht limit easily. We therefore set a different tmp directory
                # during the preBuild phase.
                + ''
                  # unset TMPDIR, otherwise mktemp will use that as a base
                  unset TMPDIR
                  export TMPDIR=$(mktemp -d)
                  export TMP=$TMPDIR
                '';
              packages.cardano-testnet.components.tests.cardano-testnet-golden.preCheck =
                let
                  # This define files included in the directory that will be passed to `H.getProjectBase` for this test:
                  filteredProjectBase = incl ../. [
                    "cardano-testnet/test/cardano-testnet-golden/files/golden/allegra_node_default_config.json"
                    "cardano-testnet/test/cardano-testnet-golden/files/golden/alonzo_node_default_config.json"
                    "cardano-testnet/test/cardano-testnet-golden/files/golden/babbage_node_default_config.json"
                    "cardano-testnet/test/cardano-testnet-golden/files/golden/byron_node_default_config.json"
                    "cardano-testnet/test/cardano-testnet-golden/files/golden/conway_node_default_config.json"
                    "cardano-testnet/test/cardano-testnet-golden/files/golden/mary_node_default_config.json"
                    "cardano-testnet/test/cardano-testnet-golden/files/golden/shelley_node_default_config.json"
                  ];
                in
                ''
                  ${exportCliPath}
                  export CARDANO_TESTNET=${config.hsPkgs.cardano-testnet.components.exes.cardano-testnet}/bin/cardano-testnet${pkgs.stdenv.hostPlatform.extensions.executable}
                  export CARDANO_NODE_SRC=${filteredProjectBase}
                '';
              # cardano-tracer-test-ext, will default to /tmp/testTracerExt, which means
              # if this test is run in parallel, things will just hang; or break.
              packages.cardano-tracer.components.tests.cardano-tracer-test-ext.preCheck = ''
                  # unset TMPDIR, otherwise mktemp will use that as a base
                  unset TMPDIR
                  export TMPDIR=$(mktemp -d)
                  export TMP=$TMPDIR
                  mkdir $TMP/testTracerExt
                  # workaround the broken --workdir argument parser.
                  # (it doesn't properly strip args before passing them to tasty;
                  # also needs them to be quoted)
                  export WORKDIR=$TMP/testTracerExt
              '';
            })
          ({ lib, pkgs, ... }: lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
            # Needed for profiled builds to fix an issue loading recursion-schemes part of makeBaseFunctor
            # that is missing from the `_p` output.  See https://gitlab.haskell.org/ghc/ghc/-/issues/18320
            # This work around currently breaks regular builds on macOS with:
            # <no location info>: error: ghc: ghc-iserv terminated (-11)
            packages.plutus-core.components.library.ghcOptions = [ "-fexternal-interpreter" ];
          })
          ({ lib, ... }@args: {
            options.packages = lib.mkOption {
              type = lib.types.attrsOf (lib.types.submodule (
                { config, lib, ... }:
                lib.mkIf config.package.isLocal
                {
                  configureFlags = [ "--ghc-option=-Werror"]
                    ++ lib.optional (args.config.compiler.version == "8.10.7") "--ghc-option=-Wwarn=unused-packages";
                }
              ));
            };
          })
          ({ lib, pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
            # systemd can't be statically linked
            packages.cardano-git-rev.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
            packages.cardano-node.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
            packages.cardano-tracer.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
          })
          # disable haddock
          # Musl libc fully static build
          ({ lib, ... }: {
            options.packages = lib.mkOption {
              type = lib.types.attrsOf (lib.types.submodule (
                { config, lib, pkgs, ...}:
                lib.mkIf (pkgs.stdenv.hostPlatform.isMusl && config.package.isLocal)
                {
                  # Module options which adds GHC flags and libraries for a fully static build
                  enableShared = false;
                  enableStatic = true;
                }
              ));
            };
            config =
              lib.mkIf pkgs.stdenv.hostPlatform.isMusl
              {
                # Haddock not working and not needed for cross builds
                doHaddock = false;
                packages.cardano-cli.enableShared = false;
                packages.cardano-cli.enableStatic = true;
              };
          })
          ({ lib, pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
            # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
            packages.Win32.components.library.build-tools = lib.mkForce [ ];
            packages.terminal-size.components.library.build-tools = lib.mkForce [ ];
            packages.network.components.library.build-tools = lib.mkForce [ ];
          })
          ({ ... }: {
            # TODO: requires
            # https://github.com/input-output-hk/ouroboros-network/pull/4673 or
            # a newer ghc
            packages.ouroboros-network-framework.doHaddock = false;
          })
          # TODO add flags to packages (like cs-ledger) so we can turn off tests that will
          # not build for windows on a per package bases (rather than using --disable-tests).
          # configureArgs = lib.optionalString stdenv.hostPlatform.isWindows "--disable-tests";
        ];
    });
in
project.appendOverlays (with haskellLib.projectOverlays; [
  projectComponents
  (final: prev:
    let inherit (final.pkgs) lib; in {
      profiled = final.appendModule {
        modules = [{
          enableLibraryProfiling = true;
          packages.cardano-node.components.exes.cardano-node.enableProfiling = true;
          packages.tx-generator.components.exes.tx-generator.enableProfiling = true;
          packages.locli.components.exes.locli.enableProfiling = true;
        }
        {
          packages = final.pkgs.lib.genAttrs
            [ "cardano-node"
              "cardano-tracer"
              "trace-dispatcher"
              "trace-forward"
              "trace-resources"
            ]
            (name: { configureFlags = [ "--ghc-option=-fprof-auto" ]; });
        }];
      };
      asserted = final.appendModule {
        modules = [{
          packages = lib.genAttrs [
            "ouroboros-consensus"
            "ouroboros-consensus-cardano"
            "ouroboros-network"
            "network-mux"
          ]
            (name: { flags.asserts = true; });
        }];
      };
      eventlogged = final.appendModule
        {
          # From 9.2+
          # on the commandline: error: [-Wdeprecated-flags, Werror=deprecated-flags]
          #     -eventlog is deprecated: the eventlog is now enabled in all runtime system ways
          modules = [({ lib, pkgs, config, ... }: lib.mkIf (builtins.compareVersions config.compiler.version "9.2" < 0) {
            packages = final.pkgs.lib.genAttrs [ "cardano-node" ]
              (name: { configureFlags = [ "--ghc-option=-eventlog" ]; });
          })];
        };
      # add passthru and gitrev to hsPkgs:
      hsPkgs = lib.mapAttrsRecursiveCond (v: !(lib.isDerivation v))
        (path: value:
          if (lib.isAttrs value) then
            lib.recursiveUpdate
              value
              {
                # Also add convenient passthru to some alternative compilation configurations:
                passthru = {
                  profiled = lib.getAttrFromPath path final.profiled.hsPkgs;
                  asserted = lib.getAttrFromPath path final.asserted.hsPkgs;
                  eventlogged = lib.getAttrFromPath path final.eventlogged.hsPkgs;
                };
              }
          else value)
        prev.hsPkgs;
    })
])
