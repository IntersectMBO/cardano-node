{
  description = "Cardano Node";

  inputs = {
    # IMPORTANT: report any change to nixpkgs channel in nix/default.nix:
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    hostNixpkgs.follows = "nixpkgs";
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    nixTools = {
        url = "github:input-output-hk/nix-tools";
        flake = false;
      };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackageNix";
    };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
      flake = false;
    };
    plutus-apps = {
      url = "github:input-output-hk/plutus-apps";
      flake = false;
    };

    # Custom user config (default: empty), eg.:
    # { outputs = {...}: {
    #   # Cutomize listeming port of node scripts:
    #   nixosModules.cardano-node = {
    #     services.cardano-node.port = 3002;
    #   };
    # };
    customConfig.url = "github:input-output-hk/empty-flake";

    node-measured = {
      url = "github:input-output-hk/cardano-node";
    };
    node-snapshot = {
      url = "github:input-output-hk/cardano-node/7f00e3ea5a61609e19eeeee4af35241571efdf5c";
    };
    node-process = {
      url = "github:input-output-hk/cardano-node";
      flake = false;
    };
    ## This pin is to prevent workbench-produced geneses being regenerated each time the node is bumped.
    cardano-node-workbench = {
      url = "github:input-output-hk/cardano-node/ed9932c52aaa535b71f72a5b4cc0cecb3344a5a3";
      # This is to avoid circular import (TODO: remove this workbench pin entirely using materialization):
      inputs.membench.url = "github:input-output-hk/empty-flake";
    };

    cardano-mainnet-mirror.url = "github:input-output-hk/cardano-mainnet-mirror/nix";
  };

  outputs =
    { self
    , nixpkgs
    , hostNixpkgs
    , utils
    , haskellNix
    , CHaP
    , iohkNix
    , plutus-apps
    , cardano-mainnet-mirror
    , node-snapshot
    , node-measured
    , node-process
    , cardano-node-workbench
    , ...
    }@input:
    let
      inherit (nixpkgs) lib;
      inherit (lib) head systems mapAttrs recursiveUpdate mkDefault
        getAttrs optionalAttrs nameValuePair attrNames;
      inherit (utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) prefixNamesWith;
      removeRecurse = lib.filterAttrsRecursive (n: _: n != "recurseForDerivations");

      supportedSystems = import ./nix/supported-systems.nix;
      defaultSystem = head supportedSystems;
      customConfig = recursiveUpdate
        (import ./nix/custom-config.nix customConfig)
        input.customConfig;

      overlays = [
        haskellNix.overlay
        iohkNix.overlays.haskell-nix-extra
        iohkNix.overlays.crypto
        iohkNix.overlays.cardano-lib
        iohkNix.overlays.utils
        (final: prev: {
          inherit customConfig;
          gitrev = final.customConfig.gitrev or self.rev or "0000000000000000000000000000000000000000";
          commonLib = lib
            // iohkNix.lib
            // final.cardanoLib
            // import ./nix/svclib.nix { inherit (final) pkgs; };
        })
        (import ./nix/pkgs.nix)
        (import ./nix/workbench/membench-overlay.nix
          {
            inherit
              lib
              input
              cardano-mainnet-mirror
              node-snapshot node-measured node-process;
            customConfig = customConfig.membench;
          })
        self.overlay
      ];

      collectExes = project:
        let
          inherit (project.pkgs.stdenv) hostPlatform;
          inherit ((import plutus-apps {
            inherit (project.pkgs) system;
          }).plutus-apps.haskell.packages.plutus-example.components.exes) plutus-example;

        in project.exes // (with project.hsPkgs; {
            inherit (ouroboros-consensus-byron.components.exes) db-converter;
            inherit (ouroboros-consensus-cardano.components.exes) db-analyser;
            inherit (ouroboros-consensus-cardano-tools.components.exes) db-synthesizer;
            inherit (bech32.components.exes) bech32;
          } // lib.optionalAttrs hostPlatform.isUnix {
            inherit (network-mux.components.exes) cardano-ping;
            inherit plutus-example;
          });

      mkCardanoNodePackages = project: (collectExes project) // {
        inherit (project.pkgs) cardanoLib;
      };

      flake = eachSystem supportedSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          inherit (pkgs.haskell-nix) haskellLib;
          inherit (haskellLib) collectChecks' collectComponents';
          inherit (pkgs.commonLib) eachEnv environments mkSupervisordCluster;
          inherit (project.pkgs.stdenv) hostPlatform;

          project = (import ./nix/haskell.nix {
            inherit (pkgs) haskell-nix gitrev;
            inputMap = {
              "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
            };
          }).appendModule customConfig.haskellNix;

          pinned-workbench = cardano-node-workbench.workbench.${system};

          shell = import ./shell.nix { inherit pkgs customConfig cardano-mainnet-mirror; };
          devShells = {
            inherit (shell) devops;
            cluster = shell;
            profiled = project.profiled.shell;
          };

          devShell = shell.dev;

          # NixOS tests run a node and submit-api and validate it listens
          nixosTests = import ./nix/nixos/tests {
            inherit pkgs;
          };

          checks = flattenTree project.checks //
            # Linux only checks:
            (optionalAttrs hostPlatform.isLinux (
              prefixNamesWith "nixosTests/" (mapAttrs (_: v: v.${system} or v) nixosTests)
            ))
            # checks run on default system only;
            // (optionalAttrs (system == defaultSystem) {
            hlint = pkgs.callPackage pkgs.hlintCheck {
              inherit (project.args) src;
            };
          });

          exes = (collectExes project) // {
            inherit (pkgs) cabalProjectRegenerate checkCabalProject;
            "dockerImages/push" = import ./.buildkite/docker-build-push.nix {
              hostPkgs = import hostNixpkgs { inherit system; };
              inherit (pkgs) dockerImage submitApiDockerImage;
            };
            "dockerImage/node/load" = pkgs.writeShellScript "load-docker-image" ''
              docker load -i ${pkgs.dockerImage} $@
            '';
            "dockerImage/submit-api/load" = pkgs.writeShellScript "load-submit-docker-image" ''
              docker load -i ${pkgs.submitApiDockerImage} $@
            '';
          } // flattenTree (pkgs.scripts // {
            # `tests` are the test suites which have been built.
            inherit (project) tests;
            # `benchmarks` (only built, not run).
            inherit (project) benchmarks;
          });

          inherit (pkgs) workbench all-profiles-json supervisord-workbench-nix supervisord-workbench-for-profile;

          packages =
            let
              supervisord-workbench =
                pkgs.callPackage supervisord-workbench-nix { workbench = pinned-workbench; };
            in
            exes
            # Linux only packages:
            // optionalAttrs (system == "x86_64-linux") rec {
              "dockerImage/node" = pkgs.dockerImage;
              "dockerImage/submit-api" = pkgs.submitApiDockerImage;
              ## TODO: drop external membench, once we bump 'node-snapshot'
              # snapshot = membench.outputs.packages.x86_64-linux.snapshot;
              # membenches = pkgs.membench-node-this-5.batch-report;
              # workbench-smoke-test =
              #   (pkgs.supervisord-workbench-for-profile
              #     {
              #       inherit supervisord-workbench;
              #       profileName = "smoke-alzo";
              #     }
              #   ).profile-run { trace = true; };
              # workbench-ci-test =
              #   (pkgs.supervisord-workbench-for-profile
              #     {
              #       inherit supervisord-workbench;
              #       profileName = "k6-600slots-1000kU-1000kD-64kbs-10tps-fixed-loaded-alzo";
              #     }
              #   ).profile-run { };
              # workbench-smoke-analysis = workbench-smoke-test.analysis;
              # workbench-ci-analysis = workbench-ci-test.analysis;
              all-profiles-json = pkgs.all-profiles-json;
            }
            # Add checks to be able to build them individually
            // (prefixNamesWith "checks/" checks);

          apps = lib.mapAttrs (n: p: { type = "app"; program = p.exePath or (if (p.executable or false) then "${p}" else "${p}/bin/${p.name or n}"); }) exes;

        in
        {

          inherit environments packages checks apps project;

          legacyPackages = pkgs;

          # Built by `nix build .`
          defaultPackage = packages.cardano-node;

          # Run by `nix run .`
          defaultApp = apps.cardano-node;

          # This is used by `nix develop .` to open a devShell
          inherit devShell devShells;

          # The parametrisable workbench.
          inherit workbench;

          systemHydraJobs = optionalAttrs (system == "x86_64-linux")
            {
              linux = {
                native = packages // {
                  shells = devShells // {
                    default = devShell;
                  };
                  internal = {
                    roots.project = project.roots;
                    plan-nix.project = project.plan-nix;
                  };
                  profiled = lib.genAttrs [ "cardano-node" "tx-generator" "locli" ] (n:
                    packages.${n}.passthru.profiled
                  );
                  asserted = lib.genAttrs [ "cardano-node" ] (n:
                    packages.${n}.passthru.asserted
                  );
                };
                musl =
                  let
                    muslProject = project.projectCross.musl64;
                    projectExes = collectExes muslProject;
                  in
                  projectExes // {
                    cardano-node-linux = import ./nix/binary-release.nix {
                      inherit pkgs;
                      inherit (exes.cardano-node.identifier) version;
                      platform = "linux";
                      exes = lib.collect lib.isDerivation projectExes;
                    };
                    internal.roots.project = muslProject.roots;
                  };
                windows =
                  let
                    windowsProject = project.projectCross.mingwW64;
                    projectExes = collectExes windowsProject;
                  in
                  projectExes
                    // (removeRecurse {
                    inherit (windowsProject) checks tests benchmarks;
                    cardano-node-win64 = import ./nix/binary-release.nix {
                      inherit pkgs;
                      inherit (exes.cardano-node.identifier) version;
                      platform = "win64";
                      exes = lib.collect lib.isDerivation projectExes;
                    };
                    internal.roots.project = windowsProject.roots;
                  });
              };
            } // optionalAttrs (system == "x86_64-darwin") {
            macos = lib.filterAttrs
              (n: _:
                # only build docker images once on linux:
                !(lib.hasPrefix "dockerImage" n))
              packages // {
              cardano-node-macos = import ./nix/binary-release.nix {
                inherit pkgs;
                inherit (exes.cardano-node.identifier) version;
                platform = "macos";
                exes = lib.collect lib.isDerivation (collectExes project);
              };
              shells = removeAttrs devShells [ "profiled" ] // {
                default = devShell;
              };
              internal = {
                roots.project = project.roots;
                plan-nix.project = project.plan-nix;
              };
            };
          };
        }
      );

      makeRequired = isPr: jobs: extra:
      let
        nonRequiredPaths = map lib.hasPrefix ([ "macos." ] ++ lib.optional isPr "linux.native.membenches");
      in with self.legacyPackages.${defaultSystem};
        releaseTools.aggregate {
          name = "github-required";
          meta.description = "All jobs required to pass CI";
          constituents = lib.collect lib.isDerivation
            (lib.mapAttrsRecursiveCond (v: !(lib.isDerivation v))
              (path: value:
                let stringPath = lib.concatStringsSep "." path; in if lib.isAttrs value && (lib.any (p: p stringPath) nonRequiredPaths) then { } else value)
              jobs) ++ extra;
        };

      makeOsRequired = isPr: jobs: {
        linux = jobs.linux // {
          required = makeRequired isPr jobs.linux [];
        };
        macos = jobs.macos // {
          required = makeRequired isPr jobs.macos [];
        };
      };

      hydraJobs =
        let
          jobs = lib.foldl' lib.mergeAttrs { } (lib.attrValues flake.systemHydraJobs);
        in
        jobs // (with self.legacyPackages.${defaultSystem}; rec {
          cardano-deployment = cardanoLib.mkConfigHtml { inherit (cardanoLib.environments) mainnet testnet; };
          build-version = writeText "version.json" (builtins.toJSON {
            inherit (self) lastModified lastModifiedDate narHash outPath shortRev rev;
          });
          required = makeRequired false jobs [ cardano-deployment build-version ];
        }) // makeOsRequired false jobs;

      hydraJobsPr =
        let
          nonPrJobs = map lib.hasPrefix [
            "linux.native.membenches"
            "linux.native.workbench-ci-analysis"
            "linux.native.workbench-ci-test"
          ];

          jobs = lib.mapAttrsRecursiveCond (v: !(lib.isDerivation v))
            (path: value:
              let stringPath = lib.concatStringsSep "." path; in if lib.isAttrs value && (lib.any (p: p stringPath) nonPrJobs) then { } else value)
            hydraJobs;
        in
        jobs // {
          required = makeRequired true jobs [ hydraJobs.cardano-deployment hydraJobs.build-version ];
        } // makeOsRequired true jobs;
    in
    builtins.removeAttrs flake [ "systemHydraJobs" ] // {

      inherit hydraJobs hydraJobsPr;

      overlay = final: prev: {
        cardanoNodeProject = flake.project.${final.system};
        cardanoNodePackages = mkCardanoNodePackages final.cardanoNodeProject;
        inherit (final.cardanoNodePackages) cardano-node cardano-cli cardano-submit-api bech32 plutus-example;

        # TODO, fix this
        #db-analyser = ouroboros-network-snapshot.haskellPackages.ouroboros-consensus-cardano.components.exes.db-analyser;
      };
      nixosModules = {
        cardano-node = { pkgs, lib, ... }: {
          imports = [ ./nix/nixos/cardano-node-service.nix ];
          services.cardano-node.cardanoNodePackages = lib.mkDefault (mkCardanoNodePackages flake.project.${pkgs.system});
        };
        cardano-submit-api = { pkgs, lib, ... }: {
          imports = [ ./nix/nixos/cardano-submit-api-service.nix ];
          services.cardano-submit-api.cardanoNodePackages = lib.mkDefault (mkCardanoNodePackages flake.project.${pkgs.system});
        };
      };
    };
}
