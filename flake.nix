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
    # Used to create OCI/Docker images for the workbench.
    # It's a parameter to nix/workbench/default.nix
    cardano-world.url = "github:input-output-hk/cardano-world";
  };

  outputs =
    { self
    , nixpkgs
    , hostNixpkgs
    , utils
    , haskellNix
    , iohkNix
    , plutus-apps
    , cardano-mainnet-mirror
    , node-snapshot
    , node-measured
    , node-process
    , cardano-node-workbench
    , cardano-world
    , ...
    }@input:
    let
      inherit (nixpkgs) lib;
      inherit (lib) head systems mapAttrs recursiveUpdate mkDefault
        getAttrs optionalAttrs nameValuePair attrNames;
      inherit (utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) prefixNamesWith;
      removeRecurse = lib.filterAttrsRecursive (n: _: n != "recurseForDerivations");
      flatten = attrs: lib.foldl' (acc: a: if (lib.isAttrs a) then acc // (removeAttrs a [ "recurseForDerivations" ]) else acc) { } (lib.attrValues attrs);

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
          inherit customConfig cardano-world;
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

      projectPackagesExes = import ./nix/project-packages-exes.nix;

      mkPackages = project:
        let
          inherit (project.pkgs.stdenv) hostPlatform;
          inherit (project.pkgs.haskell-nix) haskellLib;
          profiledProject = project.appendModule {
            modules = [{
              enableLibraryProfiling = true;
              packages.cardano-node.components.exes.cardano-node.enableProfiling = true;
              packages.tx-generator.components.exes.tx-generator.enableProfiling = true;
              packages.locli.components.exes.locli.enableProfiling = true;
            }];
          };
          assertedProject = project.appendModule {
            modules = [{
              packages = lib.genAttrs [
                "ouroboros-consensus"
                "ouroboros-consensus-cardano"
                "ouroboros-consensus-byron"
                "ouroboros-consensus-shelley"
                "ouroboros-network"
                "network-mux"
              ]
                (name: { flags.asserts = true; });
            }];
          };
          eventloggedProject = project.appendModule
            {
              modules = [{
                packages = lib.genAttrs [ "cardano-node" ]
                  (name: { configureFlags = [ "--ghc-option=-eventlog" ]; });
              }];
            };
          inherit ((import plutus-apps {
            inherit (project.pkgs) system;
          }).plutus-apps.haskell.packages.plutus-example.components.exes) plutus-example;
          pinned-workbench =
            cardano-node-workbench.workbench.${project.pkgs.system};
          hsPkgsWithPassthru = lib.mapAttrsRecursiveCond (v: !(lib.isDerivation v))
            (path: value:
              if (lib.isAttrs value) then
                lib.recursiveUpdate value
                  {
                    passthru = {
                      profiled = lib.getAttrFromPath path profiledProject.hsPkgs;
                      asserted = lib.getAttrFromPath path assertedProject.hsPkgs;
                      eventlogged = lib.getAttrFromPath path eventloggedProject.hsPkgs;
                    };
                  } else value)
            project.hsPkgs;
          projectPackages = lib.mapAttrs (n: _: hsPkgsWithPassthru.${n}) projectPackagesExes;
        in
        {
          inherit projectPackages profiledProject assertedProject eventloggedProject;
          inherit pinned-workbench;
          projectExes = flatten (haskellLib.collectComponents' "exes" projectPackages) // (with hsPkgsWithPassthru; {
            inherit (ouroboros-consensus-byron.components.exes) db-converter;
            inherit (ouroboros-consensus-cardano.components.exes) db-analyser;
            inherit (bech32.components.exes) bech32;
          } // lib.optionalAttrs hostPlatform.isUnix {
            inherit (network-mux.components.exes) cardano-ping;
            inherit plutus-example;
          });
        };

      mkCardanoNodePackages = project: (mkPackages project).projectExes // {
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
            inherit projectPackagesExes;
          }).appendModule customConfig.haskellNix // {
            profiled = profiledProject;
            asserted = assertedProject;
            eventlogged = eventloggedProject;
          };

          inherit (mkPackages project) projectPackages projectExes profiledProject assertedProject eventloggedProject pinned-workbench;

          shell = import ./shell.nix { inherit pkgs customConfig cardano-mainnet-mirror; };
          devShells = {
            inherit (shell) devops workbench-shell;
            cluster = shell;
            profiled = project.profiled.shell;
          };

          devShell = shell.dev;

          # NixOS tests run a node and submit-api and validate it listens
          nixosTests = import ./nix/nixos/tests {
            inherit pkgs;
          };

          checks = flattenTree (collectChecks' projectPackages) //
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

          exes = projectExes // {
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
            tests = collectComponents' "tests" projectPackages;
            # `benchmarks` (only built, not run).
            benchmarks = collectComponents' "benchmarks" projectPackages;
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

              ## This is a very light profile, no caching&pinning needed.
              workbench-ci-test =
                (pkgs.supervisord-workbench-for-profile
                  {
                    # inherit supervisord-workbench; ## Not required, as long as it's fast.
                    profileName = "ci-test-bage";
                    cardano-node-rev =
                      if __hasAttr "rev" self
                      then self.rev
                      else throw "Cannot get git revision of 'cardano-node', unclean checkout?";
                  }).profile-run {};

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
                    inherit (mkPackages muslProject) projectPackages projectExes;
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
                    inherit (mkPackages windowsProject) projectPackages projectExes;
                  in
                  projectExes
                    // (removeRecurse {
                    checks = collectChecks' projectPackages;
                    tests = collectComponents' "tests" projectPackages;
                    benchmarks = collectComponents' "benchmarks" projectPackages;
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
                exes = lib.collect lib.isDerivation projectExes;
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

      makeRequired = isPr: extra:
      let
        jobs = lib.foldl' lib.mergeAttrs { } (lib.attrValues flake.systemHydraJobs);
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


      hydraJobs =
        let
          jobs = lib.foldl' lib.mergeAttrs { } (lib.attrValues flake.systemHydraJobs);
        in
        jobs // (with self.legacyPackages.${defaultSystem}; rec {
          cardano-deployment = cardanoLib.mkConfigHtml { inherit (cardanoLib.environments) mainnet testnet; };
          build-version = writeText "version.json" (builtins.toJSON {
            inherit (self) lastModified lastModifiedDate narHash outPath shortRev rev;
          });
          required = makeRequired false [ cardano-deployment build-version ];
        });

      hydraJobsPr =
        let
          nonPrJobs = map lib.hasPrefix [
            "linux.native.membenches"
          ];
        in
        (lib.mapAttrsRecursiveCond (v: !(lib.isDerivation v))
          (path: value:
            let stringPath = lib.concatStringsSep "." path; in if lib.isAttrs value && (lib.any (p: p stringPath) nonPrJobs) then { } else value)
          hydraJobs) // {
            required = makeRequired true [ hydraJobs.cardano-deployment hydraJobs.build-version ];
          };

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
