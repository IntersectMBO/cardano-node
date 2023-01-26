{
  description = "Cardano Node";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

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

    tullia.url = "github:input-output-hk/tullia";
    std.follows = "tullia/std";

    nix2container.url = "github:nlewo/nix2container";

    cardano-automation.url = "github:input-output-hk/cardano-automation";
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
    , tullia
    , std
    , nix2container
    , cardano-automation
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
          inherit customConfig nix2container;
          inherit (tullia.packages.${final.system}) tullia nix-systems;
          bench-data-publish = cardano-automation.outputs.packages.${final.system}."bench-data-publish:exe:bench-data-publish";
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

        in
        project.exes // (with project.hsPkgs; {
          inherit (ouroboros-consensus-byron.components.exes) db-converter;
          inherit (ouroboros-consensus-cardano-tools.components.exes) db-analyser db-synthesizer;
          inherit (bech32.components.exes) bech32;
        } // lib.optionalAttrs hostPlatform.isUnix {
          inherit (network-mux.components.exes) cardano-ping;
          inherit plutus-example;
        });

      mkCardanoNodePackages = project: (collectExes project) // {
        inherit (project.pkgs) cardanoLib;
      };

      mkFlakeAttrs = pkgs: rec {
        inherit (pkgs) system;
        inherit (pkgs.haskell-nix) haskellLib;
        inherit (haskellLib) collectChecks' collectComponents';
        inherit (pkgs.commonLib) eachEnv environments mkSupervisordCluster;
        inherit (pkgs.stdenv) hostPlatform;
        project = pkgs.cardanoNodeProject;

        # This is used by `nix develop .` to open a devShell
        devShells = let shell = import ./shell.nix { inherit pkgs customConfig cardano-mainnet-mirror; }; in {
          inherit (shell) devops workbench-shell;
          default = shell.dev;
          cluster = shell;
          profiled = project.profiled.shell;
        };

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

        # The parametrisable workbench.
        inherit (pkgs) workbench;

        packages =
          exes
          # Linux only packages:
          // optionalAttrs (system == "x86_64-linux") rec {
            "dockerImage/node" = pkgs.dockerImage;
            "dockerImage/submit-api" = pkgs.submitApiDockerImage;

            ## This is a very light profile, no caching&pinning needed.
            workbench-ci-test =
              (pkgs.workbench-runner
                {
                  profileName = "ci-test-bage";
                  backendName = "supervisor";
                  cardano-node-rev =
                    if __hasAttr "rev" self
                    then pkgs.gitrev
                    else throw "Cannot get git revision of 'cardano-node', unclean checkout?";
                }).profile-run { };

            inherit (pkgs) all-profiles-json;
          }
          # Add checks to be able to build them individually
          // (prefixNamesWith "checks/" checks);

        apps = lib.mapAttrs (n: p: { type = "app"; program = p.exePath or (if (p.executable or false) then "${p}" else "${p}/bin/${p.name or n}"); }) exes;

        ciJobs =
          let
            ciJobsVariants = mapAttrs (_: p:
              (mkFlakeAttrs (pkgs.extend (prev: final: { cardanoNodeProject = p; }))).ciJobs
            ) project.projectVariants;
            ciJobs = {
              cardano-deployment = pkgs.cardanoLib.mkConfigHtml { inherit (pkgs.cardanoLib.environments) mainnet testnet; };
            } // optionalAttrs (system == "x86_64-linux") {
              native = packages // {
                shells = devShells;
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
                variants = mapAttrs (_: v: removeAttrs v.native ["variants"]) ciJobsVariants;
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
                  variants = mapAttrs (_: v: removeAttrs v.musl ["variants"]) ciJobsVariants;
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
                    exes = lib.collect lib.isDerivation (
                      # FIXME: restore tx-generator once plutus-scripts-bench is fixed for windows:
                      removeAttrs projectExes [ "tx-generator" ]
                    );
                  };
                  internal.roots.project = windowsProject.roots;
                  variants = mapAttrs (_: v: removeAttrs v.windows ["variants"]) ciJobsVariants;
                });
            } // optionalAttrs (system == "x86_64-darwin") {
              native = lib.filterAttrs
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
                shells = removeAttrs devShells [ "profiled" ];
                internal = {
                  roots.project = project.roots;
                  plan-nix.project = project.plan-nix;
                };
                variants = mapAttrs (_: v: removeAttrs v.musl ["variants"]) ciJobsVariants;
              };
            };
            nonRequiredPaths = [
              # hlint required status is controled via the github action:
              "native\\.(.*\\.)?checks/hlint"
              #FIXME: cardano-tracer-test for windows should probably be disabled in haskell.nix config:
              "windows\\.(.*\\.)?checks\\.cardano-tracer\\.cardano-tracer-test"
              #FIXME: plutus-scripts-bench (dep of tx-generator) does not compile for windows:
              "windows\\.(.*\\.)?tx-generator.*"
            ] ++
            lib.optionals (system == "x86_64-darwin") [
              #FIXME:  ExceptionInLinkedThread (ThreadId 253) pokeSockAddr: path is too long
              "native\\.(.*\\.)?.checks/cardano-testnet/cardano-testnet-tests"
            ];
          in
          pkgs.callPackages iohkNix.utils.ciJobsAggregates
            {
              inherit ciJobs;
              nonRequiredPaths = map (r: p: builtins.match r p != null) nonRequiredPaths;
            } // ciJobs // {
            pr = {
              # We use a generic gitrev for PR CI to avoid unecessary rebuilds:
              inherit ((mkFlakeAttrs (pkgs.extend (prev: final: { gitrev = "0000000000000000000000000000000000000000"; }))).ciJobs)
                required nonrequired;
            };
          };
      };

      flake = eachSystem supportedSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          inherit (mkFlakeAttrs pkgs) environments packages checks apps project ciJobs devShells workbench;
        in
        {

          inherit environments checks project ciJobs devShells workbench;

          legacyPackages = pkgs // {
            # allows access to hydraJobs without specifying <arch>:
            hydraJobs = ciJobs;
          };

          packages = packages // {
            # Built by `nix build .`
            default = packages.cardano-node;
          };

          # Run by `nix run .`
          apps = apps // {
            default = apps.cardano-node;
          };

        } //
        tullia.fromSimple system (import ./nix/tullia.nix)
      );

    in
    removeAttrs flake [ "ciJobs" ] // {

      hydraJobs = flake.ciJobs;

      # allows precise paths (avoid fallbacks) with nix build/eval:
      outputs = self;

      overlay = final: prev: {
        cardanoNodeProject = (import ./nix/haskell.nix {
          inherit (final) haskell-nix;
          inherit (std) incl;
        }).appendModule [
          {
            inputMap = {
              "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
            };
          }
          customConfig.haskellNix
        ];
        cardanoNodePackages = mkCardanoNodePackages final.cardanoNodeProject;
        inherit (final.cardanoNodePackages) cardano-node cardano-cli cardano-submit-api cardano-tracer bech32 locli plutus-example db-analyser;
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
