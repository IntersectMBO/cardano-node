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
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackageNix";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ops-lib = {
      url = "github:input-output-hk/ops-lib";
      flake = false;
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
      flake = false;
    };
    em = {
      url = "github:deepfire/em";
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

    empty-flake.url = "github:input-output-hk/empty-flake";

    cardano-mainnet-mirror.url = "github:input-output-hk/cardano-mainnet-mirror/nix";

    std.url = "github:divnix/std";

    nix2container.url = "github:nlewo/nix2container";

    cardano-automation = {
      url = "github:input-output-hk/cardano-automation";
      inputs = {
        haskellNix.follows = "haskellNix";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs =
    { self
    , nixpkgs
    , hostNixpkgs
    , utils
    , haskellNix
    , CHaP
    , iohkNix
    , ops-lib
    , cardano-mainnet-mirror
    , std
    , nix2container
    , cardano-automation
    , em
    , ...
    }@input:
    let
      inherit (nixpkgs) lib;
      inherit (lib) head mapAttrs recursiveUpdate optionalAttrs;
      inherit (utils.lib) eachSystem flattenTree;
      inherit (iohkNix.lib) prefixNamesWith;
      removeRecurse = lib.filterAttrsRecursive (n: _: n != "recurseForDerivations");

      supportedSystems = import ./nix/supported-systems.nix;
      defaultSystem = head supportedSystems;
      customConfig = recursiveUpdate
        (import ./nix/custom-config.nix customConfig)
        input.customConfig;

      overlays = [
        # crypto needs to come before haskell.nix.
        # FIXME: _THIS_IS_BAD_
        iohkNix.overlays.crypto
        haskellNix.overlay
        iohkNix.overlays.haskell-nix-extra
        iohkNix.overlays.haskell-nix-crypto
        iohkNix.overlays.cardano-lib
        iohkNix.overlays.utils
        (final: prev: {
          inherit customConfig nix2container;
          bench-data-publish = cardano-automation.outputs.packages.${final.system}."bench-data-publish:exe:bench-data-publish";
          em = import em { inherit (final) system;
                           nixpkgsSrcs = nixpkgs.outPath;
                           nixpkgsRev = nixpkgs.rev; };
          gitrev = final.customConfig.gitrev or self.rev or "0000000000000000000000000000000000000000";
          commonLib = lib
            // iohkNix.lib
            // final.cardanoLib
            // import ./nix/svclib.nix { inherit (final) pkgs; };
        })
        (import ./nix/pkgs.nix)
        self.overlay
      ] ++ (import ops-lib.outPath {}).overlays;

      collectExes = project:
        let set-git-rev = import ./nix/set-git-rev.nix { inherit (project) pkgs; };
        in
        # take all executables from the project local packages
        project.exes // (with project.hsPkgs; {
          # add some executables from other relevant packages
          inherit (bech32.components.exes) bech32;
          inherit (ouroboros-consensus-cardano.components.exes) db-analyser db-synthesizer db-truncater;
          # add cardano-node and cardano-cli with their git revision stamp
          cardano-node = set-git-rev project.exes.cardano-node;
          cardano-cli = set-git-rev cardano-cli.components.exes.cardano-cli;
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
        devShells =
        let
          shell = import ./shell.nix { inherit pkgs customConfig cardano-mainnet-mirror; };
        in {
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
          inherit (pkgs) checkCabalProject;
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
          // optionalAttrs (system == "x86_64-linux")
          (let workbenchTest =
                { profileName, workbenchStartArgs ? [] }:
                (pkgs.workbench-runner
                  {
                    inherit profileName workbenchStartArgs;
                    backendName = "supervisor";
                    useCabalRun = false;
                    cardano-node-rev = pkgs.gitrev;
                  }).workbench-profile-run;
          in
          {
            "dockerImage/node" = pkgs.dockerImage;
            "dockerImage/submit-api" = pkgs.submitApiDockerImage;

            ## This is a very light profile, no caching&pinning needed.
            workbench-ci-test =
              workbenchTest { profileName        = "ci-test-hydra-bage";
                              workbenchStartArgs = [ "--create-testnet-data" ];
                            };
            workbench-ci-test-trace =
              workbenchTest { profileName        = "ci-test-hydra-bage";
                              workbenchStartArgs = [ "--create-testnet-data" "--trace" ];
                            };

            inherit (pkgs) all-profiles-json;

            system-tests = pkgs.writeShellApplication {
              name = "system-tests";
              runtimeInputs = with pkgs; [ git gnused ];
              text = ''
                  NODE_REV="${self.rev or ""}"
                  if [[ -z $NODE_REV ]]; then
                    echo "Sorry, need clean/pushed git revision to run system tests"
                    exit 1;
                  fi
                  MAKE_TARGET=testpr
                  mkdir -p tmp && cd tmp
                  rm -rf cardano-node-tests
                  git clone https://github.com/intersectmbo/cardano-node-tests.git
                  cd cardano-node-tests
                  sed -i '1 s/^.*$/#! \/usr\/bin\/env bash/' ./.github/regression.sh
                  export NODE_REV
                  export MAKE_TARGET
                  nix develop --accept-flake-config .#base -c ./.github/regression.sh 2>&1
              '';
            };
          })
          # Add checks to be able to build them individually
          // (prefixNamesWith "checks/" checks);

        apps = lib.mapAttrs (n: p: { type = "app"; program = p.exePath or (if (p.executable or false) then "${p}" else "${p}/bin/${p.name or n}"); }) exes;

        ciJobs =
          let
            ciJobsVariants = mapAttrs (_: p:
              (mkFlakeAttrs (pkgs.extend (prev: final: { cardanoNodeProject = p; }))).ciJobs
            ) project.projectVariants;
            ciJobs = {
              cardano-deployment = pkgs.cardanoLib.mkConfigHtml { inherit (pkgs.cardanoLib.environments) mainnet preview preprod shelley_qa; };
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
                    exes = lib.collect lib.isDerivation (
                      # FIXME: restore tx-generator and gen-plutus once
                      #        plutus-scripts-bench is fixed for musl
                      #
                      # It stands to question though, whether or not we want those to be
                      # in the cardano-node-linux as executables anyway?
                      removeAttrs projectExes [ "tx-generator" "gen-plutus" ]
                    );
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
                variants = mapAttrs (_: v: removeAttrs v.native ["variants"]) ciJobsVariants;
              };
            };
            nonRequiredPaths = [
              #FIXME: cardano-tracer-test for windows should probably be disabled in haskell.nix config:
              "windows\\.(.*\\.)?checks\\.cardano-tracer\\.cardano-tracer-test"
              #FIXME: plutus-scripts-bench (dep of tx-generator) does not compile for windows:
              "windows\\.(.*\\.)?tx-generator.*"
              #FIXME: plutus-scripts-bench's gen-plutus does not compile for musl
              "musl\\.(.*\\.)?tx-generator.*"
              "musl\\.(.*\\.)?gen-plutus.*"
              # hlint required status is controled via the github action:
              "native\\.(.*\\.)?checks/hlint"
              #system-tests are build and run separately:
              "native\\.(.*\\.)?system-tests"
            ] ++
            lib.optionals (system == "x86_64-darwin") [
              #FIXME: make variants nonrequired for macos until CI has more capacity for macos builds
              "native\\.variants\\..*"
              "native\\.checks/cardano-testnet/cardano-testnet-test"
            ];
          in
          pkgs.callPackages iohkNix.utils.ciJobsAggregates
            {
              inherit ciJobs;
              nonRequiredPaths = map (r: p: builtins.match r p != null) nonRequiredPaths;
            } // ciJobs;
      };

      flake = eachSystem supportedSystems (system:
        let
          inherit (haskellNix) config;
          pkgs = import nixpkgs {
            inherit config system overlays;
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

        }
      );

    in
    removeAttrs flake [ "ciJobs" ] // {

      hydraJobs = flake.ciJobs // (let pkgs = self.legacyPackages.${defaultSystem}; in {
        inherit (pkgs.callPackages iohkNix.utils.ciJobsAggregates {
          ciJobs = lib.mapAttrs (_: lib.getAttr "required") flake.ciJobs // {
            # ensure hydra notify:
            gitrev = pkgs.writeText "gitrev" pkgs.gitrev;
          };
        }) required;
      });

      # allows precise paths (avoid fallbacks) with nix build/eval:
      outputs = self;

      overlay = final: prev: {
        cardanoNodeProject = (import ./nix/haskell.nix {
          inherit (final) haskell-nix;
          inherit (std) incl;
          inherit CHaP;
        }).appendModule [
          customConfig.haskellNix
        ];
        cardanoNodePackages = mkCardanoNodePackages final.cardanoNodeProject;
        inherit (final.cardanoNodePackages) cardano-node cardano-cli cardano-submit-api cardano-tracer bech32 locli db-analyser;
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
