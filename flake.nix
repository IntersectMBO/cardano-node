{
  description = "Cardano Node";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
    experimental-features = [ "nix-command" "flakes" "fetch-closure" ];
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
      supportedSystems = import ./nix/supported-systems.nix;
      defaultSystem = nixpkgs.lib.head supportedSystems;

      mkCardanoNodePackages = project:
        let
          set-git-rev = import ./nix/set-git-rev.nix { inherit (project) pkgs; };
        in
          with project.hsPkgs; project.exes // {
            inherit (project.pkgs) cardanoLib;
            # add some executables from other relevant packages
            inherit (bech32.components.exes) bech32;
            inherit (ouroboros-consensus-cardano.components.exes)
              db-analyser db-synthesizer db-truncater;
            # add cardano-node and cardano-cli with their git revision stamp
            cardano-node = set-git-rev project.exes.cardano-node;
            cardano-cli = set-git-rev cardano-cli.components.exes.cardano-cli;
          };
    in
      utils.lib.eachSystem supportedSystems (system:
        let
          customConfig = nixpkgs.lib.recursiveUpdate
            (import ./nix/custom-config.nix customConfig)
            input.customConfig;

          pkgs = import nixpkgs {
            inherit system;
            inherit (haskellNix) config;

            overlays =
              builtins.attrValues iohkNix.overlays ++ [
                haskellNix.overlay

                (final: prev: {
                  inherit customConfig nix2container;
                  bench-data-publish = cardano-automation.outputs.packages.${final.system}."bench-data-publish:exe:bench-data-publish";
                  em = import em {
                    inherit (final) system;
                    nixpkgsSrcs = nixpkgs.outPath;
                    nixpkgsRev = nixpkgs.rev;
                  };
                  gitrev = final.customConfig.gitrev or self.rev or "0000000000000000000000000000000000000000";
                  commonLib = nixpkgs.lib
                    // iohkNix.lib
                    // final.cardanoLib
                    // import ./nix/svclib.nix { inherit (final) pkgs; };
                })

                (import ./nix/pkgs.nix)

                (final: prev:
                  let
                    cardanoNodePackages = mkCardanoNodePackages project;
                  in {
                    inherit cardanoNodePackages;
                    inherit (cardanoNodePackages) db-analyser;

                    cardanoNodeProject = project;
                })
            ] ++ (import ops-lib.outPath {}).overlays;
          };

          mkBinaryRelease = project: platform:
            let
              exes = mkCardanoNodePackages project;
            in
              with pkgs.lib; import ./nix/binary-release.nix {
                inherit pkgs platform;
                inherit (exes.cardano-node.identifier) version;
                exes = collect isDerivation exes;
              };

          # Profiled and Asserted exe variants
          extraExeVariants =
            let
              mkExes = prefix: project:
                pkgs.lib.pipe (with project.exes; [cardano-node tx-generator locli]) [
                  (builtins.map
                    (exe: pkgs.lib.nameValuePair exe.identifier.component-id exe))
                  builtins.listToAttrs
                  (iohkNix.lib.prefixNamesWith "${prefix}-")
                ];
            in
              mkExes "profiled" project.profiled //
              mkExes "asserted" project.asserted //
              pkgs.lib.concatMapAttrs
                (name: project:
                  mkExes "${name}-profiled" project.profiled //
                  mkExes "${name}-profiled" project.asserted)
                project.projectVariants;

          project = (import ./nix/haskell.nix {
            inherit (pkgs) haskell-nix;
            inherit (std) incl;
            inherit CHaP;
          }).appendModule [
            customConfig.haskellNix
          ];

          nonRequiredPaths = [
          ];

          flake = project.flake (
            pkgs.lib.optionalAttrs (system == "x86_64-linux") {
              crossPlatforms = p: [ p.musl64 p.mingwW64 ];
            }
          );

          hydraJobs = with pkgs.lib; recursiveUpdate flake.hydraJobs {
            checks = {
              inherit hlint;
            };

            # ensure hydra notify
            gitrev = pkgs.writeText "gitrev" pkgs.gitrev;
          } // optionalAttrs (system == "x86_64-linux") ({
            inherit cardano-deployment cardano-node-linux cardano-node-win64;
            inherit (pkgs) all-profiles-json;
          } // nixosChecks // extraExeVariants) // optionalAttrs (system == "x86_64-darwin") {
            inherit cardano-node-macos;
          };

          # Generate NixOS tests for each compiler version
          nixosChecks =
            let
              # Create an attrset of NixOS tests with the specified prefix-nixosTests-
              # prefix
              mkNixosChecks = prefix: pkgs':
                pkgs.lib.pipe pkgs' [
                  (p: import ./nix/nixos/tests { pkgs = p; })
                  (pkgs.lib.mapAttrs (_: v: v.${system} or v))
                  (iohkNix.lib.prefixNamesWith "${prefix}nixosTests-")
                ];
              # Override cardanoNodePackages with the variants' exes
              mkPkgs = project: project.pkgs.extend (_: _: {
                cardanoNodePackages = mkCardanoNodePackages project;
              });
            in
              # Generate nixosTests-xyzTest with the default compiler (ghc8107)
              mkNixosChecks "" pkgs //
              # Generate ghcXYZ-nixosTests-xyzTest for each compiler
              pkgs.lib.concatMapAttrs
                (name: project: mkNixosChecks "${name}-" (mkPkgs project))
                project.projectVariants;

          cardano-deployment = pkgs.cardanoLib.mkConfigHtml {
            inherit (pkgs.cardanoLib.environments) mainnet preview preprod;
          };

          cardano-node-linux = mkBinaryRelease project.projectCross.musl64 "linux";
          cardano-node-win64 = mkBinaryRelease project.projectCross.mingwW64 "win64";
          cardano-node-macos = mkBinaryRelease project "macos";
          hlint = pkgs.callPackage pkgs.hlintCheck {
            inherit (project.args) src;
          };

        in
          with pkgs; lib.recursiveUpdate (removeAttrs flake [ "ciJobs" ]) (rec {
            inherit project;
            apps.default = flake.apps."cardano-node:exe:cardano-node";

            checks = {
              inherit hlint;
            } // lib.optionalAttrs (system == "x86_64-linux") nixosChecks;

            devShells = {
              profiled = project.profiled.shell;
            };

            packages = {
              inherit cardano-deployment;
              inherit (pkgs) all-profiles-json;

              default = flake.packages."cardano-node:exe:cardano-node";
            } // lib.optionalAttrs (system == "x86_64-linux") {
              inherit cardano-node-linux cardano-node-win64;
            } // lib.optionalAttrs (system == "x86_64-macos") {
              inherit cardano-node-macos;
            } // extraExeVariants;
          }) // {
            # Completele replace hydraJobs
            hydraJobs = callPackages iohkNix.utils.ciJobsAggregates {
              ciJobs = hydraJobs;
              nonRequiredPaths = map (r: p: builtins.match r p != null) nonRequiredPaths;
            } // hydraJobs;
          }) // {
            # NixOS modules are not system specific
            nixosModules = {
              cardano-node = { pkgs, lib, ... }: {
                imports = [ ./nix/nixos/cardano-node-service.nix ];
                services.cardano-node.cardanoNodePackages =
                  lib.mkDefault (mkCardanoNodePackages self.project.${pkgs.system});
              };

              cardano-submit-api = { pkgs, lib, ... }: {
                imports = [ ./nix/nixos/cardano-submit-api-service.nix ];
                services.cardano-submit-api.cardanoNodePackages =
                  lib.mkDefault (mkCardanoNodePackages self.project.${pkgs.system});
              };
            };
          };
}
