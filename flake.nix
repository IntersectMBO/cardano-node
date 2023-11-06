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
      supportedSystems = import ./nix/supported-systems.nix;
      defaultSystem = nixpkgs.lib.head supportedSystems;
    in
      utils.lib.eachSystem supportedSystems (system:
        let
          customConfig = nixpkgs.lib.recursiveUpdate
            (import ./nix/custom-config.nix customConfig)
            input.customConfig;

          pkgs = import nixpkgs {
            inherit system;
            inherit (haskellNix) config;

            overlays = [
              iohkNix.overlays.crypto
              haskellNix.overlay
              iohkNix.overlays.haskell-nix-extra
              iohkNix.overlays.haskell-nix-crypto
              iohkNix.overlays.cardano-lib
              iohkNix.overlays.utils
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
            ] ++ (import ops-lib.outPath {}).overlays;
          };

          project = (import ./nix/haskell.nix {
            inherit (pkgs) haskell-nix;
            inherit (std) incl;
            inherit CHaP;
          }).appendModule [
            customConfig.haskellNix
          ];

          flake = project.flake {};
        in
          with pkgs; lib.recursiveUpdate (removeAttrs flake [ "ciJobs" ]) {
          } // {
            hydraJobs = flake.ciJobs;
          });

    }
