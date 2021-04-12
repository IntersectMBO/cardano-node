{
  description = "Cardano Node";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, utils, haskellNix, iohkNix, ... }:
    let
      inherit (haskellNix.internal) config;
      inherit (nixpkgs) lib;
      inherit (lib) systems mapAttrs recursiveUpdate mkDefault optionalAttrs nameValuePair;
      inherit (utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) prefixNamesWith collectExes;

      overlays = with iohkNix.overlays; [
        haskellNix.overlay
        haskell-nix-extra
        crypto
        cardano-lib
        (final: prev: {
          customConfig = import ./custom-config.nix;
          workbenchConfig =  import ./workbench-config.nix;
          gitrev = self.rev or "dirty";
          commonLib = lib
            // iohkNix.lib
            // final.cardanoLib;
        })
        (import ./nix/pkgs.nix)
      ];

    in eachSystem (import ./supported-systems.nix) (system:
      let
        pkgs = import nixpkgs { inherit system overlays config; };

        inherit (pkgs.commonLib) eachEnv environnments;

        devShell = import ./shell.nix { inherit pkgs; };

        flake = pkgs.cardanoNodeProject.flake {};

        muslFlake = (import nixpkgs { inherit system overlays config;
          crossSystem = systems.examples.musl64;
        }).cardanoNodeProject.flake {};

        windowsFlake = (import nixpkgs { inherit system overlays config;
          crossSystem = systems.examples.mingwW64;
        }).cardanoNodeProject.flake {};

        configModule = conf:
          { pkgs, lib, ... }: {
            services.cardano-node = {
              stateDir = "/persist";
              socketPath = "/alloc/node.socket";
              enable = true;
              cardanoNodePkgs = lib.mkDefault pkgs;
              hostAddr = lib.mkDefault "0.0.0.0";
            } // conf;
          };

        evaluated = conf:
          lib.nixosSystem {
            inherit pkgs system;
            modules = [ ./nix/nixos/cardano-node-service.nix (configModule conf) ];
          };


        packages = let
          deps = with pkgs; [
            coreutils
            findutils
            gnugrep
            gnused
            postgresql
            strace
            lsof
            dnsutils
            bashInteractive
            iproute
            curl
            netcat
            bat
            tree
          ];

          vanilla = eachEnv (environment:
            nameValuePair "${environment}/node-entrypoint"
            (pkgs.writeShellScriptBin "cardano-node-entrypoint"
              (evaluated { inherit environment; }).config.services.cardano-node.script));

          debug = eachEnv (env:
            let
              closure = pkgs.symlinkJoin {
                name = "cardano-node-entrypoint";
                paths = [ vanilla."cardano-node-${env}" ] ++ deps;
              };
            in nameValuePair "${env}/node-entrypoint-debug" closure);

        in debug // vanilla // {
          inherit (devShell) devops;
        } // (collectExes flake.packages)
          # Linux only packages:
          // optionalAttrs (system == "x86_64-linux") (
            prefixNamesWith "windows/" (collectExes windowsFlake.packages)
            // (prefixNamesWith "static/" (collectExes muslFlake.packages))
            // {
              "dockerImage/node" = pkgs.dockerImage;
              "dockerImage/submit-api" = pkgs.submitApiDockerImage;
            }
          );

      in recursiveUpdate flake {

        inherit evaluated environnments packages;

        legacyPackages = pkgs;

        checks = # Linux only checks:
          optionalAttrs (system == "x86_64-linux") (
            prefixNamesWith "windows/" windowsFlake.checks
            // (prefixNamesWith "nixosTests/" (mapAttrs (_: v: v.${system} or v) pkgs.nixosTests))
          );

        # Built by `nix build .`
        defaultPackage = flake.packages."cardano-node:exe:cardano-node";

        # Run by `nix run .`
        defaultApp = flake.apps."cardano-node:exe:cardano-node";

        # This is used by `nix develop .` to open a devShell
        inherit devShell;

        apps = {
          repl = mkApp {
            drv = pkgs.writeShellScriptBin "repl" ''
              confnix=$(mktemp)
              echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
              trap "rm $confnix" EXIT
              nix repl $confnix
          '';
          };
        }
        # nix run .#<env>/node
        // (mapAttrs
          (_: drv: (mkApp {inherit drv; exePath = "";}))
          (flattenTree pkgs.scripts))
        # nix run .#<env>/node-entrypoint
        // (eachEnv (env: lib.nameValuePair "${env}/node-entrypoint" (utils.lib.mkApp {
            drv = packages."${env}/node-entrypoint";
            exePath = "/bin/cardano-node-entrypoint";
          })))
        # nix run .#<exe>
        // (collectExes flake.apps);

      }
    ) // {
      overlay = import ./overlay.nix self;
      nixosModules = {
        cardano-node = { pkgs, lib, ... }: {
          imports = [ ./nix/nixos/cardano-node-service.nix ];
          services.cardano-node.cardanoNodePkgs = lib.mkDefault self.legacyPackages.${pkgs.system};
        };
        cardano-submit-api = { pkgs, lib, ... }: {
          imports = [ ./nix/nixos/cardano-submit-api-service.nix ];
          services.cardano-submit-api.cardanoNodePkgs = lib.mkDefault self.legacyPackages.${pkgs.system};
        };
      };
    };
}
