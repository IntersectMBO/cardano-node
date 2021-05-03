{
  description = "Cardano Node";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix/flakes-improvements";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    customConfig = {
      url = "path:./custom-config";
    };
  };

  outputs = { self, nixpkgs, utils, haskellNix, iohkNix, customConfig }:
    let
      inherit (haskellNix.internal) config;
      inherit (nixpkgs) lib;
      inherit (lib) head systems mapAttrs recursiveUpdate mkDefault optionalAttrs nameValuePair;
      inherit (utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) prefixNamesWith collectExes;

      supportedSystems = import ./supported-systems.nix;
      defaultSystem = head supportedSystems;

      overlays = [
        haskellNix.overlay
        iohkNix.overlays.haskell-nix-extra
        iohkNix.overlays.crypto
        iohkNix.overlays.cardano-lib
        iohkNix.overlays.utils
        (final: prev: {
          customConfig = import ./custom-config // customConfig.outputs;
          gitrev = self.rev or "dirty";
          commonLib = lib
            // iohkNix.lib
            // final.cardanoLib;
        })
        (import ./nix/pkgs.nix)
      ];

    in eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs { inherit system overlays config; };

        inherit (pkgs.commonLib) eachEnv environments;

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
        // (prefixNamesWith "static/"
              (mapAttrs pkgs.rewriteStatic (collectExes
                (if system == "x86_64-darwin" then flake else muslFlake).packages)))
          # Linux only packages:
          // optionalAttrs (system == "x86_64-linux") (
            prefixNamesWith "windows/" (collectExes windowsFlake.packages)
            // {
              "dockerImage/node" = pkgs.dockerImage;
              "dockerImage/submit-api" = pkgs.submitApiDockerImage;
            }
          );

      in recursiveUpdate flake {

        inherit evaluated environments packages;

        legacyPackages = pkgs;

        checks = # Linux only checks:
          optionalAttrs (system == "x86_64-linux") (
            prefixNamesWith "windows/" windowsFlake.checks
            // (prefixNamesWith "nixosTests/" (mapAttrs (_: v: v.${system} or v) pkgs.nixosTests))
             # checks run on default system only;
          ) // optionalAttrs (system == defaultSystem) {
            hlint = pkgs.callPackage pkgs.hlintCheck {
              #inherit (pkgs.cardanoNodeProject.projectModule) src;
              src = ./.;
            };
          };

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
        // (mapAttrs (_: drv: (mkApp {inherit drv;})) (flattenTree pkgs.scripts))
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
