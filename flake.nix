{
  description = "Cardano Node";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, haskell-nix, ... }:
    (utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        legacyPackages = import ./nix {
          inherit sources system nixpkgs iohkNix;
          haskellNix = haskell-nix.legacyPackages.${system};
          gitrev = self.rev or "dirty";
        };
        lib = nixpkgs.lib;
        sources = import ./nix/sources.nix { pkgs = legacyPackages; };
        iohkNix = import sources.iohk-nix { inherit system; };
        environments = iohkNix.cardanoLib.environments;
        environmentName = "testnet";

        eachEnv = lib.flip lib.pipe [
          (lib.forEach (builtins.attrNames environments))
          lib.listToAttrs
        ];

        config = env:
          { pkgs, ... }: {
            services.cardano-node = rec {
              stateDir = "/persist";
              socketPath = "/alloc/node.socket";
              enable = true;
              package = pkgs.cardano-node;
              environment = env;
              cardanoNodePkgs = pkgs;
              hostAddr = "0.0.0.0";
              port = 3001;
            };
          };

        evaluated = env:
          lib.nixosSystem {
            inherit system;
            pkgs = legacyPackages;
            modules = [ ./nix/nixos/cardano-node-service.nix (config env) ];
          };

        packages = let
          deps = with legacyPackages; [
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

          vanilla = eachEnv (env:
            lib.nameValuePair "cardano-node-${env}"
            (legacyPackages.writeShellScriptBin "cardano-node-entrypoint" ''
              ${(evaluated env).config.services.cardano-node.script}
            ''));

          debug = eachEnv (env:
            let
              entrypoint =
                legacyPackages.writeShellScriptBin "cardano-node-entrypoint" ''
                  ${(evaluated env).config.services.cardano-node.script}
                '';

              closure = legacyPackages.symlinkJoin {
                name = "cardano-node-entrypoint";
                paths = [ entrypoint ] ++ deps;
              };
            in lib.nameValuePair "cardano-node-${env}-debug" closure);
        in debug // vanilla // {
          cardano-cli = legacyPackages.cardano-cli;
        };
      in {
        inherit iohkNix environments evaluated legacyPackages packages;

        apps = eachEnv (env:
          lib.nameValuePair "cardano-node-${env}" (utils.lib.mkApp {
            drv = packages."cardano-node-${env}";
            exePath = "/bin/cardano-node-entrypoint";
          }));
        })) // {
          overlay = import ./overlay.nix self;
          nixosModules = {
            cardano-node = { imports = [ ./nix/nixos/cardano-node-service.nix ]; };
          };
        };
}
