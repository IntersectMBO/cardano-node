{ pkgs
, customConfigs ? [ pkgs.customConfig ]
}:
with pkgs.commonLib;
let
  mkScript = envConfig: let
    service = evalService {
      inherit pkgs customConfigs;
      serviceName = "cardano-node";
      modules = [
        ./nixos/cardano-node-service.nix
        ({config, ...}: {
          services.cardano-node = let cfg = config.services.cardano-node; in {
            hostAddr = mkDefault "0.0.0.0";
            environment = mkDefault envConfig.name;
            nodeConfig = cfg.environments.${cfg.environment}.nodeConfig;
            stateDir = mkDefault "state-node-${cfg.environment}";
            runtimeDir = mkDefault null;
          } // optionalAttrs (envConfig ? topology) {
            topology = mkDefault envConfig.topology;
          };
        })
      ];
    };
  scriptBin = pkgs.writeScriptBin "cardano-node-${service.environment}" ''
    #!${pkgs.runtimeShell}
    export PATH=$PATH:${makeBinPath [ pkgs.coreutils ]}
    set -euo pipefail
    mkdir -p "$(dirname "${service.socketPath 0}")"
    ${service.script} $@
  '';
  in scriptBin // {
    exePath = "${scriptBin}/bin/cardano-node-${service.environment}";
  };

  # Until complete removal of some iohk-nix deprecated environments which have
  # dangling dependencies such as shelley_qa, explicitly declare the
  # environments we we want included.
  environments' = pkgs.lib.getAttrs [ "mainnet" "preprod" "preview" ] environments;

in forEnvironmentsCustom (environment: recurseIntoAttrs rec {
  node = mkScript environment;
}) environments'
