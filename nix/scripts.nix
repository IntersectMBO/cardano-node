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
          services.cardano-node = {
            environment = mkDefault envConfig.name;
            cardanoNodePkgs = mkDefault pkgs;
            stateDir = mkDefault "state-node-${config.services.cardano-node.environment}";
            runtimeDir = mkDefault null;
          } // optionalAttrs (envConfig ? topology) {
            topology = lib.mkDefault envConfig.topology;
          };
        })
      ];
    };

  in pkgs.writeScriptBin "cardano-node-${service.environment}" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail
    ${service.script} $@
  '';

in forEnvironments (environment: recurseIntoAttrs {
  node = mkScript environment;
})
