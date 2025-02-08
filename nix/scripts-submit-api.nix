{ pkgs
, customConfigs ? [ pkgs.customConfig ]
}:
with pkgs.commonLib;
let
  mkScript = envConfig: let
    service = evalService {
      inherit pkgs customConfigs;
      serviceName = "cardano-submit-api";
      modules = [
        ./nixos/cardano-submit-api-service.nix
        {
          services.cardano-submit-api = {
            network = mkDefault envConfig.name;
          };
        }
      ];
    };

  in pkgs.writeScriptBin "cardano-submit-api-${service.network}" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail
    ${service.script} $@
  '' // {
    passthru = { inherit service; };
  };

  # Until complete removal of some iohk-nix deprecated environments which have
  # dangling dependencies such as shelley_qa, explicitly declare the
  # environments we we want included.
  environments' = pkgs.lib.getAttrs [ "mainnet" "preprod" "preview" "sanchonet" ] environments;

  scripts = forEnvironmentsCustom (environment: recurseIntoAttrs {
    submit-api = mkScript environment;
  }) environments';
in scripts
