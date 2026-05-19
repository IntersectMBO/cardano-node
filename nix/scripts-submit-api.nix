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

  # Allow list envs we specifically want scripts for as others in iohk-nix may
  # be transient test networks.
  environments' = pkgs.lib.getAttrs [ "mainnet" "preprod" "preview" ] environments;

  scripts = forEnvironmentsCustom (environment: recurseIntoAttrs {
    submit-api = mkScript environment;
  }) environments';
in scripts
