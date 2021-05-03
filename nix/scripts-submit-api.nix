{ pkgs
, customConfig
}:
with pkgs.commonLib;
let
  mkScript = envConfig: let
    service = evalService {
      inherit pkgs customConfig;
      serviceName = "cardano-submit-api";
      modules = [
        ./nixos/cardano-submit-api-service.nix
        {
          services.cardano-submit-api = {
            network = lib.mkDefault envConfig.name;
            cardanoNodePkgs = lib.mkDefault pkgs;
          };
        }
      ];
    };

  in pkgs.writeScript "cardano-submit-api-${service.network}" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail
    ${service.script} $@
  '';

  scripts = forEnvironments (environment: recurseIntoAttrs {
    submit-api = mkScript environment;
  });
in removeAttrs scripts ["selfnode" "shelley_selfnode"]
