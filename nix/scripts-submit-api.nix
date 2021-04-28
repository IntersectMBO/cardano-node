{ pkgs
, customConfig
 }:
with pkgs.commonLib;
let
  customConfig' = customConfig // (customConfig.submit-api or {});
  mkScript = envConfig: let

    configModule = { options, config, ...}: {
      services.cardano-submit-api = recursiveUpdate {
        enable = true;
        network = envConfig.name;
        environment = envConfig;
        cardanoNodePkgs = pkgs;
      } (let validOptions = attrNames options.services.cardano-submit-api;
        in filterAttrs (n: _: elem n validOptions) customConfig');
    };

    serviceScript = (modules.evalModules {
      prefix = [];
      modules = [
        ./nixos/cardano-submit-api-service.nix
        systemdCompatModule
        configModule
      ];
      args = { inherit pkgs; };
      check = false;
    }).config.services.cardano-submit-api.script;

  in pkgs.writeScript "cardano-submit-api-${envConfig.name}" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail
    ${serviceScript} $@
  '';

  scripts = forEnvironments (environment: recurseIntoAttrs {
    submit-api = mkScript environment;
  });
in removeAttrs scripts ["selfnode" "shelley_selfnode"]
