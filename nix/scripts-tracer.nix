{ pkgs
, customConfigs ? [ pkgs.customConfig ]
}:
with pkgs.commonLib;
let
  mkScript = envConfig: let
    service = evalService {
      inherit pkgs customConfigs;
      serviceName = "cardano-tracer";
      modules = [
        ./nixos/cardano-tracer-service.nix
        ({config, ...}: let cfg = config.services.cardano-tracer; in {
          services.cardano-tracer = rec {
            environment = mkDefault envConfig.name;
            stateDir = mkDefault "state-tracer-${cfg.environment}";
            runtimeDir = mkDefault null;
            logging = mkDefault [
              {
                logRoot = stateDir;
                logMode = "FileMode";
                logFormat = "ForHuman";
              }
            ];
          };
        })
      ];
    };
  scriptBin = pkgs.writeScriptBin "cardano-tracer-${service.environment}" ''
    #!${pkgs.runtimeShell}
    export PATH=$PATH:${makeBinPath [ pkgs.coreutils ]}
    set -euo pipefail
    mkdir -p "$(dirname "${service.acceptingSocket or service.connectToSocket}")"
    ${service.script} $@
  '';
  in scriptBin // {
    exePath = "${scriptBin}/bin/cardano-tracer-${service.environment}";
  };

  # Allow list envs we specifically want scripts for as others in iohk-nix may
  # be transient test networks.
  environments' = pkgs.lib.getAttrs [ "mainnet" "preprod" "preview" ] environments;

in forEnvironmentsCustom (environment: recurseIntoAttrs {
  tracer = mkScript environment;
}) environments'
