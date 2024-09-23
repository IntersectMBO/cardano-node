{ pkgs, lib
, workbenchNix
, profileName, profiling
}:

let
  profileJson =
    workbenchNix.runWorkbench "profile-${profileName}.json"
      "profile json ${profileName}"
  ;

  topologyFiles =
    pkgs.runCommand "workbench-topology-${profileName}"
      { requiredSystemFeatures = [ "benchmark" ];
        nativeBuildInputs = with pkgs.haskellPackages; with pkgs;
          [ bash cardano-cli coreutils gnused jq moreutils workbenchNix.workbench ];
      }
      ''
      mkdir $out
      wb topology make ${profileJson} $out
      ''
  ;

  nodeSpecsJson =
    workbenchNix.runWorkbench "node-specs-${profileName}.json"
                 "profile node-specs ${profileName} ${topologyFiles}"
  ;

  jsonFilePretty = name: x: workbenchNix.runJq name ''--null-input --sort-keys
                                         --argjson x '${x}'
                                       '' "$x";

  mkServices = { profile, nodeSpecs, backend }:
    rec {
      inherit
        (pkgs.callPackage
          ../service/nodes.nix
          {
            inherit backend profile nodeSpecs;
            inherit topologyFiles profiling;
            inherit workbenchNix;
            inherit jsonFilePretty;
            baseNodeConfig = workbenchNix.cardanoNodePackages.cardanoLib.environments.testnet.nodeConfig;
          })
        node-services;

      inherit
        (pkgs.callPackage
          ../service/generator.nix
          {
            inherit backend profile nodeSpecs;
            inherit node-services;
            inherit jsonFilePretty;
          })
        generator-service;

      inherit
        (pkgs.callPackage
          ../service/tracer.nix
          {
            inherit backend profile nodeSpecs;
            inherit jsonFilePretty;
          })
        tracer-service;

      inherit
        (pkgs.callPackage
          ../service/healthcheck.nix
          {
            inherit backend profile nodeSpecs;
          })
        healthcheck-service;

      inherit
        (pkgs.callPackage
          ../service/latency.nix
          {})
        latency-service;
    };

  materialise-profile =
    profileArgs@{ backend }:
      if ! builtins.elem profileName workbenchNix.profile-names
      then
        throw "No such profile: ${profileName}; Known profiles: ${toString workbenchNix.profile-names}"
      else
        let
          profile = __fromJSON (__readFile profileJson);
          nodeSpecs = __fromJSON (__readFile nodeSpecsJson);
          inherit (mkServices
            {
              inherit backend;
              inherit profile;
              inherit nodeSpecs;
            })
            node-services
            generator-service
            tracer-service
            healthcheck-service
            latency-service
          ;
        in
          pkgs.runCommand "workbench-profile-${profileName}"
            { buildInputs = [];
              profileJsonPath = profileJson;
              nodeSpecsJsonPath = nodeSpecsJson;
              topologyJsonPath = "${topologyFiles}/topology.json";
              topologyDotPath  = "${topologyFiles}/topology.dot";
              nodeServices =
                __toJSON
                (lib.flip lib.mapAttrs node-services
                  (name: node-service:
                    with node-service;
                    { inherit name;
                      start          = start.JSON;
                      config         = config.JSON;
                      topology       = topology.JSON;
                    }));
              generatorService =
                with generator-service;
                __toJSON
                { name            = "generator";
                  start           = start.JSON;
                  config          = config.JSON;
                  plutus-redeemer = plutus-redeemer.JSON;
                  plutus-datum    = plutus-datum.JSON;
                };
              tracerService =
                with tracer-service;
                __toJSON
                { name                 = "tracer";
                  start                = start.JSON;
                  config               = config.JSON;
                };
              healthcheckService =
                with healthcheck-service;
                __toJSON
                { name                 = "healthcheck";
                  start                = start.JSON;
                };
              latencyService =
                with healthcheck-service;
                __toJSON
                { name                 = "latency";
                  start                = start.JSON;
                };
              passAsFile =
                [
                  "nodeServices"
                  "generatorService"
                  "tracerService"
                  "healthcheckService"
                  "latencyService"
                  "topologyJson"
                  "topologyDot"
                ];
            }
            ''
            mkdir $out
            cp    $profileJsonPath              $out/profile.json
            cp    $nodeSpecsJsonPath            $out/node-specs.json
            cp    $topologyJsonPath             $out/topology.json
            cp    $topologyDotPath              $out/topology.dot
            cp    $nodeServicesPath             $out/node-services.json
            cp    $generatorServicePath         $out/generator-service.json
            cp    $tracerServicePath            $out/tracer-service.json
            cp    $healthcheckServicePath       $out/healthcheck-service.json
            cp    $latencyServicePath           $out/latency-service.json
            ''
          //
          (
            profile
            //
            {
              inherit profileName;
              JSON = profileJson;
              value = profile;
              topology = {
                files = topologyFiles;
                value = (__fromJSON (__readFile "${topologyFiles}/topology.json"));
              };
              node-specs = {JSON = nodeSpecsJson; value = nodeSpecs;};
              inherit node-services generator-service tracer-service healthcheck-service latency-service;
            }
          )
  ;

in {
  name = profileName;
  inherit profiling;
  inherit profileJson nodeSpecsJson;
  inherit materialise-profile;
}
