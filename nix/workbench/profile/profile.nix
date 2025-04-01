{ pkgs, lib
, workbenchNix
, profileName, profiling
}:

let

  # All top-level profile files in one derivation.
  ##############################################################################

  inherit
    (let
      profileDerivedFiles =
        pkgs.runCommand "workbench-profile-files-${profileName}"
          { nativeBuildInputs = with pkgs.haskellPackages; with pkgs;
            [ bash coreutils gnused jq moreutils workbenchNix.workbench ];
          }
          ''
            mkdir "$out"
            wb profile json "${profileName}" > "$out"/profile.json
            wb topology make "$out"/profile.json "$out"
            wb profile node-specs  \
              "$out"/profile.json  \
              "$out/topology.json" \
          > "$out"/node-specs.json
          ''
      ;
     in
      { profileJson      = "${profileDerivedFiles}/profile.json";
        topologyJsonPath = "${profileDerivedFiles}/topology.json";
        topologyDotPath  = "${profileDerivedFiles}/topology.dot";
        nodeSpecsJson    = "${profileDerivedFiles}/node-specs.json";
      }
    )
    profileJson
    topologyJsonPath
    topologyDotPath
    nodeSpecsJson
  ;

  ## This ports the (very minimal) config of the deprecated iohk-nix testnet environment to workbench, removing the dependency on it.
  baseNodeConfigTestnet =
    {
      Protocol              = "Cardano";
      RequiresNetworkMagic  = "RequiresMagic";

      LastKnownBlockVersion-Major = 3;
      LastKnownBlockVersion-Minor = 0;
      LastKnownBlockVersion-Alt   = 0;
    }
    // workbenchNix.cardanoNodePackages.cardanoLib.defaultLogConfig;

  mkServices = { profile, nodeSpecs, backend }:
    rec {
      inherit
        (pkgs.callPackage
          ../service/nodes.nix
          {
            inherit backend profile nodeSpecs;
            inherit profiling;
            inherit profileJson topologyJsonPath;
            inherit workbenchNix;
            baseNodeConfig = baseNodeConfigTestnet;
          })
        node-services;

      inherit
        (pkgs.callPackage
          ../service/generator.nix
          {
            inherit backend profile nodeSpecs;
            inherit node-services;
          })
        generator-service;

      workloads-service = builtins.map (workload: rec {
        name = workload.name;
        start =
          ''
          ${import ../workload/${name}.nix
                  {inherit pkgs profile nodeSpecs workload;}
          }
          ${workload.entrypoints.producers}
          ''
        ;
      }) profile.workloads;

      inherit
        (pkgs.callPackage
          ../service/tracer.nix
          {inherit backend profile nodeSpecs;}
        )
        tracer-service;

      inherit
        (pkgs.callPackage
          ../service/healthcheck.nix
          {
            inherit backend profile nodeSpecs;
          })
        healthcheck-service;
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
            workloads-service
            tracer-service
            healthcheck-service
          ;
        in
          pkgs.runCommand "workbench-profile-${profileName}"
            { buildInputs = [];
              profileJsonPath = profileJson;
              nodeSpecsJsonPath = nodeSpecsJson;
              inherit topologyJsonPath topologyDotPath;
              nodeServices = __toJSON
                (lib.flip lib.mapAttrs node-services
                  (name: node-service:
                    { inherit name;
                      inherit (node-service) start config;
                      topology = node-service.topology.JSON;
                    }
                  )
                )
              ;
              generatorService = __toJSON
                { name            = "generator";
                  inherit (generator-service) start config;
                  # Not present on every profile. Can be null.
                  inherit (generator-service) plutus-redeemer;
                  # Not present on every profile. Can be null.
                  inherit (generator-service) plutus-datum;
                }
              ;
              workloadsService = __toJSON (builtins.map (workload:
                { inherit (workload) name start;
                }
              ) workloads-service);
              tracerService = __toJSON
                { name = "tracer";
                  inherit (tracer-service) start config;
                }
              ;
              healthcheckService = __toJSON
                { name = "healthcheck";
                  inherit (healthcheck-service) start;
                }
              ;
              passAsFile =
                [
                  "nodeServices"
                  "generatorService"
                  "workloadsService"
                  "tracerService"
                  "healthcheckService"
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
            cp    $workloadsServicePath         $out/workloads-service.json
            cp    $tracerServicePath            $out/tracer-service.json
            cp    $healthcheckServicePath       $out/healthcheck-service.json
            ''
          //
          (
            profile
            //
            {
              inherit profileName;
              JSON = profileJson;
              value = profile;
              topology = rec {
                JSON = "${topologyJsonPath}";
                value = (__fromJSON (__readFile JSON));
              };
              node-specs = {
                JSON = nodeSpecsJson;
                value = nodeSpecs;
              };
              inherit node-services generator-service tracer-service healthcheck-service workloads-service;
            }
          )
  ;

in {
  name = profileName;
  inherit profiling;
  inherit profileJson nodeSpecsJson;
  inherit materialise-profile;
}
