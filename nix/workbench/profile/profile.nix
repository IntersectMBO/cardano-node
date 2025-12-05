{ pkgs, lib
# The workbench attrset as it was parametrized.
, workbenchNix
, profileName
}:

let

  # All top-level profile files in one derivation.
  ##############################################################################

  inherit
    (let
      profileDerivedFiles =
        pkgs.runCommand "workbench-profile-files-${profileName}"
          { nativeBuildInputs = with pkgs;
            # A workbench with only the dependencies needed for these commands.
            [ workbenchNix.workbench
              moreutils # sponge
              jq
              graphviz
              workbenchNix.haskellProject.exes.cardano-profile
              workbenchNix.haskellProject.exes.cardano-topology
            ];
          }
          ''
            echo $PATH
            ls
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
      { profileJsonPath   = "${profileDerivedFiles}/profile.json";
        topologyJsonPath  = "${profileDerivedFiles}/topology.json";
        topologyDotPath   = "${profileDerivedFiles}/topology.dot";
        nodeSpecsJsonPath = "${profileDerivedFiles}/node-specs.json";
      }
    )
    profileJsonPath
    topologyJsonPath
    topologyDotPath
    nodeSpecsJsonPath
  ;

  # Helper to use around Nix to build the workbench.
  ##############################################################################

  profileBundle = { backend }:
    let
      profile = __fromJSON (__readFile profileJsonPath);
      nodeSpecs = __fromJSON (__readFile nodeSpecsJsonPath);
      inherit
        (import
          ../service/nodes.nix
          { inherit pkgs;
            inherit workbenchNix;
            inherit backend profile nodeSpecs;
            inherit (backend) profiling;
            inherit profileJsonPath topologyJsonPath;
            ## This ports the (very minimal) config of the deprecated iohk-nix
            ## testnet environment to workbench, removing the dependency on it.
            baseNodeConfig =
              { Protocol              = "Cardano";
                RequiresNetworkMagic  = "RequiresMagic";
                LastKnownBlockVersion-Major = 3;
                LastKnownBlockVersion-Minor = 0;
                LastKnownBlockVersion-Alt   = 0;
              }
              //
              workbenchNix.haskellProject.pkgs.cardanoLib.defaultLogConfig
            ;
          }
        )
        node-services
      ;
      inherit
        (import
          ../service/generator.nix
          { inherit pkgs;
            inherit (workbenchNix) haskellProject;
            inherit backend profile nodeSpecs;
            inherit node-services;
          }
        )
        generator-service
      ;
      workloads-service = builtins.map
        (workload: rec {
          name = workload.name;
          start =
            ''
            ${import ../workload/${name}.nix
              { inherit pkgs;
                inherit (workbenchNix) haskellProject;
                inherit profile nodeSpecs workload;
              }
            }
            ${workload.entrypoints.producers}
            ''
          ;
        })
        profile.workloads
      ;
      inherit
        (import
          ../service/tracer.nix
          { inherit pkgs;
            inherit (workbenchNix) haskellProject;
            inherit backend profile nodeSpecs;
          }
        )
        tracer-service
      ;
      healthcheck-service =
        (import
          ../service/healthcheck.nix
          { inherit pkgs;
            inherit (workbenchNix) haskellProject;
            inherit backend profile nodeSpecs;
          }
        )
      ;
    in {
      profile = {
        JSON = profileJsonPath;
        value = profile;
      };
      topology = rec {
        JSON = "${topologyJsonPath}";
        value = (__fromJSON (__readFile JSON));
      };
      node-specs = {
        JSON = nodeSpecsJsonPath;
        value = nodeSpecs;
      };
      inherit
        node-services
        generator-service
        workloads-service
        tracer-service
        healthcheck-service
      ;
    };

  # Profile output to expose to the profile run directory.
  ##############################################################################

  materialise-profile = { profileBundle }:
    pkgs.runCommand "workbench-profile-data-${profileName}"
      { buildInputs = [];
        inherit profileJsonPath;
        inherit topologyJsonPath topologyDotPath;
        inherit nodeSpecsJsonPath;
        nodeServices = __toJSON
          (pkgs.lib.mapAttrs
            (name: node-service:
              { inherit name;
                inherit (node-service) start config;
                topology = node-service.topology.JSON;
              }
            )
            profileBundle.node-services
          )
        ;
        generatorService = __toJSON
          { name            = "generator";
            inherit (profileBundle.generator-service) start config;
            # Not present on every profile. Can be null.
            inherit (profileBundle.generator-service) plutus-redeemer;
            # Not present on every profile. Can be null.
            inherit (profileBundle.generator-service) plutus-datum;
          }
        ;
        workloadsService = __toJSON (builtins.map (workload:
          { inherit (workload) name start;
          }
        ) profileBundle.workloads-service);
        tracerService = __toJSON
          { name = "tracer";
            inherit (profileBundle.tracer-service) start config;
          }
        ;
        healthcheckService = __toJSON
          { name = "healthcheck";
            inherit (profileBundle.healthcheck-service) start;
          }
        ;
        passAsFile =
          [
            "nodeServices"
            "generatorService"
            "workloadsService"
            "tracerService"
            "healthcheckService"
          ];
      }
      ''
      mkdir $out
      cp    $profileJsonPath              $out/profile.json
      cp    $topologyJsonPath             $out/topology.json
      cp    $topologyDotPath              $out/topology.dot
      cp    $nodeSpecsJsonPath            $out/node-specs.json
      cp    $nodeServicesPath             $out/node-services.json
      cp    $generatorServicePath         $out/generator-service.json
      cp    $workloadsServicePath         $out/workloads-service.json
      cp    $tracerServicePath            $out/tracer-service.json
      cp    $healthcheckServicePath       $out/healthcheck-service.json
      ''
  ;

in {
  name = profileName;
  inherit profileJsonPath nodeSpecsJsonPath;
  inherit profileBundle materialise-profile;
}
