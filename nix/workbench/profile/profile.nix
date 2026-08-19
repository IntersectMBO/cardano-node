{ pkgs
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

  profileBundle = { backend, eraName }:
    let
      profile = __fromJSON (__readFile profileJsonPath);
      nodeSpecs = __fromJSON (__readFile nodeSpecsJsonPath);
      inherit
        (import
          ../service/nodes.nix
          { inherit pkgs;
            inherit workbenchNix;
            inherit backend profile eraName nodeSpecs;
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
      # A workload's Nix template can either return the workload script
      # directly (a string) or an attrset with the script as "start" plus
      # optional extra files to materialize in the workload's run directory
      # (like the tx-generator's "run-script.json" as "config").
      workloads-service = builtins.map
        (workload:
          let template =
            import ../workload/${workload.name}.nix
              { inherit pkgs;
                inherit (workbenchNix) haskellProject;
                inherit backend eraName profile nodeSpecs workload;
              }
          ;
              bundle =
            if builtins.isAttrs template
            then template
            else { start = template; }
          ;
          in {
            name = workload.name;
            start =
              ''
              ${bundle.start}
              ${workload.entrypoint}
              ''
            ;
            config            = bundle.config or null;
            plutus-redeemer   = bundle.plutus-redeemer or null;
            plutus-datum      = bundle.plutus-datum or null;
          }
        )
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
      inherit eraName;
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
        workloads-service
        tracer-service
        healthcheck-service
      ;
    };

  # Profile output to expose to the profile run directory.
  ##############################################################################

  materialise-profile = { profileBundle }:
    # Output (node-services + the tx-generator workload) depends on the era.
    pkgs.runCommand "workbench-profile-data-${profileName}-${profileBundle.eraName}"
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
        workloadsService = __toJSON (builtins.map (workload:
          { inherit (workload) name start;
            # Only present for the "tx-generator" workload. Can be null.
            inherit (workload) config;
            # Not present on every profile. Can be null.
            inherit (workload) plutus-redeemer;
            # Not present on every profile. Can be null.
            inherit (workload) plutus-datum;
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
