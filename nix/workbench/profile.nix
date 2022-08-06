{ pkgs, lib, profileNix, backend }:
  with lib;
    pkgs.runCommand "workbench-profile-output-${profileNix.name}-${backend.name}d"
      { buildInputs = [];
        profileConfigJsonPath = profileNix.JSON;
        nodeSpecsJsonPath = profileNix.node-specs.JSON;
        backendConfigPath = backend.materialise-profile { inherit profileNix; };
        nodeServices =
          __toJSON
          (flip mapAttrs profileNix.node-services
            (name: svc:
              with svc;
              { inherit name;
                service-config = serviceConfig.JSON;
                start          = startupScript;
                config         = nodeConfig.JSON;
                topology       = topology.JSON;
              }));
        generatorService =
          with profileNix.generator-service;
          __toJSON
          { name           = "generator";
            service-config = serviceConfig.JSON;
            start          = startupScript;
            run-script     = runScript.JSON;
          };
        tracerService =
          with profileNix.tracer-service;
          __toJSON
          { name                 = "tracer";
            tracer-config        = tracer-config.JSON;
            nixos-service-config = nixos-service-config.JSON;
            config               = config.JSON;
            start                = startupScript;
          };
        passAsFile = [ "nodeServices" "generatorService" "tracerService" ];
      }
      ''
      mkdir $out
      cp    $profileConfigJsonPath        $out/profile.json
      cp    $nodeSpecsJsonPath            $out/node-specs.json
      cp    $backendConfigPath/*          $out
      cp    $nodeServicesPath             $out/node-services.json
      cp    $generatorServicePath         $out/generator-service.json
      cp    $tracerServicePath            $out/tracer-service.json
      ''
