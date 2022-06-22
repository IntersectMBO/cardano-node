{ pkgs, lib }:
with lib;

{ profileNix
, backendProfile ## Backend-specific results for forwarding
, workbench
}:
pkgs.runCommand "workbench-profile-output-${profileNix.name}"
  { buildInputs = with pkgs; [ jq workbench ];
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
  cp    ${profileNix.JSON}         $out/profile.json
  cp    ${backendProfile}/*        $out
  cp    $nodeServicesPath          $out/node-services.json
  cp    $generatorServicePath      $out/generator-service.json
  cp    $tracerServicePath         $out/tracer-service.json

  wb profile node-specs $out/profile.json > $out/node-specs.json
  ''
// { inherit (profileNix) name;
     inherit workbench;
   }
