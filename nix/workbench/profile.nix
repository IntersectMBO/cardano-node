{ pkgs, lib }:
with lib;

{ profileNix
, backendProfile ## Backend-specific results for forwarding
}:
pkgs.runCommand "workbench-profile-output-${profileNix.name}"
  { buildInputs = [];
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
    passAsFile = [ "nodeServices" "generatorService" ];
  }
  ''
  mkdir $out
  cp    ${backendProfile}/*  $out
  cp    $nodeServicesPath          $out/node-services.json
  cp    $generatorServicePath      $out/generator-service.json
  ''
