{ pkgs, lib }:
with lib;

{ profile
, backendProfileOutput ## Backend-specific results for forwarding
}:
pkgs.runCommand "workbench-profile-outputs-${profile.name}"
  { buildInputs = [];
    nodeServices =
      __toJSON
      (flip mapAttrs profile.node-services
        (name: svc:
          with svc;
          { inherit name;
            service-config = serviceConfig.JSON;
            start          = startupScript;
            config         = nodeConfig.JSON;
            topology       = topology.JSON;
          }));
    generatorService =
      with profile.generator-service;
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
  cp    ${backendProfileOutput}/*  $out
  cp    $nodeServicesPath          $out/node-services.json
  cp    $generatorServicePath      $out/generator-service.json
  ''
