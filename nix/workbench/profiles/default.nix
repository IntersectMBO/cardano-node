{ pkgs, cardanoLib
, runCommand, runWorkbenchJqOnly, runJq, workbench

## An attrset of specific methods and parameters.
, services-config

, profileName
}:

let
  JSON = runWorkbenchJqOnly "profile-${profileName}.json"
                            "profile json ${profileName}";

  value = __fromJSON (__readFile JSON);

  profile =
    rec {
      name = profileName;

      inherit JSON value;

      topology.files =
        runCommand "topology-${profileName}" {}
          "${workbench}/bin/wb topology make ${JSON} $out";

      node-specs  =
        {
          JSON = runWorkbenchJqOnly "node-specs-${profileName}.json"
                                    "profile node-specs ${JSON}";

          value = __fromJSON (__readFile node-specs.JSON);
        };

      inherit (pkgs.callPackage
               ./node-services.nix
               { inherit runJq services-config profile;
                 baseNodeConfig = cardanoLib.environments.testnet.nodeConfig;
               })
        node-services;

      inherit (pkgs.callPackage
               ./generator-service.nix
               { inherit runJq services-config profile;})
        generator-service;

      inherit (pkgs.callPackage
               ./tracer-service.nix
               { inherit runJq services-config profile;})
        tracer-service;
    };

in profile
