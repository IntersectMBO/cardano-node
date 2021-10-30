{ pkgs
, runCommand, runWorkbenchJqOnly, runJq, workbench, writeText

## The backend is an attrset of AWS/supervisord-specific methods and parameters.
, backend

## Environmental settings:
##   - either affect semantics on all backends equally,
##   - or have no semantic effect
, environment

, profileName
, profileOverride ? {}
}:

let
  baseJSON = runWorkbenchJqOnly "profile-${profileName}.json"
                          "profile get ${profileName}";
  JSON =
    if profileOverride == {}
    then baseJSON
    else
      runJq "profile-${profileName}-overridden.json"
      ''--slurpfile profile  ${baseJSON}
        --slurpfile override ${writeText "profile-override.json" profileOverride}
        --null-input
      ''
      "($profile[0] * $override[0])";

  value = __fromJSON (__readFile JSON);

  profile =
    rec {
      name = profileName;

      inherit JSON value;

      topology.files =
        runCommand "topology-${profile.name}" {}
          "${workbench}/bin/wb topology make ${profile.JSON} $out";

      node-specs  =
        rec {
          JSON = runWorkbenchJqOnly
            "node-specs-${profile.name}.json"
            "profile node-specs ${profile.JSON} ${environment.JSON}";

          value = __fromJSON (__readFile JSON);
        };

      inherit (pkgs.callPackage
               ./node-services.nix
               { inherit runJq backend profile;
                 baseNodeConfig = environment.cardanoLib.environments.testnet.nodeConfig;
               })
        node-services;

      inherit (pkgs.callPackage
               ./generator-service.nix
               { inherit runJq backend profile; })
        generator-service;
    };

in profile
