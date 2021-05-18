{ pkgs
, runCommand, runWorkbench, runJq, workbench, writeText

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
  baseJSON = runWorkbench "profile-${profileName}.json"
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
    {
      name = profileName;

      inherit environment;

      inherit JSON value;

      topology.files =
        runCommand "topology-${profile.name}" {}
          "${workbench}/bin/wb topology make ${profile.JSON} $out";

      node-specs  =
        rec {
          JSON = runWorkbench
            "node-specs-${profile.name}.json"
            "profile node-specs ${profile.JSON} ${environment.JSON}";

          value = __fromJSON (__readFile JSON);
        };

     inherit (pkgs.callPackage
               ./node-services.nix
               { inherit runJq runWorkbench backend environment profile; })
        node-services;
    };

in profile
