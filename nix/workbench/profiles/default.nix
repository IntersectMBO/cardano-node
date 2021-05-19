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
    {
      name = profileName;

      inherit environment;

      inherit JSON value;

      topology    = pkgs.callPackage
        ./topology.nix   { inherit workbench profile; };

      node-specs  = pkgs.callPackage
        ./node-specs.nix { inherit runWorkbenchJqOnly profile backend; };

     inherit (pkgs.callPackage
               ./node-services.nix
               { inherit runJq backend environment profile; })
        node-services;
    };

in profile
