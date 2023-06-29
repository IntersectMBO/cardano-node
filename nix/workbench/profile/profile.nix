{ pkgs, lib, cardanoLib
, runCommand
, runJq, jsonFilePretty
, runWorkbenchJqOnly,  runWorkbench
}:

rec {
  profileJson = { profileName }:
    runWorkbenchJqOnly "profile-${profileName}.json"
      "profile json ${profileName}";

  topologyFiles = { profileName, profileJson }:
    import ../topology/topology.nix
      { inherit pkgs profileName profileJson; };

  nodeSpecsJson = { profileName, profileJson }:
    runWorkbenchJqOnly "node-specs-${profileName}.json"
                       "profile node-specs ${profileJson} ${topologyFiles {inherit profileName profileJson;}}";

  genesisFiles = { profileName, profileJson, nodeSpecsJson }:
    import ../genesis/genesis.nix
      { inherit pkgs profileName profileJson nodeSpecsJson; };

  services = { profile, nodeSpecs, topologyFiles, backend, profiling }:
    rec {
      inherit
        (pkgs.callPackage
          ../service/nodes.nix
          {
            inherit backend profile profiling;
            inherit runJq runWorkbench nodeSpecs topologyFiles;
            baseNodeConfig = cardanoLib.environments.testnet.nodeConfig;
          })
        node-services;

      inherit
        (pkgs.callPackage
          ../service/generator.nix
          {
            inherit backend profile;
            inherit nodeSpecs node-services;
            inherit jsonFilePretty;
          })
        generator-service;

      inherit
        (pkgs.callPackage
          ../service/tracer.nix
          {
            inherit backend profile;
            inherit runJq nodeSpecs;
          })
        tracer-service;

      inherit
        (pkgs.callPackage
          ../service/healthcheck.nix
          {
            inherit backend profile;
            inherit runJq nodeSpecs;
          })
        healthcheck-service;
    };

  ## WARNING:  IFD !!
  profile = { profileName, backend, profiling }:
    rec {
      inherit profileName;

      JSON = profileJson { inherit profileName; };
      value = __fromJSON (__readFile JSON);                         ## IFD !!

      topology.files =
        topologyFiles { inherit profileName; profileJson = JSON; };

      node-specs  =
        {
          JSON = nodeSpecsJson
            { inherit profileName;
              profileJson = JSON;
            };
          value =                                                   ## IFD !!
            let nodeSpecsValue = __fromJSON (__readFile node-specs.JSON);
            in if backend.validateNodeSpecs { inherit nodeSpecsValue; }
              then nodeSpecsValue
              else builtins.throw "Incompatible backend for the current profile"
          ;
        };

      # validateNodeSpecs

      genesis.files =
        genesisFiles
          { inherit profileName;
            profileJson = JSON;
            nodeSpecsJson = node-specs.JSON;
          };

      inherit (services
        {
          inherit backend profiling;
          profile = value;
          nodeSpecs = node-specs.value;
          topologyFiles = topology.files;
        })
        node-services
        generator-service
        tracer-service
        healthcheck-service;
    };

  profileData = { profile }:
    let inherit (profile) profileName;
    in
    pkgs.runCommand "workbench-profile-${profileName}"
      { buildInputs = [];
        profileJsonPath = profile.JSON;
        nodeSpecsJsonPath = profile.node-specs.JSON;
        topologyJsonPath = "${profile.topology.files}/topology.json";
        topologyDotPath  = "${profile.topology.files}/topology.dot";
        nodeServices =
          __toJSON
          (lib.flip lib.mapAttrs profile.node-services
            (name: node-service:
              with node-service;
              { inherit name;
                start          = startupScript.JSON;
                config         = nodeConfig.JSON;
                topology       = topology.JSON;
              }));
        generatorService =
          with profile.generator-service;
          __toJSON
          { name           = "generator";
            start          = startupScript.JSON;
            run-script     = runScript.JSON;
          };
        tracerService =
          with profile.tracer-service;
          __toJSON
          { name                 = "tracer";
            config               = config.JSON;
            start                = startupScript.JSON;
          };
        healthcheckService =
          with profile.healthcheck-service;
          __toJSON
          { name                 = "healthcheck";
            start                = startupScript.JSON;
          };
        passAsFile =
          [
            "nodeServices"
            "generatorService"
            "tracerService"
            "healthcheckService"
            "topologyJson"
            "topologyDot"
          ];
      }
      ''
      mkdir $out
      cp    $profileJsonPath              $out/profile.json
      cp    $nodeSpecsJsonPath            $out/node-specs.json
      cp    $topologyJsonPath             $out/topology.json
      cp    $topologyDotPath              $out/topology.dot
      cp    $nodeServicesPath             $out/node-services.json
      cp    $generatorServicePath         $out/generator-service.json
      cp    $tracerServicePath            $out/tracer-service.json
      cp    $healthcheckServicePath       $out/healthcheck-service.json
      ''
  // profile;

  profile-names-json =
    runWorkbenchJqOnly "profile-names.json" "profiles list";

  profile-names =
    __fromJSON (__readFile profile-names-json);

  materialise-profile =
    # `workbench` is the pinned workbench in case there is one.
    profileArgs@{ profileName, ... }:
    let
      mkProfileData = profileName:
        profileData {
          profile = profile profileArgs;
        };
      ps = lib.genAttrs profile-names mkProfileData;
    in
      ps."${profileName}"
        or (throw "No such profile: ${profileName};  Known profiles: ${toString (__attrNames ps)}");
}
