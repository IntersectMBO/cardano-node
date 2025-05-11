{ pkgs
, lib
, stateDir
, basePort
, useCabalRun
, ...
}:
with lib;
let
  extraShellPkgs = with pkgs;
    [
      python3Packages.supervisor
      pstree
    ]
  ;

  # Backend-specific Nix bits:
  materialise-profile = { profileBundle }:
    pkgs.runCommand
      "workbench-backend-data-${profileBundle.profile.value.name}-supervisor"
      { # Create a `supervisord.conf`
        supervisorConf = import ./supervisor-conf.nix
        { inherit pkgs lib stateDir;
          profile = profileBundle.profile.value;
          nodeSpecs = profileBundle.node-specs.value;
          withGenerator = true;
          withTracer = profileBundle.profile.value.node.trace_forwarding;
          withSsh = false;
          inetHttpServerPort = "127.0.0.1:9001";
        };
        passAsFile = ["supervisorConf"];
      }
      ''
      mkdir $out
      cp $supervisorConfPath $out/supervisor.conf
      ''
  ;

  service-modules = {
    node =
      { config, ... }:
      let selfCfg = config.services.cardano-node;
          i       = toString selfCfg.nodeId;
      in
      { #_file = ./supervisor.nix;
        services.cardano-node.stateDir = stateDir + "/node-${i}";
      };
  };

in
{
  name = "supervisor";

  inherit extraShellPkgs;
  inherit materialise-profile;
  inherit service-modules;
  inherit stateDir basePort;

  inherit useCabalRun;
}
