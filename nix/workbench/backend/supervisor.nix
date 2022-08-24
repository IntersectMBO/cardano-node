let
  basePort              = 30000;
  cacheDirDefault       = "${__getEnv "HOME"}/.cache/cardano-workbench";
  stateDir              = "run/current";
in
{ pkgs
, lib
, workbench
##
, cacheDir              ? cacheDirDefault
, extraBackendConfig    ? {}
, useCabalRun           ? false
, enableEKG             ? true
##
, ...
}:
with lib;
let
  backend =
    rec
    { name = "supervisor";

      services-config = import ./services-config.nix {inherit lib workbench basePort stateDir useCabalRun enableEKG;};

      materialise-profile =
        { profileNix }:
          pkgs.runCommand "workbench-backend-output-${profileNix.name}-${name}d"
            {
              ## Backend-specific Nix bits:
              ## mkBackendConf :: Profile -> SupervisorConf/DockerConf
              supervisorConfPath =
                import ./supervisor-conf.nix
                { inherit (profileNix) node-services;
                  inherit
                    pkgs lib stateDir
                    basePort
                    extraBackendConfig;
                };
            }
            ''
            mkdir $out
            cp    $supervisorConfPath           $out/supervisor.conf
            '';
    };
in
{
  inherit cacheDir stateDir basePort;
  inherit workbench;
  inherit backend;
}
