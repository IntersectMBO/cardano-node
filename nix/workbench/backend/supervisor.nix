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
          pkgs.runCommand "workbench-backend-output-${profileNix.name}-${name}d" {}
            ''
            mkdir $out
            cp ${mkBackendConf profileNix} $out/supervisor.conf
            '';

      ## Backend-specific Nix bits:
      ## mkBackendConf :: Profile -> SupervisorConf/DockerConf
      mkBackendConf =
        profile:
        pkgs.callPackage ./supervisor-conf.nix
        { inherit (profile) node-services;
          inherit
            pkgs lib stateDir
            basePort
            extraBackendConfig;
        };
    };
in
{
  inherit cacheDir stateDir basePort;
  inherit workbench;
  inherit backend;
}
