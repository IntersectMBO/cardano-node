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
let
  backend =
    rec
    { name = "supervisor";

      # Unlike the nomad backend `useCabalRun` is honored here.
      inherit useCabalRun;

      services-config = import ./services-config.nix {inherit lib workbench basePort stateDir useCabalRun enableEKG;};

      extraShellPkgs = with pkgs; [
        python3Packages.supervisor
      ]
      ++ lib.optionals ( useCabalRun)
        (with haskellPackages; [
          cabalWrapped
          ghcid
          haskellBuildUtils
          cabal-plan
        ])
      ## Workbench's main script is called directly in dev mode.
      ++ lib.optionals (!useCabalRun)
        (with cardanoNodePackages; [
          cardano-node
          cardano-tracer
          tx-generator
        ]);

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
