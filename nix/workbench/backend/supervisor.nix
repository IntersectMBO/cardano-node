{ pkgs
, lib
, useCabalRun
, ...
}:
let
  name = "supervisor";

  # Unlike the nomad backend `useCabalRun` is honored here.
  inherit useCabalRun;

  extraShellPkgs = with pkgs;
    [
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
    { stateDir, profileNix }:
      pkgs.runCommand "workbench-backend-output-${profileNix.name}-${name}"
        {
          ## Backend-specific Nix bits:
          ## mkBackendConf :: Profile -> SupervisorConf/DockerConf
          supervisorConfPath =
            import ./supervisor-conf.nix
            { inherit (profileNix) node-services;
              inherit pkgs lib stateDir;
              inetHttpServerPort = "127.0.0.1:9001";
            };
        }
        ''
        mkdir $out
        cp    $supervisorConfPath           $out/supervisor.conf
        '';
in
{
  inherit name useCabalRun extraShellPkgs materialise-profile;
}
