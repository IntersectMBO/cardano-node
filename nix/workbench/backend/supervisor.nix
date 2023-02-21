{ pkgs
, lib
, stateDir
, useCabalRun
, basePort
, ...
}:
with lib;
let
  extraShellPkgs = with pkgs;
    [
      python3Packages.supervisor
    ]
  ++ lib.optionals ( useCabalRun)
    (with haskellPackages; [
      cabal-install
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

  # Backend-specific Nix bits:
  materialise-profile =
    { stateDir, profileNix }:
      let supervisorConf = import ./supervisor-conf.nix
        { inherit profileNix;
          inherit pkgs lib stateDir;
          inetHttpServerPort = "127.0.0.1:9001";
        };
      in pkgs.runCommand "workbench-backend-output-${profileNix.profileName}-supervisor"
        {supervisorConfPath = supervisorConf.INI;}
        ''
        mkdir $out
        cp    $supervisorConfPath           $out/supervisor.conf
        '';

  overlay =
    proTopo: self: super:
    {
    };

  service-modules = {
    node =
      { config, ... }:
      let selfCfg = config.services.cardano-node;
          i       = toString selfCfg.nodeId;
      in
      { #_file = ./supervisor.nix;

        services.cardano-node.stateDir = stateDir + "/node-${i}";
        services.cardano-node.operationalCertificate =
          mkIf (selfCfg.isProducer)
            "../genesis/node-keys/node${i}.opcert";
        services.cardano-node.kesKey =
          mkIf (selfCfg.isProducer)
            "../genesis/node-keys/node-kes${i}.skey";
        services.cardano-node.vrfKey =
          mkIf (selfCfg.isProducer)
            "../genesis/node-keys/node-vrf${i}.skey";
      };
  };
in
{
  name = "supervisor";

  inherit extraShellPkgs materialise-profile overlay basePort useCabalRun service-modules;
}
