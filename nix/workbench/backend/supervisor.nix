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
    ]
  ++ lib.optionals ( useCabalRun)
    (with haskellPackages; [
      cabal-install
      ghcid
      haskellBuildUtils
      pkgs.cabal-plan
    ])
  ## Workbench's main script is called directly in dev mode.
  ++ lib.optionals (!useCabalRun)
    (with cardanoNodePackages; [
      cardano-node
      cardano-tracer
      tx-generator
    ]);

  validateNodeSpecs = { nodeSpecsValue }:
    builtins.all (r: r == "loopback")
      (lib.attrsets.mapAttrsToList
        (name: value: value.region)
        nodeSpecsValue
      )
  ;

  # Backend-specific Nix bits:
  materialise-profile =
    { profileData }:
      let supervisorConf = import ./supervisor-conf.nix
        { inherit pkgs lib stateDir;
          # Create a `supervisord.conf`
          nodeSpecs = profileData.node-specs.value;
          withGenerator = true;
          withTracer = profileData.value.node.tracer;
          withSsh = false;
          inetHttpServerPort = "127.0.0.1:9001";
        };
      in pkgs.runCommand "workbench-backend-output-${profileData.profileName}-supervisor"
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
      };
  };

in
{
  name = "supervisor";

  inherit extraShellPkgs;
  inherit validateNodeSpecs materialise-profile;
  inherit overlay service-modules;
  inherit stateDir basePort;

  inherit useCabalRun;
}
