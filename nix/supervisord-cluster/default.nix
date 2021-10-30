let
  basePortDefault    = 30000;
  cacheDirDefault    = "${__getEnv "HOME"}/.cache/cardano-workbench";
  stateDirDefault    = "run/current";
  profileNameDefault = "default-alzo";
in
{ pkgs
, workbench
, lib
, bech32
, basePort              ? basePortDefault
, stateDir              ? stateDirDefault
, cacheDir              ? cacheDirDefault
, extraSupervisorConfig ? {}
, useCabalRun           ? false
, workbenchDevMode      ? false
, enableEKG             ? true
##
, profileName           ? profileNameDefault
, batchName             ? "plain"
, ...
}:
with lib;
let
  backend =
    rec
    { ## Generic Nix bits:
      topologyForNodeSpec =
        { profile, nodeSpec }:
        let inherit (nodeSpec) name i; in
        workbench.runWorkbench
          "topology-${name}.json"
          "topology projection-for local-${nodeSpec.kind} ${toString i} ${profile.name} ${profile.topology.files} ${toString basePort}";

      nodePublicIP =
        { i, name, ... }@nodeSpec:
        "127.0.0.1";

      finaliseNodeService =
        { name, i, isProducer, ... }: svc: recursiveUpdate svc
          ({
            stateDir       = stateDir + "/${name}";
            ## Everything is local in the supervisord setup:
            socketPath     = "node.socket";
            topology       = "topology.json";
            nodeConfigFile = "config.json";
          } // optionalAttrs useCabalRun {
            executable     = "cabal run exe:cardano-node --";
          } // optionalAttrs isProducer {
            operationalCertificate = "../genesis/node-keys/node${toString i}.opcert";
            kesKey         = "../genesis/node-keys/node-kes${toString i}.skey";
            vrfKey         = "../genesis/node-keys/node-vrf${toString i}.skey";
          });

      finaliseNodeConfig =
        { port, ... }: cfg: recursiveUpdate cfg
          ({
            AlonzoGenesisFile    = "../genesis/alonzo-genesis.json";
            ShelleyGenesisFile   = "../genesis.json";
            ByronGenesisFile     = "../genesis/byron/genesis.json";
          } // optionalAttrs enableEKG {
            hasEKG               = port + supervisord.portShiftEkg;
            hasPrometheus        = [ "127.0.0.1" (port + supervisord.portShiftPrometheus) ];
            setupBackends = [
              "EKGViewBK"
            ];
          });

      finaliseGeneratorService =
        svc: recursiveUpdate svc
          ({
            sigKey         = "../genesis/utxo-keys/utxo1.skey";
            nodeConfigFile = "config.json";
            runScriptFile  = "run-script.json";
          } // optionalAttrs useCabalRun {
            executable     = "cabal run exe:tx-generator --";
          });

      finaliseGeneratorConfig =
        cfg: recursiveUpdate cfg
          ({
            AlonzoGenesisFile    = "../genesis/alonzo-genesis.json";
            ShelleyGenesisFile   = "../genesis.json";
            ByronGenesisFile     = "../genesis/byron/genesis.json";
          });

      profileOutput =
        { profile }:
        pkgs.runCommand "workbench-profile-outputs-${profile.name}-supervisord" {}
          ''
          mkdir $out
          cp ${supervisord.mkSupervisorConf profile} $out/supervisor.conf
          '';

      ## Backend-specific Nix bits:
      supervisord =
        {
          inherit
            extraSupervisorConfig;

          portShiftEkg        = 100;
          portShiftPrometheus = 200;

          ## mkSupervisorConf :: Profile -> SupervisorConf
          mkSupervisorConf =
            profile:
            pkgs.callPackage ./supervisor-conf.nix
            { inherit (profile) node-services generator-service;
              inherit
                pkgs lib stateDir
                basePort
                extraSupervisorConfig;
            };
        };
    };

  ## IMPORTANT:  keep in sync with envArgs in 'workbench/default.nix/generateProfiles/environment'.
  envArgs =
    {
      inherit (pkgs) cardanoLib;
      inherit
        stateDir
        cacheDir basePort;
      staggerPorts = true;
    };

  workbenchProfiles = workbench.generateProfiles
    { inherit pkgs backend envArgs; };
in

let
  profile = workbenchProfiles.profiles."${profileName}"
    or (throw "No such profile: ${profileName};  Known profiles: ${toString (__attrNames workbenchProfiles.profiles)}");

  profileOut = workbench.profileOutput { inherit profile;
                                         backendProfileOutput =
                                           backend.profileOutput { inherit profile; };
                                       };

  inherit (profile.value) era composition monetary;

  path = makeBinPath path';
  path' =
    [ bech32 pkgs.jq pkgs.gnused pkgs.coreutils pkgs.bash pkgs.moreutils
    ]
    ## In dev mode, call the script directly:
    ++ optionals (!workbenchDevMode)
    [ workbench.workbench ];

  start = pkgs.writeScriptBin "start-cluster" ''
    set -euo pipefail

    export PATH=$PATH:${path}

    wb start \
        --batch-name   ${batchName} \
        --profile-name ${profileName} \
        --profile-out  ${profileOut} \
        --cache-dir    ${cacheDir} \
        --base-port    ${toString basePort} \
        ''${WORKBENCH_CABAL_MODE:+--cabal} \
        "$@" &&
        echo 'workbench:  cluster started. Run `stop-cluster` to stop' >&2
  '';

  stop = pkgs.writeScriptBin "stop-cluster" ''
    set -euo pipefail

    wb finish "$@"
  '';

  restart = pkgs.writeScriptBin "restart-cluster" ''
    set -euo pipefail

    wb run restart "$@" && \
        echo "workbench:  alternate command for this action:  wb run restart" >&2
  '';

in
{
  inherit workbench;
  inherit (workbenchProfiles) profilesJSON;
  inherit profile stateDir start stop restart;
}
