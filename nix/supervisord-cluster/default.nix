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
      topologyForNode =
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

      ## Backend-specific Nix bits:
      supervisord =
        {
          inherit
            extraSupervisorConfig;

          portShiftEkg        = 100;
          portShiftPrometheus = 200;

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

  inherit (profile.value) era composition monetary;

  path = makeBinPath path';
  path' =
    [ bech32 pkgs.jq pkgs.gnused pkgs.coreutils pkgs.bash pkgs.moreutils
    ]
    ## In dev mode, call the script directly:
    ++ optionals (!workbenchDevMode)
    [ workbench.workbench ];

  startClusterUsage = ''
Usage:
   start-cluster [FLAGS..]

   Flags:

      --batch-name NAME               Override the batch name (default: ${batchName})
      --no-generator | --no-gen       Don't auto-start the tx-generator

      --trace | --debug               Trace the start-cluster script
      --trace-wb | --trace-workbench  Trace the workbench script
      --help                          This help message
'';

  start = pkgs.writeScriptBin "start-cluster" ''
    set -euo pipefail

    batch_name=${batchName}
    run_start_flags=()

    while test $# -gt 0
    do case "$1" in
        --batch-name )                   batch_name=$2; shift;;
        --no-generator | --no-gen )      run_start_flags+=($1);;

        --trace | --debug )              set -x;;
        --trace-wb | --trace-workbench ) export WORKBENCH_EXTRA_FLAGS=--trace;;
        --help )                         cat <<EOF
${startClusterUsage}
EOF
                                         exit 1;;
        * ) break;; esac; shift; done

    workbench-prebuild-executables

    export PATH=$PATH:${path}

    wb backend assert-is supervisor
    wb backend assert-stopped

    wb_run_allocate_args=(
        --cache-dir            "${cacheDir}"
        --base-port             ${toString basePort}
        --stagger-ports
        --
        --port-shift-ekg        100
        --port-shift-prometheus 200
        --supervisor-conf      "${backend.supervisord.mkSupervisorConf profile}"
      )
    wb run allocate $batch_name ${profile.name} "''${wb_run_allocate_args[@]}"

    current_run_path=$(wb run current-path)
    ${workbench.initialiseProfileRunDirShellScript profile "$current_run_path"}

    wb run start "''${run_start_flags[@]}" $(wb run current-tag)

    echo 'workbench:  cluster started. Run `stop-cluster` to stop'
  '';

  stop = pkgs.writeScriptBin "stop-cluster" ''
    set -euo pipefail

    while test $# -gt 0
    do case "$1" in
        --trace | --debug )              set -x;;
        --trace-wb | --trace-workbench ) export WORKBENCH_EXTRA_FLAGS=--trace;;
        * ) break;; esac; shift; done

    wb run stop $(wb run current-tag)
  '';

  restart = pkgs.writeScriptBin "restart-cluster" ''
    set -euo pipefail

    wb_flags=()
    wb_run_restart_flags=()

    while test $# -gt 0
    do case "$1" in
        --no-generator | --no-gen )      wb_run_restart_flags+=($1);;
        --trace | --debug | --trace-wb | --trace-workbench )
                                         wb_flags+=(--trace);;
        --help )                         cat <<EOF
${startClusterUsage}
EOF
                                         exit 1;;
        * ) break;; esac; shift; done

    wb "''${wb_flags[@]}" run restart "''${wb_run_restart_flags[@]}"

    echo "workbench:  alternate command for this action:  wb run restart"
  '';

in
{
  inherit workbench;
  inherit (workbenchProfiles) profilesJSON;
  inherit profile stateDir start stop restart;
}
