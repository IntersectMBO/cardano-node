{ pkgs
, lib
, bech32
, basePort ? 30000
, stateDir ? "state-cluster"
, cacheDir ? "${__getEnv "HOME"}/.cache"
, extraSupervisorConfig ? {}
, useCabalRun ? false
, workbenchDevMode ? false
, enableEKG ? true
##
, profileName ? "default-mary"
, ...
}:
with lib;
let
  backend =
    rec
    { ## First, generic items:
      inherit basePort;
      staggerPorts = true;

      topologyForNode =
        { profile, nodeSpec }:
        let inherit (nodeSpec) name i; in
        pkgs.runWorkbench "topology-${name}.json"
          (if nodeSpec.isProducer
           then "topology for-local-node     ${toString i}   ${profile.topology.files} ${toString basePort}"
           else "topology for-local-observer ${profile.name} ${profile.topology.files} ${toString basePort}");

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
            ShelleyGenesisFile   = "../genesis/genesis.json";
            ByronGenesisFile     = "../genesis/byron/genesis.json";
          } // optionalAttrs enableEKG {
            hasEKG               = port + supervisord.portShiftEkg;
            hasPrometheus        = [ "127.0.0.1" (port + supervisord.portShiftPrometheus) ];
            setupBackends = [
              "EKGViewBK"
            ];
          });

      ## Backend-specific:
      supervisord =
        {
          inherit
            extraSupervisorConfig;

          portShiftEkg        = 100;
          portShiftPrometheus = 200;

          mkSupervisorConf =
            profile:
            pkgs.callPackage ./supervisor-conf.nix
            { inherit (profile) node-services;
              inherit
                pkgs lib stateDir
                basePort
                extraSupervisorConfig;
            };
        };

      ## Actions:
      deploy =
        profile:
        ''
        cat <<EOF
workbench:  starting cluster:
  - state dir:       ${stateDir}
  - profile JSON:    ${profile.JSON}
  - node specs:      ${profile.node-specs.JSON}
  - topology:        ${profile.topology.files}/topology-nixops.json ${profile.topology.files}/topology.pdf
  - node port base:  ${toString basePort}
  - EKG URLs:        http://localhost:${toString (basePort + supervisord.portShiftEkg)}/
  - Prometheus URLs: http://localhost:${toString (basePort + supervisord.portShiftPrometheus)}/metrics

EOF

        ${__concatStringsSep "\n"
          (flip mapAttrsToList profile.node-services
            (name: svc:
              ''
              mkdir -p ${stateDir}/${name}
              cp ${svc.nodeSpec.JSON}      ${stateDir}/${name}/spec.json
              cp ${svc.serviceConfig.JSON} ${stateDir}/${name}/service-config.json
              cp ${svc.nodeConfig.JSON}    ${stateDir}/${name}/config.json
              cp ${svc.topology.JSON}      ${stateDir}/${name}/topology.json
              cp ${svc.startupScript}      ${stateDir}/${name}/start.sh
              ''
            ))}

        SUPERVISOR_CONF=${supervisord.mkSupervisorConf profile}
        cp $SUPERVISOR_CONF ${stateDir}/supervisor/supervisord.conf
        ${pkgs.python3Packages.supervisor}/bin/supervisord \
            --config $SUPERVISOR_CONF $@
        '';

      activate =
        profile:
        ''
        if test ! -v CARDANO_NODE_SOCKET_PATH
        then export  CARDANO_NODE_SOCKET_PATH=$(wb local get-node-socket-path ${stateDir})
        fi

        wb local wait-for-node-socket
        wb local record-node-pids     "${stateDir}"

        echo 'workbench:  cluster started. Run `stop-cluster` to stop'
        '';
    };

  environment =
    {
      cardanoLib = pkgs.commonLib.cardanoLib;
      inherit
        stateDir cacheDir
        basePort;
    };

  workbenchProfiles = pkgs.workbench.generateProfiles
    { inherit pkgs backend environment; };
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
    [ pkgs.workbench.workbench ];

  start = pkgs.writeScriptBin "start-cluster" ''
    set -euo pipefail

    PATH=$PATH:${path}

    genesis_args=()

    while test $# -gt 0
    do case "$1" in
        --trace | --debug ) set -x;;
        --force-genesis ) genesis_args+=(--force);;
        --trace-wb | --trace-workbench ) export workbench_extra_flags=--trace;;
        * ) break;; esac; shift; done

    wb local assert-stopped

    wb profile describe ${profileName}

    if test -e "${stateDir}" && test ! -f "${stateDir}"/genesis/byron-protocol-params.json
    then echo "workbench ERROR:  state directory exists, but looks suspicious -- refusing to remove it: '${stateDir}'" >&2
         exit 1
    fi

    rm -rf   "${stateDir}"
    mkdir -p "${stateDir}"/supervisor "${cacheDir}"

    ln -s ${profile.topology.files} "${stateDir}"/topology

    genesis_args+=(
        ## Positionals:
        ${profile.JSON}
        ${profile.topology.files}
        "${stateDir}"/genesis
        )
    wb genesis prepare "''${genesis_args[@]}"

    ${backend.deploy   profile}
    ${backend.activate profile}
  '';
  stop = pkgs.writeScriptBin "stop-cluster" ''
    set -euo pipefail

    while test $# -gt 0
    do case "$1" in
        --trace | --debug ) set -x;;
        * ) break;; esac; shift; done

    ${pkgs.python3Packages.supervisor}/bin/supervisorctl stop all
    if wb local supervisord-running
    then
      if test -f "${stateDir}/supervisor/cardano-node.pids"
      then kill $(<${stateDir}/supervisor/supervisord.pid) $(<${stateDir}/supervisor/cardano-node.pids)
      else pkill supervisord
      fi
      echo "workbench:  cluster terminated"
      rm -f ${stateDir}/supervisor/supervisord.pid ${stateDir}/supervisor/cardano-node.pids
    else
      echo "workbench:  cluster is not running"
    fi
  '';

in
{
  inherit (pkgs) workbench;
  inherit profile stateDir start stop;
}
