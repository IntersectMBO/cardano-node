{ pkgs
, lib
, bech32
, basePort ? 30000
, stateDir ? "state-cluster"
, cacheDir ? "~/.cache"
, extraSupervisorConfig ? {}
, useCabalRun ? false
##
, profileName ? "default-mary"
, profileOverride ? {}
, ...
}:
with lib;
let
  path = makeBinPath
    [ bech32 pkgs.jq pkgs.gnused pkgs.coreutils pkgs.bash pkgs.moreutils ];

  profilesJSON = pkgs.callPackage ./profiles.nix
    { inherit
      lib;
    };
  profiles = __fromJSON (__readFile profilesJSON);

  profile = recursiveUpdate profiles."${__trace profileName profileName}" profileOverride;
  inherit (profile) era composition monetary;

  profileJSONFile = pkgs.writeText "profile-${profile.name}.json"
    (__toJSON profile);

  topologyNixopsFile = "${stateDir}/topology-nixops.json";
  topology = pkgs.callPackage ./topology.nix
    { inherit lib stateDir topologyNixopsFile;
      inherit (pkgs) graphviz;
      inherit (profile) composition;
      localPortBase = basePort;
    };

  defCardanoExesBash = pkgs.callPackage ./cardano-exes.nix
    { inherit (pkgs) cabal-install cardano-node cardano-cli cardano-topology;
      inherit lib stateDir useCabalRun;
    };

  ## This yields two attributes: 'params' and 'files'
  mkGenesisBash = pkgs.callPackage ./genesis.nix
    { inherit
      lib
      cacheDir stateDir
      basePort
      profile
      profileJSONFile;
      topologyNixopsFile = "${stateDir}/topology-nixops.json";
    };

  node-setups = pkgs.callPackage ./node-setups.nix
    { inherit (pkgs.commonLib.cardanoLib) defaultLogConfig;
      inherit (topology) nodeSpecs;
      inherit
        pkgs lib stateDir
        basePort
        profile
        useCabalRun;
    };

  supervisorConf = pkgs.callPackage ./supervisor-conf.nix
    { inherit (node-setups) nodeSetups;
      inherit
        pkgs lib stateDir
        basePort
        extraSupervisorConfig;
    };

  start = pkgs.writeScriptBin "start-cluster" ''
    set -euo pipefail

    while test $# -gt 0
    do case "$1" in
        --trace | --debug ) set -x;;
        * ) break;; esac; shift; done

    mkdir -p ${cacheDir}

    if test "$(netstat -pltn 2>/dev/null | grep ':9001 ' | wc -l)" != "0"
    then echo "Cluster already running. Please run 'stop-cluster' first!"
         exit 1; fi

    cat <<EOF
Starting cluster:
  - state dir:       ${stateDir}
  - topology:        ${topologyNixopsFile}, ${topology.topologyPdf}
  - node port base:  ${toString basePort}
  - EKG URLs:        http://localhost:${toString (node-setups.nodeIndexToEkgPort 0)}/
  - Prometheus URLs: http://localhost:${toString (node-setups.nodeIndexToPrometheusPort 0)}/metrics
  - profile JSON:    ${profileJSONFile}

EOF

    ${pkgs.jq}/bin/jq '
      include "profiles/derived" { search: "${./.}" };

      profile_pretty_describe(.)
      ' ${profileJSONFile} --raw-output

    rm -rf     ${stateDir}
    mkdir -p ./${stateDir}/supervisor

    PATH=$PATH:${path}
    ${defCardanoExesBash}

    ${topology.mkTopologyBash}

    ${mkGenesisBash}

    ${__concatStringsSep "\n"
      (flip mapAttrsToList node-setups.nodeSetups
        (name: nodeSetup:
          ''
          jq . ${__toFile "${name}-node-config.json"
            (__toJSON nodeSetup.nodeService.nodeConfig)} > \
             ${stateDir}/${name}/config.json

          jq . ${__toFile "${name}-service-config.json"
                (__toJSON
                   (removeAttrs nodeSetup.nodeServiceConfig
                      ["override" "overrideDerivation"]))} > \
             ${stateDir}/${name}/service-config.json

          jq . ${__toFile "${name}-spec.json"
                (__toJSON
                   (removeAttrs nodeSetup.nodeSpec
                      ["override" "overrideDerivation"]))} > \
             ${stateDir}/${name}/spec.json

          ''
        ))}

    cp ${supervisorConf} ${stateDir}/supervisor/supervisord.conf
    ${pkgs.python3Packages.supervisor}/bin/supervisord \
        --config ${supervisorConf} $@

    ## Wait for socket activation:
    #
    if test ! -v "CARDANO_NODE_SOCKET_PATH"
    then export CARDANO_NODE_SOCKET_PATH=$PWD/${stateDir}/node-0/node.socket; fi
    while [ ! -S $CARDANO_NODE_SOCKET_PATH ]; do echo "Waiting 5 seconds for bft node to start"; sleep 5; done

    echo 'Recording node pids..'
    ${pkgs.psmisc}/bin/pstree -Ap $(cat ${stateDir}/supervisor/supervisord.pid) |
    grep 'cabal.*cardano-node' |
    sed -e 's/^.*-+-cardano-node(\([0-9]*\))-.*$/\1/' \
      > ${stateDir}/supervisor/cardano-node.pids

    ${optionalString (!profile.genesis.single_shot)
     ''
      echo "Transfering genesis funds to pool owners, register pools and delegations"
      cli transaction submit \
        --cardano-mode \
        --tx-file ${stateDir}/shelley/transfer-register-delegate-tx.tx \
        --testnet-magic ${toString profile.genesis.network_magic}
      sleep 5
     ''}

    echo 'Cluster started. Run `stop-cluster` to stop'
  '';
  stop = pkgs.writeScriptBin "stop-cluster" ''
    set -euo pipefail
    ${pkgs.python3Packages.supervisor}/bin/supervisorctl stop all
    if [ -f ${stateDir}/supervisor/supervisord.pid ]
    then
      kill $(<${stateDir}/supervisor/supervisord.pid) $(<${stateDir}/supervisor/cardano-node.pids)
      echo "Cluster terminated!"
      rm -f ${stateDir}/supervisor/supervisord.pid ${stateDir}/supervisor/cardano-node.pids
    else
      echo "Cluster is not running!"
    fi
  '';

in { inherit stateDir start stop profilesJSON profile; }
