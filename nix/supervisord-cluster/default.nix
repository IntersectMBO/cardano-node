{ pkgs
, lib
, bech32
, basePort ? 30000
, stateDir ? "./state-cluster"
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

  baseEnvConfig = pkgs.callPackage ./base-env.nix
    { inherit (pkgs.commonLib.cardanoLib) defaultLogConfig;
      inherit (profile) era;
      inherit stateDir lib;
    };

  ## This yields two attributes: 'params' and 'files'
  mkGenesisBash = pkgs.callPackage ./genesis.nix
    { inherit
      lib
      cacheDir stateDir
      baseEnvConfig
      basePort
      profile
      profileJSONFile;
      topologyNixopsFile = "${stateDir}/topology-nixops.json";
    };

  node-setups = pkgs.callPackage ./node-setups.nix
    { inherit (topology) nodeSpecs;
      inherit
        pkgs lib stateDir
        baseEnvConfig
        basePort
        useCabalRun;
    };

  supervisorConf = pkgs.callPackage ./supervisor-conf.nix
    { inherit (topology) nodeSpecs;
      inherit (node-setups) nodeSetups;
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

    if [ -f ${stateDir}/supervisord.pid ]
    then echo "Cluster already running. Please run 'stop-cluster' first!"
         exit 1
    else cat <<EOF
Starting cluster:
  - profile:      ${profile.name}
  - profile JSON: ${profileJSONFile}
  - state dir:    ${stateDir}
  - topology:     ${topologyNixopsFile}

EOF
    fi

    rm -rf ${stateDir}

    PATH=$PATH:${path}
    ${defCardanoExesBash}

    ${topology.mkTopologyBash}

    ${mkGenesisBash}

    echo "Profile '${profile.name}' dump in: ${profileJSONFile}"

    ${__concatStringsSep "\n"
      (flip mapAttrsToList node-setups.nodeSetups
        (name: nodeSetup:
          ''
          jq . ${__toFile "${name}.json"
            (__toJSON nodeSetup.nodeConfig)} > \
             ${stateDir}/${name}.config.json

          jq . ${__toFile "${name}.json"
                (__toJSON
                   (removeAttrs nodeSetup.envConfig
                      ["override" "overrideDerivation"]))} > \
             ${stateDir}/${name}.env.json
          ''
        ))}

    ${pkgs.python3Packages.supervisor}/bin/supervisord \
        --config ${__trace "supervisorConfig: ${supervisorConf} "
                   supervisorConf} $@

    ## Wait for socket activation:
    #
    if test ! -v "CARDANO_NODE_SOCKET_PATH"
    then export CARDANO_NODE_SOCKET_PATH=$PWD/${stateDir}/node-0.socket; fi
    while [ ! -S $CARDANO_NODE_SOCKET_PATH ]; do echo "Waiting 5 seconds for bft node to start"; sleep 5; done

    echo 'Recording node pids..'
    ${pkgs.psmisc}/bin/pstree -Ap $(cat ${stateDir}/supervisord.pid) |
    grep 'cabal.*cardano-node' |
    sed -e 's/^.*-+-cardano-node(\([0-9]*\))-.*$/\1/' \
      > ${stateDir}/cardano-node.pids

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
    if [ -f ${stateDir}/supervisord.pid ]
    then
      kill $(<${stateDir}/supervisord.pid) $(<${stateDir}/cardano-node.pids)
      echo "Cluster terminated!"
      rm -f ${stateDir}/supervisord.pid ${stateDir}/cardano-node.pids
    else
      echo "Cluster is not running!"
    fi
  '';

in { inherit baseEnvConfig start stop profilesJSON profile; }
