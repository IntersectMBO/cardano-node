{ pkgs
, lib
, cardano-cli
, bech32
, basePort ? 30000
, stateDir ? "./state-cluster"
, extraSupervisorConfig ? {}
, useCabalRun ? false
##
, profileName ? "default-mary"
, profileOverride ? {}
, ...
}:
let
  profilesJSON = pkgs.callPackage ./profiles.nix
    { inherit
      lib;
    };
  profiles = __fromJSON (__readFile profilesJSON);

  profile = lib.recursiveUpdate profiles."${profileName}" profileOverride;
  inherit (profile) era composition monetary;

  profileDump = pkgs.writeText "profile-${profile.name}.json"
    (__toJSON profile);

  baseEnvConfig = pkgs.callPackage ./base-env.nix
    { inherit (pkgs.commonLib.cardanoLib) defaultLogConfig;
      inherit stateDir;
      inherit (profile) era;
    };

  cardanoExes = pkgs.callPackage ./cardano-exes.nix
    { inherit (pkgs) cabal-install cardano-node cardano-cli;
      inherit lib stateDir useCabalRun;
    };

  ## This yields two attributes: 'params' and 'files'
  genesis = pkgs.callPackage ./genesis.nix
    { inherit
      lib
      stateDir
      baseEnvConfig
      basePort
      profile
      cardanoExes;
      path = lib.makeBinPath
        [ bech32 pkgs.jq pkgs.gnused pkgs.coreutils pkgs.bash pkgs.moreutils ];
    };

  supervisorConfig = pkgs.callPackage ./supervisor.nix
    { inherit
      pkgs
      lib
      stateDir
      baseEnvConfig
      basePort
      extraSupervisorConfig
      useCabalRun
      profile;
    };

  start = pkgs.writeScriptBin "start-cluster" ''
    while test $# -gt 0
    do case "$1" in
        --trace | --debug ) set -x;;
        * ) break;; esac; shift; done

    ${cardanoExes}

    echo "Profile '${profile.name}' dump in: ${profileDump}"
    set -euo pipefail
    if [ -f ${stateDir}/supervisord.pid ]
    then
      echo "Cluster already running. Please run `stop-cluster` first!"
    fi
    ${genesis.files}
    ${pkgs.python3Packages.supervisor}/bin/supervisord \
        --config ${__trace "supervisorConfig: ${supervisorConfig} "
                   supervisorConfig} $@

    if test ! -v "CARDANO_NODE_SOCKET_PATH"
    then export CARDANO_NODE_SOCKET_PATH=$PWD/${stateDir}/bft1.socket; fi

    while [ ! -S $CARDANO_NODE_SOCKET_PATH ]; do echo "Waiting 5 seconds for bft node to start"; sleep 5; done
    echo "Transfering genesis funds to pool owners, register pools and delegations"
    cli transaction submit \
      --cardano-mode \
      --tx-file ${stateDir}/shelley/transfer-register-delegate-tx.tx \
      --testnet-magic ${toString genesis.params.network_magic}
    sleep 5

    echo 'Recording node pids..'
    ${pkgs.psmisc}/bin/pstree -Ap $(cat ${stateDir}/supervisord.pid) |
    grep 'cabal.*cardano-node' |
    sed -e 's/^.*-+-cardano-node(\([0-9]*\))-.*$/\1/' \
      > ${stateDir}/cardano-node.pids
    echo 'Cluster started. Run `stop-cluster` to stop'
  '';
  stop = pkgs.writeScriptBin "stop-cluster" ''
    set -euo pipefail
    ${pkgs.python3Packages.supervisor}/bin/supervisorctl stop all
    if [ -f ${stateDir}/supervisord.pid ]
    then
      kill $(<${stateDir}/supervisord.pid) $(<${stateDir}/cardano-node.pids)
      echo "Cluster terminated!"
    else
      echo "Cluster is not running!"
    fi
  '';

in { inherit baseEnvConfig start stop profilesJSON profile genesis; }
