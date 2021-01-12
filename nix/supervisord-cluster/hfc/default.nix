{
  pkgs
, lib
, cardano-cli
, bech32
, jq
, numBft ? 3
, basePort ? 30000
, securityParam ? 10
, stateDir ? "./state-cluster"
, ...
}:
let
  mkStartScript = envConfig: let
    systemdCompat.options = {
      systemd.services = lib.mkOption {};
      systemd.sockets = lib.mkOption {};
      users = lib.mkOption {};
      assertions = lib.mkOption {};
    };
    eval = let
      extra = {
        services.cardano-node = {
          enable = true;
          inherit (envConfig) operationalCertificate kesKey vrfKey delegationCertificate signingKey topology nodeConfig nodeConfigFile port dbPrefix socketPath;
          inherit stateDir;
        };
      };
    in lib.evalModules {
      prefix = [];
      modules = import ../../nixos/module-list.nix ++ [ systemdCompat extra ];
      args = { inherit pkgs; };
    };
  in pkgs.writeScript "cardano-node" ''
    #!${pkgs.stdenv.shell}
    ${eval.config.services.cardano-node.script}
  '';
  baseEnvConfig = pkgs.callPackage ./base-env.nix { inherit (pkgs.commonLib.cardanoLib) defaultLogConfig; inherit stateDir; };

  supervisorConfig = pkgs.writeText "supervisor.conf" (pkgs.commonLib.supervisord.writeSupervisorConfig ({
    supervisord = {
      logfile = "${stateDir}/supervisord.log";
      pidfile = "${stateDir}/supervisord.pid";
    };
    supervisorctl = {};
    inet_http_server = {
      port = "127.0.0.1:9001";
    };
    "rpcinterface:supervisor" = {
      "supervisor.rpcinterface_factory" = "supervisor.rpcinterface:make_main_rpcinterface";
    };
  } // lib.listToAttrs (map (i:
    lib.nameValuePair "program:bft${toString i}" {
      command = let
        envConfig = baseEnvConfig // rec {
          operationalCertificate = "${stateDir}/nodes/node-bft${toString i}/op.cert";
          kesKey = "${stateDir}/nodes/node-bft${toString i}/kes.skey";
          vrfKey = "${stateDir}/nodes/node-bft${toString i}/vrf.skey";
          signingKey = "${stateDir}/nodes/node-bft${toString i}/byron-deleg.key";
          delegationCertificate = "${stateDir}/nodes/node-bft${toString i}/byron-deleg.json";
          topology = __toFile "topology.yaml" (__toJSON (topologyFile port));
          socketPath = "${stateDir}/bft${toString i}.socket";
          dbPrefix = "db-bft${toString i}";
          port = basePort + i;
          nodeConfigFile = "${stateDir}/config.json";
        };
        script = mkStartScript envConfig;
      in "${script}";
      stdout_logfile = "${stateDir}/bft${toString i}.stdout";
      stderr_logfile = "${stateDir}/bft${toString i}.stderr";
    }
  ) (lib.genList (i: i + 1) numBft))));
  topologyFile = selfPort: {
    Producers = map (p:
      {
        addr = "127.0.0.1";
        port = p;
        valency = 1;
      }) (lib.filter (p: p != selfPort) (lib.genList (i: basePort + i + 1) (numBft)));
  };
  start = pkgs.writeScriptBin "start-cluster-hfc" ''
    set -euo pipefail
    if [ -f ${stateDir}/supervisord.pid ]
    then
      echo "Cluster already running. Please run `stop-cluster` first!"
    fi
    rm -rf ${stateDir}
    mkdir -p ${stateDir}/shelley
    cp ${./byron-params.json} ${stateDir}/byron-params.json
    START_TIME_SHELLEY=$(date --utc +"%Y-%m-%dT%H:%M:%SZ" --date="5 seconds")
    START_TIME=$(date +%s --date="$START_TIME_SHELLEY")
    cardano-cli byron genesis genesis --protocol-magic 42 --k ${toString securityParam} --n-poor-addresses 0 --n-delegate-addresses ${toString numBft} --total-balance ${toString (1000000000000000 * numBft)} --delegate-share 1 --avvm-entry-count 0 --avvm-entry-balance 0 --protocol-parameters-file ${stateDir}/byron-params.json --genesis-output-dir ${stateDir}/byron --start-time "$START_TIME"
    mv ${stateDir}/byron-params.json ${stateDir}/byron/params.json
    jq -r '.securityParam = ${toString securityParam} | .updateQuorum = ${toString numBft}' < ${./genesis.spec.json} > ${stateDir}/shelley/genesis.spec.json
    cardano-cli genesis create --genesis-dir ${stateDir}/shelley --testnet-magic 42 --gen-genesis-keys 3 --start-time "$START_TIME_SHELLEY"
    cp ${__toFile "node.json" (__toJSON baseEnvConfig.nodeConfig)} ${stateDir}/config.json
    chmod u+w ${stateDir}/config.json
    for i in {1..${toString numBft}}
    do
      mkdir -p "${stateDir}/nodes/node-bft$i"
      ln -s "../../shelley/delegate-keys/delegate$i.vrf.skey" "${stateDir}/nodes/node-bft$i/vrf.skey"
      ln -s "../../shelley/delegate-keys/delegate$i.vrf.vkey" "${stateDir}/nodes/node-bft$i/vrf.vkey"
      cardano-cli node key-gen-KES \
        --verification-key-file "${stateDir}/nodes/node-bft$i/kes.vkey" \
        --signing-key-file "${stateDir}/nodes/node-bft$i/kes.skey"
      cardano-cli node issue-op-cert \
        --kes-period 0 \
        --cold-signing-key-file "${stateDir}/shelley/delegate-keys/delegate$i.skey" \
        --kes-verification-key-file "${stateDir}/nodes/node-bft$i/kes.vkey" \
        --operational-certificate-issue-counter-file "${stateDir}/shelley/delegate-keys/delegate$i.counter" \
        --out-file "${stateDir}/nodes/node-bft$i/op.cert"
      INDEX=$(printf "%03d" $(($i - 1)))
      ln -s "../..//byron/delegate-keys.$INDEX.key" "${stateDir}/nodes/node-bft$i/byron-deleg.key"
      ln -s "../..//byron/delegation-cert.$INDEX.json" "${stateDir}/nodes/node-bft$i/byron-deleg.json"
      BFT_PORT=$(("${toString basePort}" + $i))
      echo "$BFT_PORT" > "${stateDir}/nodes/node-bft$i/port"
    done
    ${pkgs.python3Packages.supervisor}/bin/supervisord --config ${supervisorConfig} $@
    while [ ! -S $CARDANO_NODE_SOCKET_PATH ]; do echo "Waiting 5 seconds for bft node to start"; sleep 5; done
    echo 'Cluster started. Run `stop-cluster` to stop'
  '';
  stop = pkgs.writeScriptBin "stop-cluster-hfc" ''
    set -euo pipefail
    ${pkgs.python3Packages.supervisor}/bin/supervisorctl stop all
    if [ -f ${stateDir}/supervisord.pid ]
    then
      kill $(<${stateDir}/supervisord.pid)
      echo "Cluster terminated!"
    else
      echo "Cluster is not running!"
    fi
  '';

in { inherit start stop; }
