{
  pkgs
, lib
, cardano-cli
, bech32
, jq
, numBft ? 3
, numPools ? 2
, basePort ? 30000
, securityParam ? 10
, stateDir ? "./state-cluster"
, ...
}:
let
  mkStartScript = envConfig: let
    systemdCompat.options = {
      systemd.services = lib.mkOption {};
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
    ) (lib.genList (i: i + 1) numBft))
    // lib.listToAttrs (map (i:
      lib.nameValuePair "program:pool${toString i}" {
        command = let
          envConfig = baseEnvConfig // rec {
            operationalCertificate = "${stateDir}/nodes/node-pool${toString i}/op.cert";
            kesKey = "${stateDir}/nodes/node-pool${toString i}/kes.skey";
            vrfKey = "${stateDir}/nodes/node-pool${toString i}/vrf.skey";
            topology = __toFile "topology.yaml" (__toJSON (topologyFile port));
            socketPath = "${stateDir}/pool${toString i}.socket";
            dbPrefix = "db-pool${toString i}";
            port = basePort + numBft + i;
            nodeConfigFile = "${stateDir}/config.json";
          };
          script = mkStartScript envConfig;
        in "${script}";
        stdout_logfile = "${stateDir}/pool${toString i}.stdout";
        stderr_logfile = "${stateDir}/pool${toString i}.stderr";
      }
      ) (lib.genList (i: i + 1) numPools))
      // {
        "program:webserver" = {
          command = "${pkgs.python3}/bin/python -m http.server ${toString basePort}";
          directory = "${stateDir}/webserver";
      };

      } // extraSupervisorConfig));
      # creates a dummy genesis used as a template
      genesisSpec = pkgs.runCommand "create-genesis" { buildInputs = [ cardano-cli ]; } ''
        cardano-cli shelley genesis create --testnet-magic 42 \
                                           --genesis-dir . \
                                           --gen-genesis-keys ${toString numBft}
        cp genesis.spec.json $out
      '';
      genesisSpecJSON = __fromJSON (__readFile genesisSpec);
      genesisSpecMergedJSON = lib.foldl' lib.recursiveUpdate genesisSpecJSON [ defaultGenesisParams genesisParams ];
      path = lib.makeBinPath [ cardano-cli bech32 pkgs.jq pkgs.gnused pkgs.coreutils pkgs.bash pkgs.moreutils ];
      genFiles = ''
        PATH=${path}
        rm -rf ${stateDir}
        mkdir -p ${stateDir}/{shelley,webserver}
        cp ${__toFile "node.json" (__toJSON baseEnvConfig.nodeConfig)} ${stateDir}/config.json
        cp ${pkgs.writeText "genesis.spec.json" (__toJSON genesisSpecMergedJSON)} ${stateDir}/shelley/genesis.spec.json
        cardano-cli shelley genesis create --testnet-magic ${toString genesisSpecMergedJSON.networkMagic} \
                                           --genesis-dir ${stateDir}/shelley \
                                           --gen-genesis-keys ${toString numBft} \
                                           --gen-utxo-keys 1
        jq -r --arg systemStart $(date --utc +"%Y-%m-%dT%H:%M:%SZ" --date="5 seconds") \
               '.systemStart = $systemStart |
                .updateQuorum = ${toString numBft} |
                .initialFunds = (${__toJSON initialFunds})' \
        ${stateDir}/shelley/genesis.json | sponge ${stateDir}/shelley/genesis.json

  topologyFile = selfPort: {
    Producers = map (p:
      {
        addr = "127.0.0.1";
        port = p;
        valency = 1;
      }) (lib.filter (p: p != selfPort) (lib.genList (i: basePort + i + 1) (numBft + numPools)));
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
    cardano-cli genesis --protocol-magic 42 --k ${toString securityParam} --n-poor-addresses 0 --n-delegate-addresses ${toString numBft} --total-balance ${toString (1000000000000000 * numBft)} --byron-formats --delegate-share 1 --avvm-entry-count 0 --avvm-entry-balance 0 --protocol-parameters-file ${stateDir}/byron-params.json --genesis-output-dir ${stateDir}/byron --start-time "$START_TIME"
    mv ${stateDir}/byron-params.json ${stateDir}/byron/params.json
    jq -r '.securityParam = ${toString securityParam} | .updateQuorum = ${toString numBft}' < ${./genesis.spec.json} > ${stateDir}/shelley/genesis.spec.json
    cardano-cli shelley genesis create --genesis-dir ${stateDir}/shelley --testnet-magic 42 --gen-genesis-keys 3 --start-time "$START_TIME_SHELLEY"
    cp ${__toFile "node.json" (__toJSON baseEnvConfig.nodeConfig)} ${stateDir}/config.json
    chmod u+w ${stateDir}/config.json
    for i in {1..${toString numBft}}
    do
      mkdir -p "${stateDir}/nodes/node-bft$i"
      ln -s "../../shelley/delegate-keys/delegate$i.vrf.skey" "${stateDir}/nodes/node-bft$i/vrf.skey"
      ln -s "../../shelley/delegate-keys/delegate$i.vrf.vkey" "${stateDir}/nodes/node-bft$i/vrf.vkey"
      cardano-cli shelley node key-gen-KES \
        --verification-key-file "${stateDir}/nodes/node-bft$i/kes.vkey" \
        --signing-key-file "${stateDir}/nodes/node-bft$i/kes.skey"
      cardano-cli shelley node issue-op-cert \
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
    for i in {1..${toString numPools}}
    do
      mkdir -p "${stateDir}/nodes/node-pool$i"
      echo "Generating Pool $i Secrets"
      cardano-cli shelley address key-gen \
        --signing-key-file "${stateDir}/nodes/node-pool$i/owner-utxo.skey" \
        --verification-key-file "${stateDir}/nodes/node-pool$i/owner-utxo.vkey"
      cardano-cli shelley stake-address key-gen \
        --signing-key-file "${stateDir}/nodes/node-pool$i/owner-stake.skey" \
        --verification-key-file "${stateDir}/nodes/node-pool$i/owner-stake.vkey"
      # Payment addresses
      cardano-cli shelley address build \
        --payment-verification-key-file "${stateDir}/nodes/node-pool$i/owner-utxo.vkey" \
        --stake-verification-key-file "${stateDir}/nodes/node-pool$i/owner-stake.vkey" \
        --testnet-magic ${toString genesisSpecMergedJSON.networkMagic} \
        --out-file "${stateDir}/nodes/node-pool$i/owner.addr"
      # Stake addresses
      cardano-cli shelley stake-address build \
        --stake-verification-key-file "${stateDir}/nodes/node-pool$i/owner-stake.vkey" \
        --testnet-magic ${toString genesisSpecMergedJSON.networkMagic} \
        --out-file "${stateDir}/nodes/node-pool$i/owner-stake.addr"
      # Stake addresses registration certs
      cardano-cli shelley stake-address registration-certificate \
        --stake-verification-key-file "${stateDir}/nodes/node-pool$i/owner-stake.vkey" \
        --out-file "${stateDir}/nodes/node-pool$i/stake.reg.cert"

      cardano-cli shelley stake-address key-gen \
        --signing-key-file "${stateDir}/nodes/node-pool$i/reward.skey" \
        --verification-key-file "${stateDir}/nodes/node-pool$i/reward.vkey"
      # Stake reward addresses registration certs
      cardano-cli shelley stake-address registration-certificate \
        --stake-verification-key-file "${stateDir}/nodes/node-pool$i/reward.vkey" \
        --out-file "${stateDir}/nodes/node-pool$i/stake-reward.reg.cert"
      cardano-cli shelley node key-gen \
        --cold-verification-key-file "${stateDir}/nodes/node-pool$i/cold.vkey" \
        --cold-signing-key-file "${stateDir}/nodes/node-pool$i/cold.skey" \
        --operational-certificate-issue-counter-file "${stateDir}/nodes/node-pool$i/cold.counter"
      cardano-cli shelley node key-gen-KES \
        --verification-key-file "${stateDir}/nodes/node-pool$i/kes.vkey" \
        --signing-key-file "${stateDir}/nodes/node-pool$i/kes.skey"
      cardano-cli shelley node key-gen-VRF \
        --verification-key-file "${stateDir}/nodes/node-pool$i/vrf.vkey" \
        --signing-key-file "${stateDir}/nodes/node-pool$i/vrf.skey"

      # Stake address delegation certs
      cardano-cli shelley stake-address delegation-certificate \
        --stake-verification-key-file "${stateDir}/nodes/node-pool$i/owner-stake.vkey" \
        --cold-verification-key-file  "${stateDir}/nodes/node-pool$i/cold.vkey" \
        --out-file "${stateDir}/nodes/node-pool$i/owner-stake.deleg.cert"

      cardano-cli shelley node issue-op-cert \
        --kes-period 0 \
        --cold-signing-key-file "${stateDir}/nodes/node-pool$i/cold.skey" \
        --kes-verification-key-file "${stateDir}/nodes/node-pool$i/kes.vkey" \
        --operational-certificate-issue-counter-file "${stateDir}/nodes/node-pool$i/cold.counter" \
        --out-file "${stateDir}/nodes/node-pool$i/op.cert"

      echo "Generating Pool $i Metadata"
      jq -n \
         --arg name "TestPool$i" \
         --arg description "Test Pool $i" \
         --arg ticker "TEST$i" \
         --arg homepage "http://localhost:${toString basePort}/pool$i.html" \
         '{"name": $name, "description": $description, "ticker": $ticker, "homepage": $homepage}' > "${stateDir}/webserver/pool$i.json"

      METADATA_URL="http://localhost:${toString basePort}/pool$i.json"
      METADATA_HASH=$(cardano-cli shelley stake-pool metadata-hash --pool-metadata-file "${stateDir}/webserver/pool$i.json")
      POOL_IP="127.0.0.1"
      POOL_PORT=$(("${toString basePort}" + "${toString numBft}" + $i))
      echo "$POOL_PORT" > "${stateDir}/nodes/node-pool$i/port"
      POOL_PLEDGE=$(( $RANDOM % 1000000000 + 1000000000000))
      echo $POOL_PLEDGE > "${stateDir}/nodes/node-pool$i/pledge"
      POOL_MARGIN_NUM=$(( $RANDOM % 10 + 1))

      cardano-cli shelley stake-pool registration-certificate \
        --cold-verification-key-file "${stateDir}/nodes/node-pool$i/cold.vkey" \
        --vrf-verification-key-file "${stateDir}/nodes/node-pool$i/vrf.vkey" \
        --pool-pledge "$POOL_PLEDGE" \
        --pool-margin "$(jq -n $POOL_MARGIN_NUM/10)" \
        --pool-cost "$(($RANDOM % 100000000))" \
        --pool-reward-account-verification-key-file "${stateDir}/nodes/node-pool$i/reward.vkey" \
        --pool-owner-stake-verification-key-file "${stateDir}/nodes/node-pool$i/owner-stake.vkey" \
        --metadata-url "$METADATA_URL" \
        --metadata-hash "$METADATA_HASH" \
        --pool-relay-port "$POOL_PORT" \
        --pool-relay-ipv4 "127.0.0.1" \
        --testnet-magic ${toString genesisSpecMergedJSON.networkMagic} \
        --out-file "${stateDir}/nodes/node-pool$i/register.cert"

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
