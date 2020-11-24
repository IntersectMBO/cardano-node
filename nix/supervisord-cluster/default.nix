let
  defaultGenesisParams = {
    protocolParams = {
      poolDeposit = 500000000;
      keyDeposit = 400000;
      nOpt = 10;
      rho = 0.0022;
      tau = 0.05;
      a0 = 0.3;
      minFeeA = 44;
      minFeeB = 155381;
      decentralisationParam = 0.8;
    };
    slotLength = 0.2;
    activeSlotsCoeff = 0.1;
    securityParam = 10;
    epochLength = 1000;
    maxLovelaceSupply = 45000000000000000;
  };

in {
  pkgs
, lib
, cardano-cli
, bech32
, numBft ? 1
, numPools ? 2
, d ? "0.5"
, basePort ? 30000
, stateDir ? "./state-cluster"
, initialFunds ? import ./initial-funds.nix
, delegatePoolAmount ? 1000000000000
, genesisParams ? {}
, extraSupervisorConfig ? {}
, ...
}:
let
  baseEnvConfig = pkgs.callPackage ./base-env.nix { inherit (pkgs.commonLib.cardanoLib) defaultLogConfig; inherit stateDir; };
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
          inherit (envConfig) operationalCertificate kesKey vrfKey topology nodeConfig nodeConfigFile port dbPrefix socketPath;
          inherit stateDir;
        };
      };
    in lib.evalModules {
      prefix = [];
      modules = import ../nixos/module-list.nix ++ [ systemdCompat extra ];
      args = { inherit pkgs; };
    };
  in pkgs.writeScript "cardano-node" ''
    #!${pkgs.stdenv.shell}
    ${eval.config.services.cardano-node.script}
  '';
  topologyFile = selfPort: {
    Producers = map (p:
      {
        addr = "127.0.0.1";
        port = p;
        valency = 1;
      }) (lib.filter (p: p != selfPort) (lib.genList (i: basePort + i + 1) (numBft + numPools)));
  };
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
      POOL_PLEDGE=${toString delegatePoolAmount}
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

    # Copy genesis-utxo key to ${stateDir}/shelley

    cp ${./genesis-utxo.vkey} ${stateDir}/shelley/genesis-utxo.vkey
    cp ${./genesis-utxo.skey} ${stateDir}/shelley/genesis-utxo.skey

    # Tranfer funds, register pools and delegations, all in one big transaction:

    jq .protocolParams < ${stateDir}/shelley/genesis.json > ${stateDir}/pparams.json

    TXIN_ADDR=$(cardano-cli shelley genesis initial-addr \
                    --testnet-magic ${toString genesisSpecMergedJSON.networkMagic} \
                    --verification-key-file ${stateDir}/shelley/genesis-utxo.vkey)

    cardano-cli shelley transaction build-raw \
        --ttl 1000 \
        --fee 0 \
        --tx-in $(cardano-cli shelley genesis initial-txin \
                    --testnet-magic ${toString genesisSpecMergedJSON.networkMagic} \
                    --verification-key-file ${stateDir}/shelley/genesis-utxo.vkey) \
        --tx-out "$TXIN_ADDR+0" \
        ${lib.concatMapStringsSep "" (i: ''
          --tx-out $(cat "${stateDir}/nodes/node-pool${toString i}/owner.addr")+${toString delegatePoolAmount} \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/stake.reg.cert" \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/stake-reward.reg.cert" \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/register.cert" \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/owner-stake.deleg.cert" \'') (lib.genList (i: i + 1) numPools)}
        --out-file "${stateDir}/shelley/transfer-register-delegate-tx.txbody"
    FEE=$(cardano-cli shelley transaction calculate-min-fee \
                --testnet-magic ${toString genesisSpecMergedJSON.networkMagic} \
                --protocol-params-file ${stateDir}/pparams.json \
                --tx-in-count 1 \
                --tx-out-count ${toString (numPools + 1)} \
                --witness-count ${toString (3 * numPools + 1)} \
                --byron-witness-count 0 \
                --tx-body-file "${stateDir}/shelley/transfer-register-delegate-tx.txbody" |
                cut -d' ' -f1)

    TXIN_ADDR_HEX=$(bech32 <<< "$TXIN_ADDR")

    TXOUT_AMOUNT=$(jq --arg addr "$TXIN_ADDR_HEX" \
                      --arg fee "$FEE" \
    '.initialFunds[$addr] - ($fee|tonumber) - (.protocolParams.poolDeposit + (2 * .protocolParams.keyDeposit) + ${toString delegatePoolAmount}) * ${toString numPools}' < ${stateDir}/shelley/genesis.json)
    cardano-cli shelley transaction build-raw \
        --ttl 1000 \
        --fee "$FEE" \
        --tx-in $(cardano-cli shelley genesis initial-txin \
                    --testnet-magic ${toString genesisSpecMergedJSON.networkMagic} \
                    --verification-key-file ${stateDir}/shelley/genesis-utxo.vkey) \
        --tx-out "$TXIN_ADDR+$TXOUT_AMOUNT" \
        ${lib.concatMapStringsSep "" (i: ''
          --tx-out $(cat "${stateDir}/nodes/node-pool${toString i}/owner.addr")+${toString delegatePoolAmount} \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/stake.reg.cert" \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/stake-reward.reg.cert" \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/register.cert" \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/owner-stake.deleg.cert" \'') (lib.genList (i: i + 1) numPools)}
        --out-file "${stateDir}/shelley/transfer-register-delegate-tx.txbody"

    cardano-cli shelley transaction sign \
      --signing-key-file ${stateDir}/shelley/genesis-utxo.skey \
      ${lib.concatMapStringsSep "" (i: ''
        --signing-key-file "${stateDir}/nodes/node-pool${toString i}/owner-stake.skey" \
        --signing-key-file "${stateDir}/nodes/node-pool${toString i}/reward.skey" \
        --signing-key-file "${stateDir}/nodes/node-pool${toString i}/cold.skey" \'') (lib.genList (i: i + 1) numPools)}
      --testnet-magic ${toString genesisSpecMergedJSON.networkMagic} \
      --tx-body-file  "${stateDir}/shelley/transfer-register-delegate-tx.txbody" \
      --out-file      "${stateDir}/shelley/transfer-register-delegate-tx.tx"

  '';

  start = pkgs.writeScriptBin "start-cluster" ''
    set -euo pipefail
    if [ -f ${stateDir}/supervisord.pid ]
    then
      echo "Cluster already running. Please run `stop-cluster` first!"
    fi
    ${genFiles}
    ${pkgs.python3Packages.supervisor}/bin/supervisord --config ${supervisorConfig} $@
    while [ ! -S $CARDANO_NODE_SOCKET_PATH ]; do echo "Waiting 5 seconds for bft node to start"; sleep 5; done
    echo "Transfering genesis funds to pool owners, register pools and delegations"
    cardano-cli shelley transaction submit --shelley-mode \
      --tx-file ${stateDir}/shelley/transfer-register-delegate-tx.tx \
      --testnet-magic ${toString genesisSpecMergedJSON.networkMagic}
    sleep 5
    echo 'Cluster started. Run `stop-cluster` to stop'
  '';
  stop = pkgs.writeScriptBin "stop-cluster" ''
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

in { inherit baseEnvConfig start stop genesisSpecMergedJSON genesisSpecJSON; }
