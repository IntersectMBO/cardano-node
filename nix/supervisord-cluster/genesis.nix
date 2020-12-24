{ lib
, runCommand
, writeText
, cardano-cli
##
, path
, stateDir
, baseEnvConfig
, basePort
##
, profile
}:

with profile;
let

  # creates a dummy genesis used as a template
  genesisSpecJSON = runCommand "genesis-spec" { buildInputs = [ cardano-cli ]; } ''
    cardano-cli genesis create --genesis-dir . \
        ${toString cli_args.createSpec}
    mv genesis.spec.json $out
  '';
  genesisSpec = lib.recursiveUpdate
                  (__fromJSON (__readFile genesisSpecJSON))
                  genesis.verbatim;
in

rec {
  params = genesis;
  files = ''
    PATH=${path}
    rm -rf ${stateDir}
    mkdir -p ${stateDir}/{shelley,webserver}
    cp ${__toFile "node.json" (__toJSON baseEnvConfig.nodeConfig)} ${stateDir}/config.json
    cp ${writeText "genesis.spec.json" (__toJSON genesisSpec)} ${stateDir}/shelley/genesis.spec.json
    cardano-cli genesis create --genesis-dir ${stateDir}/shelley \
      ${toString
        (__trace "creating genesis for profile \"${name}\""
          cli_args.createSpec)}
    jq -r --arg systemStart $(date --utc +"%Y-%m-%dT%H:%M:%SZ" --date="${genesis.genesis_future_offset}") \
           '.systemStart = $systemStart |
            .initialFunds = ${__toJSON genesisSpec.initialFunds} |
            .updateQuorum = ${toString composition.n_bft_hosts}' \
    ${stateDir}/shelley/genesis.json | sponge ${stateDir}/shelley/genesis.json
    for i in {1..${toString composition.n_bft_hosts}}
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
      BFT_PORT=$(("${toString basePort}" + $i))
      echo "$BFT_PORT" > "${stateDir}/nodes/node-bft$i/port"
    done
    for i in {1..${toString composition.n_pools}}
    do
      mkdir -p "${stateDir}/nodes/node-pool$i"
      echo "Generating Pool $i Secrets"
      cardano-cli address key-gen \
        --signing-key-file "${stateDir}/nodes/node-pool$i/owner-utxo.skey" \
        --verification-key-file "${stateDir}/nodes/node-pool$i/owner-utxo.vkey"
      cardano-cli stake-address key-gen \
        --signing-key-file "${stateDir}/nodes/node-pool$i/owner-stake.skey" \
        --verification-key-file "${stateDir}/nodes/node-pool$i/owner-stake.vkey"
      # Payment addresses
      cardano-cli address build \
        --payment-verification-key-file "${stateDir}/nodes/node-pool$i/owner-utxo.vkey" \
        --stake-verification-key-file "${stateDir}/nodes/node-pool$i/owner-stake.vkey" \
        --testnet-magic ${toString genesis.network_magic} \
        --out-file "${stateDir}/nodes/node-pool$i/owner.addr"
      # Stake addresses
      cardano-cli stake-address build \
        --stake-verification-key-file "${stateDir}/nodes/node-pool$i/owner-stake.vkey" \
        --testnet-magic ${toString genesis.network_magic} \
        --out-file "${stateDir}/nodes/node-pool$i/owner-stake.addr"
      # Stake addresses registration certs
      cardano-cli stake-address registration-certificate \
        --stake-verification-key-file "${stateDir}/nodes/node-pool$i/owner-stake.vkey" \
        --out-file "${stateDir}/nodes/node-pool$i/stake.reg.cert"

      cardano-cli stake-address key-gen \
        --signing-key-file "${stateDir}/nodes/node-pool$i/reward.skey" \
        --verification-key-file "${stateDir}/nodes/node-pool$i/reward.vkey"
      # Stake reward addresses registration certs
      cardano-cli stake-address registration-certificate \
        --stake-verification-key-file "${stateDir}/nodes/node-pool$i/reward.vkey" \
        --out-file "${stateDir}/nodes/node-pool$i/stake-reward.reg.cert"
      cardano-cli node key-gen \
        --cold-verification-key-file "${stateDir}/nodes/node-pool$i/cold.vkey" \
        --cold-signing-key-file "${stateDir}/nodes/node-pool$i/cold.skey" \
        --operational-certificate-issue-counter-file "${stateDir}/nodes/node-pool$i/cold.counter"
      cardano-cli node key-gen-KES \
        --verification-key-file "${stateDir}/nodes/node-pool$i/kes.vkey" \
        --signing-key-file "${stateDir}/nodes/node-pool$i/kes.skey"
      cardano-cli node key-gen-VRF \
        --verification-key-file "${stateDir}/nodes/node-pool$i/vrf.vkey" \
        --signing-key-file "${stateDir}/nodes/node-pool$i/vrf.skey"

      # Stake address delegation certs
      cardano-cli stake-address delegation-certificate \
        --stake-verification-key-file "${stateDir}/nodes/node-pool$i/owner-stake.vkey" \
        --cold-verification-key-file  "${stateDir}/nodes/node-pool$i/cold.vkey" \
        --out-file "${stateDir}/nodes/node-pool$i/owner-stake.deleg.cert"

      cardano-cli node issue-op-cert \
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
      METADATA_HASH=$(cardano-cli stake-pool metadata-hash --pool-metadata-file "${stateDir}/webserver/pool$i.json")
      POOL_IP="127.0.0.1"
      POOL_PORT=$(("${toString basePort}" + "${toString composition.n_bft_hosts}" + $i))
      echo "$POOL_PORT" > "${stateDir}/nodes/node-pool$i/port"
      POOL_PLEDGE=${toString genesis.pool_coin}
      echo $POOL_PLEDGE > "${stateDir}/nodes/node-pool$i/pledge"
      POOL_MARGIN_NUM=$(( $RANDOM % 10 + 1))

      cardano-cli stake-pool registration-certificate \
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
        --testnet-magic ${toString genesis.network_magic} \
        --out-file "${stateDir}/nodes/node-pool$i/register.cert"

    done

    # Copy genesis-utxo key to ${stateDir}/shelley

    cp ${./genesis-utxo.vkey} ${stateDir}/shelley/genesis-utxo.vkey
    cp ${./genesis-utxo.skey} ${stateDir}/shelley/genesis-utxo.skey

    # Tranfer funds, register pools and delegations, all in one big transaction:

    jq .protocolParams < ${stateDir}/shelley/genesis.json > ${stateDir}/pparams.json

    TXIN_ADDR=$(cardano-cli genesis initial-addr \
                    --testnet-magic ${toString genesis.network_magic} \
                    --verification-key-file ${stateDir}/shelley/genesis-utxo.vkey)

    cardano-cli transaction build-raw \
        --ttl 1000 \
        --fee 0 \
        --tx-in $(cardano-cli genesis initial-txin \
                    --testnet-magic ${toString genesis.network_magic} \
                    --verification-key-file ${stateDir}/shelley/genesis-utxo.vkey) \
        --tx-out "$TXIN_ADDR+0" \
        ${lib.concatMapStringsSep "" (i: ''
          --tx-out $(cat "${stateDir}/nodes/node-pool${toString i}/owner.addr")+${toString genesis.delegator_coin} \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/stake.reg.cert" \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/stake-reward.reg.cert" \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/register.cert" \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/owner-stake.deleg.cert" \'') (lib.genList (i: i + 1) composition.n_pools)}
        --out-file "${stateDir}/shelley/transfer-register-delegate-tx.txbody"
    FEE=$(cardano-cli transaction calculate-min-fee \
                --testnet-magic ${toString genesis.network_magic} \
                --protocol-params-file ${stateDir}/pparams.json \
                --tx-in-count 1 \
                --tx-out-count ${toString (composition.n_pools + 1)} \
                --witness-count ${toString (3 * composition.n_pools + 1)} \
                --byron-witness-count 0 \
                --tx-body-file "${stateDir}/shelley/transfer-register-delegate-tx.txbody" |
                cut -d' ' -f1)

    TXIN_ADDR_HEX=$(bech32 <<< "$TXIN_ADDR")

    TXOUT_AMOUNT=$(jq --arg addr "$TXIN_ADDR_HEX" \
                      --arg fee "$FEE" \
    '(if .initialFunds | has($addr)
      then .initialFunds[$addr]
      else error("initialFunds has no address corresponding to the genesis key: \($addr)")
      end) as $funds
    | $funds - ($fee|tonumber) - (.protocolParams.poolDeposit + (2 * .protocolParams.keyDeposit) + ${toString genesis.delegator_coin}) * ${toString composition.n_pools}' < ${stateDir}/shelley/genesis.json)
    cardano-cli transaction build-raw \
        --ttl 1000 \
        --fee "$FEE" \
        --tx-in $(cardano-cli genesis initial-txin \
                    --testnet-magic ${toString genesis.network_magic} \
                    --verification-key-file ${stateDir}/shelley/genesis-utxo.vkey) \
        --tx-out "$TXIN_ADDR+$TXOUT_AMOUNT" \
        ${lib.concatMapStringsSep "" (i: ''
          --tx-out $(cat "${stateDir}/nodes/node-pool${toString i}/owner.addr")+${toString genesis.delegator_coin} \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/stake.reg.cert" \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/stake-reward.reg.cert" \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/register.cert" \
          --certificate-file "${stateDir}/nodes/node-pool${toString i}/owner-stake.deleg.cert" \'') (lib.genList (i: i + 1) composition.n_pools)}
        --out-file "${stateDir}/shelley/transfer-register-delegate-tx.txbody"

    cardano-cli transaction sign \
      --signing-key-file ${stateDir}/shelley/genesis-utxo.skey \
      ${lib.concatMapStringsSep "" (i: ''
        --signing-key-file "${stateDir}/nodes/node-pool${toString i}/owner-stake.skey" \
        --signing-key-file "${stateDir}/nodes/node-pool${toString i}/reward.skey" \
        --signing-key-file "${stateDir}/nodes/node-pool${toString i}/cold.skey" \'') (lib.genList (i: i + 1) composition.n_pools)}
      --testnet-magic ${toString genesis.network_magic} \
      --tx-body-file  "${stateDir}/shelley/transfer-register-delegate-tx.txbody" \
      --out-file      "${stateDir}/shelley/transfer-register-delegate-tx.tx"

  '';
}
