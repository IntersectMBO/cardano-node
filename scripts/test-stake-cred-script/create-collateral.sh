#!/usr/bin/env bash

set -e
# Unoffiical bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"


# query the UTxO
cardano-cli query utxo \
            --address "$(cat addresses/address.addr)" \
            --cardano-mode \
            --testnet-magic ${TESTNET_MAGIC} \
            --out-file queries/utxo.json

# cardano-cli transaction build-raw
TXIN=$(jq -r 'keys[0]' queries/utxo.json)
LOVELACE=$(jq -r ".[\"$TXIN\"].value.lovelace" queries/utxo.json)

mkdir -p txs

cardano-cli transaction build \
            --alonzo-era \
            --cardano-mode \
            --testnet-magic ${TESTNET_MAGIC} \
            --tx-in ${TXIN} \
            --tx-out "$(cat addresses/address.addr)+1000000" \
            --change-address $(cat addresses/address-script.addr) \
            --out-file txs/create-collateral.txraw

cardano-cli transaction sign \
            --tx-body-file txs/create-collateral.txraw \
            --signing-key-file addresses/payment-addr.skey \
            --testnet-magic ${TESTNET_MAGIC} \
            --out-file txs/create-collateral

cardano-cli transaction submit \
            --cardano-mode \
            --testnet-magic ${TESTNET_MAGIC} \
            --tx-file txs/create-collateral
