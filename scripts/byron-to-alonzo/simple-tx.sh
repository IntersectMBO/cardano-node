#!/usr/bin/env bash

set -e
set -x

# This script creates, signs, and submits a simple transaction in the Alonzo era.
# We simply generate a new user and send him some money.
# To use:
#   start an alonzo cluster and wait until, say, you can query the tip of node-bft1
#   then run as ./scripts/byron-to-alonzo/simple-tx.sh

ROOT=example
TESTNET=42
WORK=simple-tx-files
CLI=cardano-cli
pushd ${ROOT}
mkdir ${WORK}
export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock

# 1. Build a new user
#########################################################################################

pushd ${WORK}
$CLI address key-gen --normal-key \
  --verification-key-file user.vkey \
  --signing-key-file user.skey
$CLI address build --payment-verification-key-file user.vkey --testnet-magic $TESTNET \
  --out-file user.addr
popd

# 2. Build a transaction body
#########################################################################################

# The single utxo input
UTXO_SKEY=shelley/utxo-keys/utxo1.skey
UTXO_VKEY=shelley/utxo-keys/utxo1.vkey
$CLI address build --payment-verification-key-file ${UTXO_VKEY} \
  --testnet-magic $TESTNET \
  --out-file shelley/utxo-keys/utxo1.addr
UTXO_ADDR=$(cat shelley/utxo-keys/utxo1.addr)
$CLI query utxo --address ${UTXO_ADDR} --cardano-mode --testnet-magic $TESTNET \
  --out-file $WORK/txin.json
TXIN=$(jq -r 'keys[0]' $WORK/txin.json)

# The first output of 10 ada
USER_ADDR=$(cat $WORK/user.addr)
TXOUT="${USER_ADDR}+10000000"

# Protocol parameters
$CLI query protocol-parameters --cardano-mode --testnet-magic \
  $TESTNET --out-file $WORK/pparams.json

# The tx body itself
$CLI transaction build --alonzo-era --cardano-mode --testnet-magic $TESTNET \
  --change-address ${UTXO_ADDR} \
  --tx-in $TXIN \
  --tx-out $TXOUT \
  --protocol-params-file $WORK/pparams.json \
  --out-file $WORK/example-tx.body

# 3. Sign the transaction body
#########################################################################################

$CLI transaction sign --tx-body-file $WORK/example-tx.body \
  --testnet-magic $TESTNET \
  --signing-key-file ${UTXO_SKEY} \
  --out-file $WORK/example-tx.tx

# 4. Submit the signed transaction
#########################################################################################

$CLI transaction submit --tx-file $WORK/example-tx.tx --testnet-magic $TESTNET

# 4. Query the new utxo of the new user
#########################################################################################

sleep 4
$CLI query utxo --address ${USER_ADDR} --cardano-mode --testnet-magic $TESTNET

popd
