#!/usr/bin/env bash
set -euo pipefail

# This script creates, signs, and submits a transaction that creates some new tokens.
# It uses the output of the transaction from update-4.sh.

[ -n "${DEBUG:-}" ] && set -x

ROOT=example
SPLIT_OUTPUT_ALLOC=1000000000
pushd ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock
export CARDANO_NODE_NETWORK_ID=42

mkdir -p ma
cardano-cli address key-gen \
            --verification-key-file ma/policy.vkey \
            --signing-key-file ma/policy.skey

KEYHASH=$(cardano-cli address key-hash --payment-verification-key-file ma/policy.vkey)

SCRIPT=ma/policy.script
rm -f $SCRIPT
{
  echo "{"
  echo "  \"keyHash\": \"${KEYHASH}\","
  echo "  \"type\": \"sig\""
  echo "}"
} >> $SCRIPT

TXID3=$(cardano-cli mary transaction txid --tx-file tx3.tx)

POLICYID=$(cardano-cli mary transaction policyid --script-file ma/policy.script)
TOKEN_NAME="couttscoin"
TOKEN_NAME_HEX=$(printf "%s" "$TOKEN_NAME" | od -A n -t x1 | tr -d ' \n')

# Obtain the input lovelace dynamically to reduce change calc complexity
TOTAL_INPUT_LOVELACE=$(
  cardano-cli query utxo --whole-utxo --output-json \
    | jq -er '[to_entries[] | select(.value.value | length == 1) | .value.value.lovelace] | add')

# Slight over-estimate on the fee
FEE=200000
MIN_UTXO=1000000
CHANGE=$((
  + TOTAL_INPUT_LOVELACE
  - SPLIT_OUTPUT_ALLOC
  - MIN_UTXO
  - FEE
))

# Overwrite the prior tx3 tx out file so update-5.sh still obtains the correct utxo after mint/burn
cardano-cli mary transaction build-raw \
            --tx-in "$TXID3#0" \
            --tx-in "$TXID3#1" \
            --tx-in "$TXID3#2" \
            --tx-out "$(cat addresses/user1.addr)+$((SPLIT_OUTPUT_ALLOC / 2))" \
            --tx-out "$(cat addresses/user1.addr)+$((SPLIT_OUTPUT_ALLOC / 2))" \
            --tx-out "$(cat addresses/user1.addr)+$CHANGE" \
            --tx-out="$(cat addresses/user1.addr)+$MIN_UTXO+5 $POLICYID.$TOKEN_NAME_HEX" \
            --mint="5 $POLICYID.$TOKEN_NAME_HEX" \
            --mint-script-file $SCRIPT \
            --fee "$FEE" \
            --out-file tx3.txbody

cardano-cli mary transaction sign \
            --signing-key-file addresses/user1.skey \
            --signing-key-file ma/policy.skey \
            --tx-body-file  tx3.txbody \
            --out-file      tx3.tx

echo
echo "The whole utxo prior to minting is:"
echo
cardano-cli query utxo --whole-utxo --output-json
echo

cardano-cli mary transaction submit --tx-file tx3.tx
sleep 2
echo
echo "The whole utxo after minting is:"
echo
cardano-cli query utxo --whole-utxo --output-json
echo

popd
