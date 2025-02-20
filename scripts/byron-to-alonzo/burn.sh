#!/usr/bin/env bash
set -euo pipefail

# This script creates, signs, and submits a transaction that burns the tokens
# that were created with mint.sh.

[ -n "${DEBUG:-}" ] && set -x

ROOT=example
pushd ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock

SCRIPT=ma/policy.script

POLICYID=$(cardano-cli mary transaction policyid --script-file ma/policy.script)
TOKEN_NAME="couttscoin"
TOKEN_NAME_HEX=$(printf "%s" "$TOKEN_NAME" | od -A n -t x1 | tr -d ' \n')

# Obtain the input lovelace dynamically to reduce change calc complexity
NT_UTXO=$(
  cardano-cli query utxo --whole-utxo --output-json \
    | jq -er '[to_entries[] | select(.value.value | length != 1)][0]
      | {"txin": .key, "address": .value.address, "amount": .value.value.lovelace}')

NT_TXIN=$(jq -er '.txin' <<< "$NT_UTXO")

# Match the fee to the min utxo set in mint.sh
FEE=$(jq -er '.amount' <<< "$NT_UTXO")

# For simplicity and to enable use of update-5.sh as a follow up script, we'll
# burn the min utxo lovelace in the native token utxo.
cardano-cli mary transaction build-raw \
            --tx-in "$NT_TXIN" \
            --mint="-5 $POLICYID.$TOKEN_NAME_HEX" \
            --mint-script-file $SCRIPT \
            --fee "$FEE" \
            --out-file tx-burn.txbody

cardano-cli mary transaction sign \
            --signing-key-file addresses/user1.skey \
            --signing-key-file ma/policy.skey \
            --testnet-magic 42 \
            --tx-body-file  tx-burn.txbody \
            --out-file      tx-burn.tx

cardano-cli mary transaction submit --tx-file tx-burn.tx --testnet-magic 42

popd
