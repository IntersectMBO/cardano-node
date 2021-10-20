#!/usr/bin/env bash

# This script iterates through the txouts of an address and splits each of them into two smaller txouts
#
# The script takes one argument, which is the maximum number of txouts to split.

set -eo pipefail

export BASE="${BASE:-.}"
export CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export UTXO_SKEY="${UTXO_SKEY:-example/shelley/utxo-keys/utxo1.skey}"
export SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

amount="$1"
source_skey="$2"
source="$("$SCRIPT_DIR/address-of.sh" "${source_skey%.skey}.vkey")"
target="$("$SCRIPT_DIR/address-of.sh" "$3")"

utxo_file="$(mktemp)"

tx_base="$(mktemp)"

"$CARDANO_CLI" query utxo --address "$source" --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file "$utxo_file"

echo "Queried UTxO for address: utxoaddr=$utxoaddr"

txins="$(jq -r ". | to_entries | sort_by(.value.value.lovelace) | reverse | map(.key)[0:$count][]" "$utxo_file")"

"$CARDANO_CLI" transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$target" \
  --tx-in "$txin" \
  --tx-out "$target+$amount" \
  --out-file "$tx_base.tx"

"$CARDANO_CLI" transaction sign \
  --tx-body-file "$tx_base.tx" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$source_skey" \
  --out-file "$tx_base.tx.signed"

if [ "$SUBMIT_API_PORT" != "" ]; then
  xxd -r -p <<< $(jq .cborHex "$tx_base.tx.signed") > "$tx_base.tx.signed.cbor"

  curl \
    -s \
    --header "Content-Type: application/cbor" \
    -X POST "http://localhost:$SUBMIT_API_PORT/api/submit/tx" \
    --data-binary "@$tx_base.tx.signed.cbor"
else
  "$CARDANO_CLI" transaction submit --tx-file "$tx_base.tx.signed.cbor" --testnet-magic "$TESTNET_MAGIC"
fi
