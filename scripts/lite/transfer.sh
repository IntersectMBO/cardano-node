#!/usr/bin/env bash

# This script iterates through the txouts of an address and splits each of them into two smaller txouts
#
# The script takes one argument, which is the maximum number of txouts to split.

set -eo pipefail

export BASE="${BASE:-.}"
export CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export SOURCE_SKEY="${SOURCE_SKEY:-example/shelley/utxo-keys/utxo1.skey}"
export TARGET_VKEY="${TARGET_VKEY:-example/shelley/utxo-keys/utxo2.vkey}"
export SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

amount="$1"
source="$("$SCRIPT_DIR/address-of.sh" "${SOURCE_SKEY%.skey}.vkey")"
target="$("$SCRIPT_DIR/address-of.sh" "$TARGET_VKEY")"

utxo_file="$(mktemp)"

tx_base="$(mktemp)"

"$CARDANO_CLI" query utxo --address "$source" --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file "$utxo_file"

echo "Queried UTxO for address: utxoaddr=$utxoaddr"

mapfile -t utxo_as_txins < <(cat "$utxo_file" | jq -r 'to_entries | map(["--tx-in", .key]) | flatten | .[]')

"$CARDANO_CLI" transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$target" \
  ${utxo_as_txins[@]} \
  --tx-out "$target+$amount" \
  --out-file "$tx_base.tx"

"$CARDANO_CLI" transaction sign \
  --tx-body-file "$tx_base.tx" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$SOURCE_SKEY" \
  --out-file "$tx_base.tx.signed"

if [ "$SUBMIT_API_PORT" != "" ]; then
  xxd -r -p <<< $(jq .cborHex "$tx_base.tx.signed") > "$tx_base.tx.signed.cbor"

  curl \
    -s \
    --header "Content-Type: application/cbor" \
    -X POST "http://localhost:$SUBMIT_API_PORT/api/submit/tx" \
    --data-binary "@$tx_base.tx.signed.cbor"
else
  "$CARDANO_CLI" transaction submit --tx-file "$tx_base.tx.signed" --testnet-magic "$TESTNET_MAGIC"
fi
