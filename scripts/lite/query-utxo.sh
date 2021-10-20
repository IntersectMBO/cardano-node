#!/usr/bin/env bash

# This script queries for the UTxO of an address.
#
# The address may be supplied as an address, a file containing the address, or a verification key file

set -eo pipefail

export CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export UTXO_VKEY="${UTXO_VKEY:-example/shelley/utxo-keys/utxo1.vkey}"
export SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

source="$($SCRIPT_DIR/address-of.sh "$1")"

echo "UTxOs for $source"

utxo_out=$(mktemp)

$CARDANO_CLI query utxo --address "$source" --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file "$utxo_out"

cat "$utxo_out" | jq -r '
    to_entries
  | map({idx: .key | split("#")[1], lovelace: .value.value.lovelace})
  | sort_by(.idx)[]
  | "#" + .idx + "\t" + (.lovelace | tostring)
  '
