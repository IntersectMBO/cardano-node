#!/usr/bin/env bash

# This script queries for the UTxO of an address.
#
# The address may be supplied as an address, a file containing the address, or a verification key file

set -eo pipefail

export CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export UTXO_VKEY="${UTXO_VKEY:-example/shelley/utxo-keys/utxo1.vkey}"

source="$1"
addr="unknown"

if [[ "$source" == *.vkey ]]; then
  $CARDANO_CLI address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$source"
elif [[ "$source" == *.addr ]]; then
  cat "$source"
elif [[ "$source" == addr_* ]]; then
  echo -n "$source"
else
  echo "unknown source: $source" 2> /dev/null
  exit 1
fi
