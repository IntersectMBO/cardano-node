#!/usr/bin/env bash
set -euo pipefail

# This script will initiate the transition to protocol version 5 (Alonzo).

# You need to provide the current epoch as a positional argument (the Shelley
# update system requires this to be included in the update proposal).


# In order for this to be successful, you need to already be in protocol version
# 4 (which happens one or two epoch boundaries after invoking update-4.sh).
# Also, you need to restart the nodes after running this script in order for the
# update to be endorsed by the nodes.

[ -n "${DEBUG:-}" ] && set -x

[ ! "${1:-}" ] && { echo "update-5.sh: expects an <N> epoch argument"; exit; }

EPOCH=$1
VERSION=5

ROOT=example
SPLIT_OUTPUT_ALLOC=1000000000

pushd ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-pool1/node.sock
export CARDANO_NODE_NETWORK_ID=42

TXID3=$(cardano-cli mary transaction txid --tx-file tx3.tx)


# Create the update proposal to change the protocol version to 5

cardano-cli mary governance action create-protocol-parameters-update \
            --out-file update-proposal-alonzo \
            --epoch "${EPOCH}" \
            --genesis-verification-key-file shelley/genesis-keys/genesis1.vkey \
            --genesis-verification-key-file shelley/genesis-keys/genesis2.vkey \
            --protocol-major-version "${VERSION}" \
            --protocol-minor-version 0

# Create a transaction body containing the update proposal.

# Obtain the input lovelace dynamically to reduce change calc complexity
TOTAL_INPUT_LOVELACE=$(
  cardano-cli query utxo --whole-utxo --output-json \
    | jq -er '[to_entries[] | select(.value.value | length == 1) | .value.value.lovelace] | add')

# Slight over-estimate on the fee
FEE=200000
CHANGE=$((
  + TOTAL_INPUT_LOVELACE
  - SPLIT_OUTPUT_ALLOC
  - FEE
))

cardano-cli mary transaction build-raw \
            --fee "$FEE" \
            --tx-in "$TXID3#0" \
            --tx-in "$TXID3#1" \
            --tx-in "$TXID3#2" \
            --tx-out "$(cat addresses/user1.addr)+$((SPLIT_OUTPUT_ALLOC / 2))" \
            --tx-out "$(cat addresses/user1.addr)+$((SPLIT_OUTPUT_ALLOC / 2))" \
            --tx-out "$(cat addresses/user1.addr)+$CHANGE" \
            --update-proposal-file update-proposal-alonzo \
            --out-file tx4.txbody

# Sign the transaction body with the two genesis delegate keys,
# and the the uxto spending key.

cardano-cli mary transaction sign \
            --signing-key-file addresses/user1.skey \
            --signing-key-file shelley/delegate-keys/delegate1.skey \
            --signing-key-file shelley/delegate-keys/delegate2.skey \
            --tx-body-file  tx4.txbody \
            --out-file      tx4.tx


cardano-cli mary transaction submit --tx-file tx4.tx

sed -i configuration.yaml \
    -e 's/LastKnownBlockVersion-Major: 4/LastKnownBlockVersion-Major: 5/' \

popd

echo "Restart the nodes now to endorse the update."
