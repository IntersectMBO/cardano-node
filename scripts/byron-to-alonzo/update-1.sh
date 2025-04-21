#!/usr/bin/env bash
set -euo pipefail

# This script will:
# - move funds out of the Byron genesis address, so that we can use them later in Shelley
# - initiate the transition to protocol version 1 (Byron, OBFT)

[ -n "${DEBUG:-}" ] && set -x

ROOT=example

pushd ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock
export CARDANO_NODE_NETWORK_ID=42

# move funds out of Byron genesis
cardano-cli submit-tx \
            --tx tx0.tx

cardano-cli submit-tx \
            --tx tx1.tx

# submit update proposal
cardano-cli byron submit-update-proposal \
            --filepath update-proposal

sleep 2

# vote on proposal
cardano-cli byron submit-proposal-vote  \
            --filepath update-vote.000

cardano-cli byron submit-proposal-vote  \
            --filepath update-vote.001

popd
