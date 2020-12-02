#!/usr/bin/env bash

set -e
set -x

# This script will initiate the transition to protocol version 3 (Allegra).

# It will also set up a working stake pool (including delegating to it).

# You need to provide the current epoch as a positional argument (the Shelley
# update system requires this to be includded in the update proposal).


# In order for this to be successful, you need to already be in protocol version
# 2 (which happens one or two epoch boundaries after invoking update-2.sh).
# Also, you need to restart the nodes after running this script in order for the
# update to be endorsed by the nodes.

EPOCH=$1
VERSION=4

ROOT=example
COINS_IN_INPUT=450000000

pushd ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock

TXHASH=$(cardano-cli query utxo --testnet-magic 42 --allegra-era | grep ${COINS_IN_INPUT} | awk '{print $1;}')

cardano-cli governance create-update-proposal \
            --out-file update-proposal-mary \
            --epoch ${EPOCH} \
            --genesis-verification-key-file shelley/genesis-keys/genesis1.vkey \
            --genesis-verification-key-file shelley/genesis-keys/genesis2.vkey \
            --protocol-major-version ${VERSION} \
            --protocol-minor-version 0

# Now we'll construct one whopper of a transaction that does everything
# just to show off that we can, and to make the script shorter

# We'll transfer all the funds to the user1, which delegates to pool1
# We'll register certs to:
#  1. register the pool-owner1 stake address
#  2. register the stake pool 1
#  3. register the user1 stake address
#  4. delegate from the user1 stake address to the stake pool
# We'll include the update proposal

cardano-cli transaction build-raw \
            --ttl 100000 \
            --fee 0 \
            --tx-in $TXHASH#0\
            --tx-out $(cat addresses/user1.addr)+${COINS_IN_INPUT} \
            --update-proposal-file update-proposal-mary \
            --out-file tx2.txbody

# So we'll need to sign this with a bunch of keys:
# 1. the initial utxo spending key, for the funds
# 2. the user1 stake address key, due to the delegatation cert
# 3. the pool1 owner key, due to the pool registration cert
# 4. the pool1 operator key, due to the pool registration cert
# 5. the genesis delegate keys, due to the update proposal

cardano-cli transaction sign \
            --signing-key-file addresses/user1.skey \
            --signing-key-file shelley/delegate-keys/delegate1.skey \
            --signing-key-file shelley/delegate-keys/delegate2.skey \
            --testnet-magic 42 \
            --tx-body-file  tx2.txbody \
            --out-file      tx2.tx


cardano-cli transaction submit --tx-file tx2.tx --testnet-magic 42

sed -i configuration.yaml \
    -e 's/LastKnownBlockVersion-Major: 3/LastKnownBlockVersion-Major: 4/' \


popd

echo "Restart the nodes now to endorse the update."
