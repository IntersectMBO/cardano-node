#!/usr/bin/env bash

set -euox pipefail

#
# Enact a protocol version change.
#
# Submit an SIP, vote on it, then submit an implementation, vote on it, and
# finally endorse it.
#
# Usage:
#
# The script expects as argument the data directory created by the
# 'pivo-testnet.sh' script.
#

if [ $# -ne 1 ];
then
    echo "This script expects the data directory as argument."
    exit 1
fi

data_dir=$1

if [ ! -d "$data_dir" ];
then
    echo "Data directory '$data_dir' does not exists."
    exit 1
fi

CLI="cabal run -v0 exe:cardano-cli"

# We will communicate with node 1 only to keep things simple.
export CARDANO_NODE_SOCKET_PATH=$data_dir/socket/node-1-socket

# We will use the funds in associated to this genesis key to pay for the
# transactions issued during this script. The transactions will pay 0 fee, and
# do not spend any UTXO. The change will be sent to the the same address that we
# spent from.
UTXO=$data_dir/genesis/utxo-keys/utxo1

# Create a staking key pair that will be used for sumitting proposals and
# voting on it.
#
# TODO: we will need to register these keys and transfer funds to them so that
# they have voting power.
# Create a staking key-pair
STAKE=stake
$CLI -- stake-address key-gen \
          --verification-key-file $STAKE.vkey \
          --signing-key-file $STAKE.skey

submit_update_transaction() {
    initial_addr=$1
    update_file=$2
    signing_args=$3

    TX_INFO=/tmp/tx-info.json
    $CLI -- query utxo --testnet-magic 42 --shelley-mode \
          --address $(cat $initial_addr) \
          --out-file $TX_INFO
    BALANCE=`jq '.[].value' $TX_INFO | xargs printf '%.0f\n'`
    TX_IN=`grep -oP '"\K[^"]+' -m 1 $TX_INFO | head -1 | tr -d '\n'`
    # This script assumes the fee to be 0. We might want to check the protocol
    # parameters to make sure that this is indeed the case.
    FEE=0
    CHANGE=`expr $BALANCE - $FEE`
    rm $TX_INFO

    TX_FILE=tx.raw
    # We use a large time-to-live to keep the script simple.
    TTL=1000000
    $CLI -- transaction pivo-build-raw \
          --tx-in $TX_IN \
          --tx-out $(cat $INITIAL_ADDR)+$CHANGE \
          --invalid-hereafter $TTL \
          --fee $FEE \
          --update-payload-file $update_file \
          --out-file $TX_FILE

    SIGNED_TX_FILE=tx.signed
    ## Sign the transaction
    $CLI -- transaction sign \
          --tx-body-file $TX_FILE \
          $signing_args \
          --testnet-magic 42 \
          --out-file $SIGNED_TX_FILE

    ## Submit the signed transaction
    $CLI -- transaction submit \
          --tx-file $SIGNED_TX_FILE \
          --testnet-magic 42 \
          --pivo-mode
}

# Location of the initial address file used to get the funds from.
INITIAL_ADDR=initial.addr
$CLI -- genesis initial-addr \
          --testnet-magic 42 \
          --verification-key-file $UTXO.vkey > $INITIAL_ADDR

################################################################################
## Submit the proposal
################################################################################
UPDATE_FILE=update.payload
$CLI -- governance pivo sip new \
     --stake-verification-key-file $STAKE.vkey \
     --proposal-text "hello world!" \
     --out-file $UPDATE_FILE
submit_update_transaction \
    $INITIAL_ADDR \
    $UPDATE_FILE \
    "--signing-key-file $UTXO.skey --signing-key-file $STAKE.skey"
rm $UPDATE_FILE
################################################################################
## Reveal the proposal
################################################################################
# Wait till the submission is stable in the chain. This depends on the global
# parameters of the era. More specifically:
#
# - activeSlotsCoeff
# - securityParam
# - slotLength
#
# Ideally the values of these parameters should be retrieved from the node. For
# simplicity we use the values of the test genesis file, however there is no
# sanity check that the values assumed in this script are correct.
#
# We assume:
#
# - activeSlotsCoeff = 0.1
# - securityParam    = 10
# - slotLength       = 0.2
#
# So we have:
#
# - stabilityWindow = (3 * securityParam) / activeSlotsCoeff = (3 * 10) / 0.1 = 300
#
# We assume (according to the values of the genesis file) that a slot occurs
# every 0.2 seconds, so we need to wait for 300 * 0.2 = 60 seconds. In practice
# we add a couple of seconds to be on the safe side. In a proper test script we
# would ask the node when a given commit is stable on the chain.
sleep 65

UPDATE_FILE=update.payload
$CLI -- governance pivo sip reveal \
     --stake-verification-key-file $STAKE.vkey \
     --proposal-text "hello world!" \
     --out-file $UPDATE_FILE
submit_update_transaction \
    $INITIAL_ADDR \
    $UPDATE_FILE \
    "--signing-key-file $UTXO.skey" # Note that we do not need to sign with the
                                    # staking key
rm $UPDATE_FILE
################################################################################
## Vote on the proposal
################################################################################
# We wait till the revelation is stable on the chain, which means that the
# voting period is open.
sleep 65
$CLI -- governance pivo sip vote \
          --stake-verification-key-file $STAKE.vkey \
          --proposal-text "hello world!" \
          --out-file $UPDATE_FILE
submit_update_transaction \
    $INITIAL_ADDR \
    $UPDATE_FILE \
    "--signing-key-file $UTXO.skey --signing-key-file $STAKE.skey"
rm $UPDATE_FILE
