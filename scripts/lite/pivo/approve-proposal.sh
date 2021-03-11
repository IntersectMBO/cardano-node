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

submit_update_transaction() {
    initial_addr=$1
    update_file=$2
    signing_args=$3

    submit_transaction \
        $initial_addr \
        $initial_addr \
        pivo-build-raw \
        "--update-payload-file $update_file" \
        "$signing_args" \
        --pivo-mode
}

# Procedure
#
#   submit_transaction \
#     initial_addr \
#     change_addr \
#     tx_building_cmd \
#     tx_building_args \
#     signing_args \
#     tx_submission_mode
#
# submits a transaction where:
#
# - initial_addr is the address where the funds are going to be taken from.
# - This functions assumes a fee of 0.
#
# - change_addr is the address where the change of the transaction is sent.
#
# - tx_building_cmd is the command to be used to build the transaction.
#   Examples of such commands include: build-raw and pivo-build-raw
#
# - tx_building_args are the arguments to be used when buildng the transaction.
#   Examples of such arguments include update payload file, or certificate
#   files.
#
# - signing_args are the arguments to be passed to the transaction sign
#   command. This argument can be used to pass the signing keys of the
#   transaction.
#
# - tx_submission_mode is the mode used when submitting the transaction.
#   Examples of the available modes are --pivo-mode or --shelley-mode. Remember
#   to include the '--' symbols when specifying a mode!
submit_transaction() {
    initial_addr=$1
    change_addr=$2
    tx_building_cmd=$3
    tx_building_args=$4
    signing_args=$5
    tx_submission_mode=$6

    TX_INFO=/tmp/tx-info.json
    $CLI -- query utxo --testnet-magic 42 --shelley-mode \
          --address $(cat $initial_addr) \
          --out-file $TX_INFO
    cat $TX_INFO
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
    $CLI -- transaction $tx_building_cmd \
          --tx-in $TX_IN \
          --tx-out $(cat $change_addr)+$CHANGE \
          --invalid-hereafter $TTL \
          --fee $FEE \
          $tx_building_args \
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
          $tx_submission_mode
}

# Location of the initial address file used to get the funds from.
INITIAL_ADDR=initial.addr
$CLI -- genesis initial-addr \
          --testnet-magic 42 \
          --verification-key-file $UTXO.vkey > $INITIAL_ADDR

################################################################################
## Register the stakepools
################################################################################
register_stakepool(){
    # Path where the stake keys should be created
    stake_key=$1
    # Path where the payment address should be stored
    payment_addr=$2
    # Utxo key used to:
    #
    # - pay for the transaction fees
    # - create a payment address together with the stake key.
    utxo_key=$3
    # Address used to pay for the transaction fees. The change will be sent
    # back to this address.
    utxo_addr=$4
    # File containing the pool metadata
    metadata_file=$5
    # VRF key to associate to the cold keys
    vrf_key=$6

    ##
    ## Stake address registration
    ##
    # Create the stake key files
    $CLI -- stake-address key-gen \
          --verification-key-file $stake_key.vkey \
          --signing-key-file $stake_key.skey

    # Use these keys to create a payment address. This key should have funds
    # associated to it if we want the stakepool to have stake delegated to it.
    $CLI -- address build \
          --payment-verification-key-file $utxo_key.vkey \
          --stake-verification-key-file $stake_key.vkey \
          --out-file $payment_addr \
          --testnet-magic 42

    # Create an address registration certificate, which will be submitted to
    # the blockchain.
    $CLI -- stake-address registration-certificate \
          --stake-verification-key-file $stake_key.vkey \
          --out-file $stake_key.cert

    submit_transaction \
        $utxo_addr \
        $utxo_addr \
        build-raw \
        "--certificate-file $stake_key.cert" \
        "--signing-key-file $utxo_key.skey --signing-key-file $stake_key.skey" \
        --shelley-mode

    ##
    ## Stake pool registration
    ##
    # Get the hash of the file:
    METADATA_HASH=`$CLI -- stake-pool metadata-hash --pool-metadata-file $metadata_file`

    # Generate cold keys
    COLD=cold
    $CLI -- node key-gen \
            --cold-verification-key-file $COLD.vkey \
            --cold-signing-key-file $COLD.skey \
            --operational-certificate-issue-counter-file $COLD.counter

    # Create a pool registration certificate
    # Pledge amount in Lovelace
    PLEDGE=1000000
    # Pool cost per-epoch in Lovelace
    COST=1000
    # Pool cost per epoch in percentage
    MARGIN=0.1
    POOL_REGISTRATION_CERT=pool-registration.cert
    # Create the registration certificate
    $CLI -- stake-pool registration-certificate \
            --cold-verification-key-file $COLD.vkey \
            --vrf-verification-key-file $vrf_key.vkey \
            --pool-pledge $PLEDGE \
            --pool-cost $COST \
            --pool-margin $MARGIN \
            --pool-reward-account-verification-key-file $stake_key.vkey \
            --pool-owner-stake-verification-key-file $stake_key.vkey \
            --testnet-magic 42 \
            --metadata-url file://$metadata_file \
            --metadata-hash $METADATA_HASH \
            --out-file $POOL_REGISTRATION_CERT

    # Create a delegation certificate between the stake key and the cold key
    DELEGATION_CERT=delegation.cert
    $CLI -- stake-address delegation-certificate \
            --stake-verification-key-file $stake_key.vkey \
            --cold-verification-key-file $COLD.vkey \
            --out-file $DELEGATION_CERT

    # Finally submit the transaction
    sleep 5
    submit_transaction \
        $utxo_addr \
        $payment_addr \
        build-raw \
        "--certificate-file $POOL_REGISTRATION_CERT --certificate-file $DELEGATION_CERT" \
        "--signing-key-file $utxo_key.skey --signing-key-file $stake_key.skey --signing-key-file $COLD.skey " \
        --shelley-mode
}

##
## Stake keys for submitting proposals and voting on them.
##
PROPOSING_KEY=proposing_key
VOTING_KEY1=proposing_key # For simplicity the proposing key is also a voting key
VOTING_KEY2=voting_key2
VOTING_KEY3=voting_key3
# So the voting key 'i' is associated with the utxo key 'i'.

##
## Stake pool metadata
##
for i in 1 2 3; do
    echo "{
      \"name\": \"PriviPool ${i}\",
      \"description\": \"Priviledge Pool ${i}\",
      \"ticker\": \"PP${i}\",
      \"homepage\": \"https://ppp${i}\"
    }" > "pool${i}_metadata.json"
done

UTXO1=$data_dir/genesis/utxo-keys/utxo1
register_stakepool \
    $PROPOSING_KEY \
    payment1.addr \
    $UTXO1 \
    $INITIAL_ADDR \
    pool1_metadata.json \
    $data_dir/node-1/vrf

UTXO2=$data_dir/genesis/utxo-keys/utxo2
UTXO2_ADDR=UTXO2_ADDR
$CLI -- genesis initial-addr \
          --testnet-magic 42 \
          --verification-key-file $UTXO2.vkey > $UTXO2_ADDR
register_stakepool \
    $VOTING_KEY2 \
    payment2.addr \
    $UTXO2 \
    $UTXO2_ADDR \
    pool2_metadata.json \
    $data_dir/node-2/vrf

UTXO3_ADDR=UTXO3_ADDR
UTXO3=$data_dir/genesis/utxo-keys/utxo3
$CLI -- genesis initial-addr \
          --testnet-magic 42 \
          --verification-key-file $UTXO3.vkey > $UTXO3_ADDR
register_stakepool \
    $VOTING_KEY3 \
    payment3.addr \
    $UTXO3 \
    $UTXO3_ADDR \
    pool3_metadata.json \
    $data_dir/node-3/vrf

################################################################################
## Submit the proposal
################################################################################
UPDATE_FILE=update.payload
$CLI -- governance pivo sip new \
     --stake-verification-key-file $PROPOSING_KEY.vkey \
     --proposal-text "hello world!" \
     --out-file $UPDATE_FILE
# Note that PROPOSING_KEY is associated to payment1.addr
submit_update_transaction \
    payment1.addr \
    $UPDATE_FILE \
    "--signing-key-file $UTXO1.skey --signing-key-file $PROPOSING_KEY.skey"
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
     --stake-verification-key-file $PROPOSING_KEY.vkey \
     --proposal-text "hello world!" \
     --out-file $UPDATE_FILE
submit_update_transaction \
    payment1.addr \
    $UPDATE_FILE \
    "--signing-key-file $UTXO1.skey" # Note that we do not need to sign with the
                                    # staking key
rm $UPDATE_FILE
################################################################################
## Vote on the proposal
################################################################################
# We wait till the revelation is stable on the chain, which means that the
# voting period is open.
sleep 65
$CLI -- governance pivo sip vote \
          --stake-verification-key-file $VOTING_KEY1.vkey \
          --proposal-text "hello world!" \
          --out-file $UPDATE_FILE
submit_update_transaction \
    payment1.addr \
    $UPDATE_FILE \
    "--signing-key-file $UTXO1.skey --signing-key-file $VOTING_KEY1.skey"
rm $UPDATE_FILE
