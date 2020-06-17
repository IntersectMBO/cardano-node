#!/bin/bash

# Current working dir: cardano-node

CWD=$PWD

# source common lib

. $CWD/e2e-automation-scripts/common.sh

# Lets create counter file that will keep the number of how many times this script was ran
# ran == how many addresses were created. It  will be used to create separate directories for each
# new key pair / addresses

check_address_counter_file

# Init variables

amount_transferred=1000000

keys_dirpath=$addresses_root_dirpath/$address_counter_value
user=user$address_counter_value

payment_signing_keypath=$keys_dirpath/${user}.skey
payment_vrf_keypath=$keys_dirpath/${user}.vkey

stake_signing_keypath=$keys_dirpath/${user}-stake.skey
stake_vrf_keypath=$keys_dirpath/${user}-stake.vkey

payment_address_path=$keys_dirpath/${user}.addr
stake_address_path=$keys_dirpath/${user}-stake.addr
stake_cert_path=$keys_dirpath/${user}-stake.reg.cert

# setting aliases:
#
# user1 already exists and was created by mkfiles scripts so this is our source
# of money - "from" - and to_address is an alias for payment_address that we create in this script

raw_tx_filepath=$keys_dirpath/${user}-tx-body.raw
signed_tx_filepath=$keys_dirpath/${user}-tx-body.signed

# Create directory for payment, stake key pair and addresses

info_msg "Creating directory: $keys_dirpath for payment key pair and address files ..."
mkdir -p $keys_dirpath

# Create payment address keys

info_msg "Creating payment address keys for $user ..."

cardano-cli shelley address key-gen \
    --verification-key-file $payment_vrf_keypath \
    --signing-key-file $payment_signing_keypath

if [ $?	!= 0 ]; then
    echo "ERROR: Error during payment keypair creation"
    exit 1
fi

# Create stake address keys

info_msg "Creating stake address keys for $user ..."

cardano-cli shelley stake-address key-gen \
    --verification-key-file $stake_vrf_keypath \
    --signing-key-file $stake_signing_keypath

if [ $? != 0 ]; then
    echo "ERROR: Error during stake keypair creation"
    exit 1
fi

# Build payment address

info_msg "Building payment address for $user ..."

cardano-cli shelley address build \
    --payment-verification-key-file $payment_vrf_keypath \
    --stake-verification-key-file $stake_vrf_keypath \
    --testnet-magic $testnet_magic \
    --out-file $payment_address_path

if [ $? != 0 ]; then
    error_msg "Error during payment address creation"
    exit 1
fi

payment_address=$(cat $payment_address_path)
to_address=$payment_address

# Build stake address

info_msg "Building stake address for $user ..."

cardano-cli shelley stake-address build \
    --stake-verification-key-file $stake_vrf_keypath \
    --testnet-magic $testnet_magic \
    --out-file $stake_address_path

if [ $? != 0 ]; then
    error_msg "Error during stake address creation"
    exit 1
fi

stake_address=$(cat $stake_address_path)

# Create stake addresses registration cert

info_msg "Creating registartion cert $stake_cert_path ..."

cardano-cli shelley stake-address registration-certificate \
    --stake-verification-key-file $stake_vrf_keypath \
    --out-file $stake_cert_path

if [ $? != 0 ]; then
    error_msg "Error during creation of registartion certificate"
    exit 1
fi

# Build TX

info_msg "Building raw TX in order to register certificate for stake address ..."

# Determine TTL

current_tip=$(get_current_tip)

if [ $? != 0 ]; then
    error_msg "Error when getting current tip"
    exit 1
fi

ttl=$(calculate_ttl)

info_msg "Current tip: $current_tip"
info_msg "Setting TTL to: $ttl"

# Calculate fee

info_msg "Calculating TX fee ..."

# Get current protocol params and write it to file

$(get_protocol_params)

if [ $?	!= 0 ]; then
    error_msg "Error when obtaining protocol parameters"
    exit 1
fi

fee=$(cardano-cli shelley transaction calculate-min-fee \
    --tx-in-count 1 \
    --tx-out-count 2 \
    --ttl $ttl \
    --testnet-magic $testnet_magic \
    --signing-key-file $user1_payment_signing_keypath \
    --signing-key-file $stake_signing_keypath \
    --certificate-file $stake_cert_path \
    --protocol-params-file $protocol_params_filepath \
    | awk '{ print $2}')

if [ $? != 0 ]; then
    echo "ERROR: Error during fee calculation"
    exit 1
fi

info_msg "Fee: $fee"

# In order to register stake key on the chain we also need to pay a deposit which is specified in genesis
# parameters

info_msg "Obtaining key deposit ..."

key_deposit=$(get_key_deposit)

if [ $? != 0 ]; then
    error_msg "Error when obtaining key deposit"
    exit 1
fi

info_msg "Key deposit: $key_deposit"

# Build TX

tx=$(get_tx_info_for_address $from_address)
input=$(get_input_for_tx $tx)
balance=$(get_balance_for_tx $tx)
change=$(( balance - fee - amount_transferred - key_deposit))

info_msg "TX Info:"
info_msg "TX Input: $input"
info_msg "TX Input Balance: $balance"
info_msg "Output address: $to_address"
info_msg "Amount trasfered $amount_transferred"
info_msg "Change: $change"

info_msg "Sending $(( amount_transferred / 1000000 )) ADA from $from_address to $to_address"
info_msg "Registering stake address $stake_address"

info_msg "Building raw TX ..."

cardano-cli shelley transaction build-raw \
    --ttl $ttl \
    --fee $fee \
    --tx-in $input \
    --tx-out "${to_address}+${amount_transferred}" \
    --tx-out "${from_address}+${change}" \
    --certificate-file $stake_cert_path \
    --out-file $raw_tx_filepath

# ISSUE with incorrect return code = 1 for success
if [ $?	== 1 ]; then
    error_msg "Error when building raw transaction"
    exit 1
fi

# Sign TX

info_msg "Signing TX ..."

cardano-cli shelley transaction sign \
    --signing-key-file $user1_payment_signing_keypath \
    --signing-key-file $stake_signing_keypath \
    --testnet-magic $testnet_magic \
    --tx-body-file $raw_tx_filepath \
    --out-file $signed_tx_filepath

# ISSUE with incorrect return code = 1 for success
if [ $?	== 1 ]; then
    error_msg "Error when signing transaction"
    exit 1
fi

# Submit TX

info_msg "Submitting TX ..."

cardano-cli shelley transaction submit \
    --tx-file "${signed_tx_filepath}" \
    --testnet-magic "${testnet_magic}"

# ISSUE with incorrect return code = 1 for success
if [ $?	== 1 ]; then
    error_msg "Error when submitting transaction"
    exit 1
fi

# Wait for some time

info_msg "Waiting for the tx to be included into a block ..."

wait_for_new_tip 100

$(assert_address_balance $to_address ${amount_transferred})

if [ $?	!= 0 ]; then
    error_msg "Error when asserting balance"
    exit 1
fi
