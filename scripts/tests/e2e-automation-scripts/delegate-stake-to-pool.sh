#!/bin/bash

# Current working dir: cardano-node

CWD=$PWD

# Create new addresses with help of existing script

./e2e-automation-scripts/create-payment-and-stake-addresses-with-registartion.sh

if [ $? != 0 ]; then
    error_msg "Creation & registartion of stake address failed"
    exit 1
fi

# Source common lib

. $CWD/e2e-automation-scripts/common.sh

info_msg "Delegating stake to existing stake pools ..."

# Create directory for payment, stake key pair and addresses

keys_dirpath=$addresses_root_dirpath/$address_counter_value
user=user$address_counter_value

stake_signing_keypath=$keys_dirpath/${user}-stake.skey
stake_vrf_keypath=$keys_dirpath/${user}-stake.vkey

pool_1_cold_vrf_keypath=$pool_1_dirpath/operator.vkey
delegation_cert_path=$keys_dirpath/${user}-deleg.cert
raw_tx_for_delegation_cert_path=$keys_dirpath/${user}-tx-deleg-cert.raw
signed_tx_for_delegation_cert_path=$keys_dirpath/${user}-tx-deleg-cert.signed

# Generate the certificate

info_msg "Generating delegation certificate $delegation_cert_path ..."

cardano-cli shelley stake-address delegation-certificate \
    --staking-verification-key-file $stake_vrf_keypath \
    --cold-verification-key-file $pool_1_cold_vrf_keypath \
    --out-file $delegation_cert_path

if [ $? != 0 ]; then
    error_msg "Error during generation of delegation certificate"
    exit 1
fi

# Build TX

info_msg "Building raw TX in order to register certificate $delegation_cert_path for stake delegation address ..."

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

info_msg "Calculating TX fee for registration of delegation certificate ..."

fee=$(cardano-cli shelley transaction calculate-min-fee \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --ttl $ttl \
    --testnet-magic $testnet_magic \
    --signing-key-file $user1_payment_signing_keypath \
    --signing-key-file $stake_signing_keypath \
    --certificate $delegation_cert_path \
    --protocol-params-file $protocol_params_filepath \
    | awk '{ print $2}')

if [ $? != 0 ]; then
    error_msg "Error during fee calculation"
    exit 1
fi

info_msg "Delegaion certificate registration fee: $fee"

# Build TX

info_msg "Building raw TX ..."

tx=$(get_tx_info_for_address $from_address)
input=$(get_input_for_tx $tx)
balance=$(get_balance_for_tx $tx)
change=$(( balance - fee ))

info_msg "TX Info:"
info_msg "TX Input: $input"
info_msg "TX Input Balance: $balance"
info_msg "Change: $change"
info_msg "Sending $fee lovelaces to register delegation certificate"


# Build

info_msg "Building raw TX ..."

cardano-cli shelley transaction build-raw \
    --ttl $ttl \
    --fee $fee \
    --tx-in $input \
    --tx-out "${from_address}+${change}" \
    --certificate-file $delegation_cert_path \
    --out-file $raw_tx_for_delegation_cert_path

# ISSUE with incorrect return code = 1 for success
if [ $?	== 1 ]; then
    error_msg "Error when building raw transaction"
    exit 1
fi


# Sign TX

info_msg "Signing TX ..."

cardano-cli shelley transaction sign \
    --tx-body-file $raw_tx_for_delegation_cert_path \
    --signing-key-file $user1_payment_signing_keypath \
    --signing-key-file $stake_signing_keypath \
    --testnet-magic $testnet_magic \
    --out-file $signed_tx_for_delegation_cert_path

# ISSUE with incorrect return code = 1 for success
if [ $?	== 1 ]; then
    error_msg "Error when signing transaction"
    exit 1
fi

# Submit TX

info_msg "Submitting TX ..."

cardano-cli shelley transaction submit \
    --tx-file $signed_tx_for_delegation_cert_path \
    --testnet-magic $testnet_magic

# ISSUE with incorrect return code = 1 for success
if [ $?	== 1 ]; then
    error_msg "Error when submitting transaction"
    exit 1
fi

success_msg "Transaction submitted without errors"
