#!/bin/bash

# Scenario:
#	1. create 2 addresses (only with payment keys) - addr1 and addr2
#	2. send 5 transactions of 100 Lovelace from user1 (the faucet) to addr1 ($addr1) - 5 different UTXOs in addr1
#	3. send 210 Lovelace from addr1 ($addr1) to addr2 ($addr2) (multiple input UTXOs)
#	4. send all the remaining funds from addr1 ($addr1) to addr2 ($addr2) (multiple input UTXOs)

# Current working dir: cardano-node

CWD=$PWD

# source common lib

. $CWD/e2e-automation-scripts/common.sh

# Lets create counter file that will keep the number of how many times this script was ran
# ran == how many addresses were created. It  will be used to create separate directories for each
# new key pair / addresses


# Create init variables, directory, payment keys and address for the first address (addr1)
check_address_counter_file

keys_dirpath1=$addresses_root_dirpath/$address_counter_value
addr1=user$address_counter_value
payment_verification_keypath1=$keys_dirpath1/${addr1}.vkey
payment_signing_keypath1=$keys_dirpath1/${addr1}.skey
payment_address_path1=$keys_dirpath1/${addr1}.addr

info_msg "Creating directory: $keys_dirpath1 for payment key pair and address files ..."
mkdir -p $keys_dirpath1

info_msg "Creating payment address keys for $addr1 (addr1) ..."

cardano-cli shelley address key-gen \
    --verification-key-file $payment_verification_keypath1 \
    --signing-key-file $payment_signing_keypath1

if [ $?	!= 0 ]; then
    echo "ERROR: Error during payment keypair creation for $addr1"
    exit 1
fi

info_msg "Building payment address for $addr1 ..."

cardano-cli shelley address build \
    --payment-verification-key-file $payment_verification_keypath1 \
    --testnet-magic $testnet_magic \
    --out-file $payment_address_path1

if [ $? != 0 ]; then
    error_msg "Error during payment address creation for $addr1"
    exit 1
fi

payment_address1=$(cat $payment_address_path1)


# Create init variables, directory, payment keys and address for the first address (addr2)
check_address_counter_file

keys_dirpath2=$addresses_root_dirpath/$address_counter_value
addr2=user$address_counter_value
payment_verification_keypath2=$keys_dirpath2/${addr2}.vkey
payment_signing_keypath2=$keys_dirpath2/${addr2}.skey
payment_address_path2=$keys_dirpath2/${addr2}.addr

info_msg "Creating directory: $keys_dirpath2 for payment key pair and address files ..."
mkdir -p $keys_dirpath2

info_msg "Creating payment address keys for $addr2 (addr2) ..."

cardano-cli shelley address key-gen \
    --verification-key-file $payment_verification_keypath2 \
    --signing-key-file $payment_signing_keypath2

if [ $?	!= 0 ]; then
    echo "ERROR: Error during payment keypair creation for $addr2"
    exit 1
fi

info_msg "Building payment address for $addr2 ..."

cardano-cli shelley address build \
    --payment-verification-key-file $payment_verification_keypath2 \
    --testnet-magic $testnet_magic \
    --out-file $payment_address_path2

if [ $? != 0 ]; then
    error_msg "Error during payment address creation for $addr2"
    exit 1
fi

payment_address2=$(cat $payment_address_path2)


# Send 5 transactions of 100 Lovelace from user1 (the faucet) to addr1 ($addr1)

for i in {0..4}; do
	tx_amount=$(( 100 + i ))
	from_address=$user1_payment_address
	to_address=$payment_address1
	signing_key=$user1_payment_signing_keypath

	info_msg "============ Sending $tx_amount Lovelace from user1 (faucet) to addr1 ($addr1) - ($i)"

	send_funds $from_address $to_address $tx_amount $signing_key

	if [ $?	!= 0 ]; then
		error_msg "Error when sending funds from user1 to addr1"
		exit 1
	fi
done

# Send 210 Lovelace from addr1 ($addr1) to addr2 ($addr2)
# It will be required that 3 or more (depending on fee values) UTXOs of addr1 to be grouped and send to addr2

tx_amount=210
from_address=$payment_address1
to_address=$payment_address2
signing_key=$payment_signing_keypath1

send_funds $from_address $to_address $tx_amount $signing_key

if [ $?	!= 0 ]; then
	error_msg "Error when sending funds from user1 to addr1"
	exit 1
fi

# Send all the remaining funds from addr1 ($addr1) to addr2 ($addr2)

tx_amount="ALL"
from_address=$payment_address1
to_address=$payment_address2
signing_key=$payment_signing_keypath1

send_funds $from_address $to_address $tx_amount $signing_key

if [ $?	!= 0 ]; then
	error_msg "Error when sending funds from user1 to addr1"
	exit 1
fi

