#!/bin/bash

# Current working dir: cardano-node

CWD=$PWD

./e2e-automation-scripts/create-payment-and-stake-addresses-with-registartion.sh

if [ $? != 0 ]; then
    error_msg "Creation & registartion of stake address failed"
    exit 1
fi

# Source common lib

. $CWD/e2e-automation-scripts/common.sh

# Lets create counter file that will keep the number of how many times this script was ran
# ran == how many addresses were created. It  will be used to create separate directories for each
# new key pair / addresses

check_pool_counter_file

# Create directory for pool and relay

pool_dirpath=$CWD/example/node-pool$pool_counter_value
relay_dirpath=$CWD/example/node-relay$pool_counter_value

pool_cold_vrf_keypath=$pool_dirpath/cold.vkey
pool_cold_signing_keypath=$pool_dirpath/cold.skey

pool_hot_kes_vkeypath=$pool_dirpath/kes.vkey
pool_hot_kes_signing_keypath=$pool_dirpath/kes.skey

pool_hot_vrf_vkeypath=$pool_dirpath/vrf.vkey
pool_hot_vrf_signing_keypath=$pool_dirpath/vrf.skey

pool_operational_certificate_counter_filepath=$pool_dirpath/cold.counter
pool_operational_certificate_path=$pool_dirpath/op.cert

info_msg "Creating directories $pool_dirpath and $relay_dirpath for pool and relay ..."

mkdir -p $pool_dirpath
mkdir -p $relay_dirpath


# Generate  offline "cold" key pair

info_msg "Creating cold-keys inside $pool_dirpath ..."

cardano-cli shelley node key-gen \
    --verification-key-file $pool_cold_vrf_keypath \
    --signing-key-file $pool_cold_signing_keypath \
    --operational-certificate-issue-counter $pool_operational_certificate_counter_filepath

if [ $?	!= 0 ]; then
    error_msg "Error during cold keys creation"
    exit 1
fi


# Generate hot KES keypair

info_msg "Generating hot KES keypair ..."

cardano-cli shelley node key-gen-KES \
    --verification-key-file $pool_hot_kes_vkeypath \
    --signing-key-file $pool_hot_kes_signing_keypath

if [ $?	!= 0 ]; then
    error_msg "Error during hot KES keypair creation"
    exit 1
fi

# Get start of KES validity period

kes_period=$(calculate_kes_period)

# Create operational certificate for your pool

info_msg "Creating operational certificate for new pool ..."

cardano-cli shelley node issue-op-cert \
    --kes-verification-key-file $pool_hot_kes_vkeypath \
    --cold-signing-key-file $pool_cold_signing_keypath \
    --operational-certificate-issue-counter-file $pool_operational_certificate_counter_filepath \
    --kes-period $kes_period \
    --out-file $pool_operational_certificate_path

if [ $?	!= 0 ]; then
    error_msg "Error during operational certificate creation"
    exit 1
fi

# Generate a VRF key pair for your new stake pool - they will be used for leader lottery selection

info_msg "Generating a VRF key pair for new stake pool ..."

cardano-cli shelley node key-gen-VRF \
    --verification-key-file $pool_hot_vrf_vkeypath \
    --signing-key-file $pool_hot_vrf_signing_keypath

if [ $?	!= 0 ]; then
    error_msg "Error during VRF key pair creation"
    exit 1
fi

# Create topology files for pool and relay

relay_hostname=127.0.0.1
relay_port=3001

local_relay_hostname=127.0.0.1
local_relay_port=4240

local_block_producer_hostname=127.0.0.1
local_block_producer_port=4242

info_msg "Creating topology files for pool and relay ..."

echo "{
   \"Producers\": [
     {
       \"addr\": \"${local_block_producer_hostname}\",
       \"port\": ${local_block_producer_port},
       \"valency\": 1
     }
   ]
}" > $pool_dirpath/topology.json



 echo "{
   \"Producers\": [
     {
       \"addr\": \"${local_relay_hostname}\",
       \"port\": ${local_relay_port},
       \"valency\": 1
     },
     {
       \"addr\": \"${relay_hostname}\",
       \"port\": ${relay_port},
       \"valency\": 1
     }
   ]
}" > $relay_dirpath/topology.json



# Generate registration cert

info_msg "Generating pool certificate ..."

# Pool params
pool_pledge=7000000
pool_cost=77000000
pool_margin=0.07

# Get current protocol params and write it to file

$(get_protocol_params)

stake_keys_and_address_location=$addresses_root_dirpath/$address_counter_value
user=user$address_counter_value
stake_vkey_keypath=$stake_keys_and_address_location/${user}-stake.vkey
stake_skey_keypath=$stake_keys_and_address_location/${user}-stake.skey
pool_registartion_cert_path=$pool_dirpath/pool.cert
pool_own_delegation_cert_path=$pool_dirpath/own-deleg.cert
raw_tx_pool_registration_filepath=$pool_dirpath/pool-registration-tx.raw
signed_tx_pool_registration_filepath=$pool_dirpath/pool-registration-tx.signed

cardano-cli shelley stake-pool registration-certificate \
    --cold-verification-key-file $pool_cold_vrf_keypath \
    --vrf-verification-key-file $pool_hot_vrf_vkeypath \
    --pool-pledge $pool_pledge \
    --pool-cost $pool_cost \
    --pool-margin $pool_margin \
    --pool-reward-account-verification-key-file $stake_vkey_keypath \
    --pool-owner-stake-verification-key-file $stake_vkey_keypath \
    --testnet-magic $testnet_magic \
    --out-file $pool_registartion_cert_path

if [ $?	!= 0 ]; then
    error_msg "Error during pool registration certificate creation"
    exit 1
fi

# Pledge some stake to your stake pool

info_msg "Generating delegation certificate - delegating to own pool ..."

cardano-cli shelley stake-address delegation-certificate \
    --stake-verification-key-file $stake_vkey_keypath \
    --cold-verification-key-file $pool_cold_vrf_keypath \
    --out-file $pool_own_delegation_cert_path

if [ $?	!= 0 ]; then
    error_msg "Error during delegation certificate creation"
    exit 1
fi

# Register the pool online

info_msg "Registering pool ..."

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
    --tx-out-count 1 \
    --ttl $ttl \
    --testnet-magic $testnet_magic \
    --signing-key-file $user1_payment_signing_keypath \
    --signing-key-file $pool_cold_signing_keypath \
    --signing-key-file $stake_skey_keypath \
    --certificate $pool_registartion_cert_path \
    --certificate $pool_own_delegation_cert_path \
    --protocol-params-file $protocol_params_filepath \
    | awk '{ print $2}')

if [ $? != 0 ]; then
    error_msg "Error during fee calculation"
    exit 1
fi

info_msg "Pool registration certificate fee: $fee"


# Pool deposit

info_msg "Obtaining pool deposit ..."

pool_deposit=$(get_pool_deposit)

if [ $?	!= 0 ]; then
    error_msg "Error when obtaining pool deposit"
    exit 1
fi

info_msg "Pool deposit: $pool_deposit"

# By default use address 1 as source address

tx=$(get_tx_info_for_address $from_address)
input=$(get_input_for_tx $tx)
balance=$(get_balance_for_tx $tx)
change=$(( balance - pool_deposit - fee ))


info_msg "TX Info:"
info_msg "Input: $input"
info_msg "Balance: $balance"
info_msg "Change: $change"
info_msg "Sending $(( pool_deposit + fee )) lovelaces to register pool"

# Build raw TX

info_msg "Building raw TX ..."

cardano-cli shelley transaction build-raw \
    --tx-in $input \
    --tx-out "${from_address}+${change}" \
    --ttl $ttl \
    --fee $fee \
    --certificate $pool_registartion_cert_path \
    --certificate $pool_own_delegation_cert_path \
    --tx-body-file $raw_tx_pool_registration_filepath

# ISSUE with incorrect return code = 1 for success
if [ $?	== 1 ]; then
    error_msg "Error when building raw transaction"
    exit 1
fi

# Sign TX

info_msg "Signing TX ..."

cardano-cli shelley transaction sign \
    --tx-body-file $raw_tx_pool_registration_filepath \
    --signing-key-file $user1_payment_signing_keypath \
    --signing-key-file $pool_cold_signing_keypath \
    --signing-key-file $stake_skey_keypath \
    --testnet-magic $testnet_magic \
    --tx-file $signed_tx_pool_registration_filepath

# ISSUE with incorrect return code = 1 for success
if [ $?	== 1 ]; then
    error_msg "Error when signing transaction"
    exit 1
fi

# Submit TX

info_msg "Submitting TX ..."

cardano-cli shelley transaction submit \
    --tx-file $signed_tx_pool_registration_filepath \
    --testnet-magic $testnet_magic

# ISSUE with incorrect return code = 1 for success
if [ $?	== 1 ]; then
    error_msg "Error when submitting transaction"
    exit 1
fi

############################### TMUX SESSION ##################################

pool_topology_filepath=$pool_dirpath/topology.json
pool_db_path=$pool_dirpath/db
pool_socket_path=$pool_dirpath/db/node.socket

relay_topology_filepath=$relay_dirpath/topology.json
relay_db_path=$relay_dirpath/db
relay_socket_path=$relay_dirpath/db/node.socket

info_msg "Starting pool and relay nodes in tmux session ..."

tmux has-session -t dev

if [ $? != 0 ]; then
    tmux new-session -s dev -n "Cardano Pool & Relay Nodes" -d
    tmux split-window -h -t dev:0
    tmux send-keys -t dev:0.0 "cardano-node run --config example/configuration.yaml --topology $pool_topology_filepath --database-path $pool_db_path --socket-path $pool_socket_path --shelley-kes-key $pool_hot_kes_signing_keypath --shelley-vrf-key $pool_hot_vrf_signing_keypath --shelley-operational-certificate $pool_operational_certificate_path --port $local_relay_port" C-m
    tmux send-keys -t dev:0.1 "cardano-node run --config example/configuration.yaml --topology $relay_topology_filepath --database-path $relay_db_path --socket-path $relay_socket_path --port $local_block_producer_port" C-m
    tmux select-window -t dev:0
    tmux attach -t dev
fi
