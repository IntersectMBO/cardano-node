#!/usr/bin/env bash

# Run this script after mkfiles.sh
# This scripts uses set -x to show in terminal the commands executed by the script. Remove or comment set -x to disable this behavior
# The "exec 2>" below this comment helps the user to differenciate between the commands and its outputs by changing the color
# of the set -x output (the commands).

exec 2> >(while IFS= read -r line; do echo -e "\e[34m${line}\e[0m" >&2; done)

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -x
set -euo pipefail

UNAME=$(uname -s) SED=
case $UNAME in
  Darwin )      SED="gsed";;
  Linux )       SED="sed";;
esac

sprocket() {
  if [ "$UNAME" == "Windows_NT" ]; then
    # Named pipes names on Windows must have the structure: "\\.\pipe\PipeName"
    # See https://docs.microsoft.com/en-us/windows/win32/ipc/pipe-names
    echo -n '\\.\pipe\'
    echo "$1" | sed 's|/|\\|g'
  else
    echo "$1"
  fi
}

CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
NETWORK_MAGIC=42
UTXO_DIR=example/utxo-keys
POOL_DIR=example/pools
TRANSACTIONS_DIR=example/transactions

mkdir -p "$TRANSACTIONS_DIR"

# ----------------------

# GENERATE NEW PAYMENT KEYS

for i in {5..5}; do
$CARDANO_CLI conway address key-gen \
  --verification-key-file "${UTXO_DIR}/payment${i}.vkey" \
  --signing-key-file "${UTXO_DIR}/payment${i}.skey"

done

# GENERATE NEW STAKE KEYS

for i in {5..5}; do
$CARDANO_CLI conway stake-address key-gen \
  --verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
  --signing-key-file "${UTXO_DIR}/stake${i}.skey"

done

# BUILD ADDRESSES FOR OUR NEW KEYS
for i in {5..5}; do
  $CARDANO_CLI conway address build \
    --testnet-magic $NETWORK_MAGIC \
    --payment-verification-key-file "${UTXO_DIR}/payment${i}.vkey" \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --out-file  "${UTXO_DIR}/payment${i}.addr"
done

for i in {5..5}; do
  $CARDANO_CLI stake-address build \
  --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
  --testnet-magic $NETWORK_MAGIC \
  --out-file "${UTXO_DIR}/stake${i}.addr"
done
# BUILD ADDRESSES FOR THE EXISTING KEYS, WE WILL NEED THEM FOR OUT FUTURE TRANSACTIONS
# --------------------
# FUND OUR NEWLY CREATED ADDRESSES

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/utxo1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --tx-out "$(cat ${UTXO_DIR}/payment5.addr)+25000000" \
  --change-address "$(cat ${UTXO_DIR}/utxo1.addr)" \
  --out-file "${TRANSACTIONS_DIR}/tx.raw"

$CARDANO_CLI conway transaction sign --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/tx.raw" \
  --signing-key-file "${UTXO_DIR}/utxo1.skey" \
  --out-file "${TRANSACTIONS_DIR}/tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/tx.signed"

sleep 5

# SHOW THE UTXO DISTRIBUTION

$CARDANO_CLI conway query utxo --whole-utxo --testnet-magic $NETWORK_MAGIC

sleep 5

# REGISTER STAKE ADDRESSES

for i in {5..5}; do
  $CARDANO_CLI conway stake-address registration-certificate \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --key-reg-deposit-amt 0 \
    --out-file "${TRANSACTIONS_DIR}/stake${i}reg.cert"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment5.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment5.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/stake5reg.cert" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/reg-stake-tx.raw"

$CARDANO_CLI conway transaction sign --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/reg-stake-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment5.skey" \
  --signing-key-file "${UTXO_DIR}/stake5.skey" \
  --out-file "${TRANSACTIONS_DIR}/reg-stake-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/reg-stake-tx.signed"

sleep 5

# DELEGATE STAKE KEYS TO POOLS

for i in {5..5}; do
  $CARDANO_CLI conway stake-address stake-delegation-certificate \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --stake-pool-verification-key-file "${POOL_DIR}/cold1.vkey" \
    --out-file "${TRANSACTIONS_DIR}/stake${i}-pool-deleg.cert"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment5.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment5.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/stake5-pool-deleg.cert" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/stake-delegation-tx.raw"

$CARDANO_CLI conway transaction sign --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/stake-delegation-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment5.skey" \
  --signing-key-file "${UTXO_DIR}/stake5.skey" \
  --out-file "${TRANSACTIONS_DIR}/stake-delegation-tx.raw"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/stake-delegation-tx.raw"

sleep 500

rewardsBalance="$($CARDANO_CLI conway query stake-address-info --testnet-magic 42 --address "$(cat ${UTXO_DIR}/stake5.addr)" | jq -r '.[].rewardAccountBalance')"
echo "$rewardsBalance"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment5.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --withdrawal "$(cat "${UTXO_DIR}/stake5.addr")"+"$rewardsBalance" \
  --change-address "$(cat ${UTXO_DIR}/payment5.addr)" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/withdrawrewards-tx.raw"

$CARDANO_CLI conway transaction sign --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/withdrawrewards-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment5.skey" \
  --signing-key-file "${UTXO_DIR}/stake5.skey" \
  --out-file "${TRANSACTIONS_DIR}/withdrawrewards-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/withdrawrewards-tx.signed"

sleep 5

rewardsBalance="$($CARDANO_CLI conway query stake-address-info --testnet-magic 42 --address "$(cat ${UTXO_DIR}/stake5.addr)" | jq -r '.[].rewardAccountBalance')"
echo "$rewardsBalance"
