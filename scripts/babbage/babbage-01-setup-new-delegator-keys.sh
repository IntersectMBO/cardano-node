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

for i in {1..3}; do
cardano-cli babbage address key-gen \
  --verification-key-file "${UTXO_DIR}/payment${i}.vkey" \
  --signing-key-file "${UTXO_DIR}/payment${i}.skey"

done

# GENERATE NEW STAKE KEYS

for i in {1..3}; do
cardano-cli babbage stake-address key-gen \
  --verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
  --signing-key-file "${UTXO_DIR}/stake${i}.skey"
done

# BUILD ADDRESSES FOR OUR NEW KEYS
for i in {1..3}; do
  cardano-cli babbage address build \
    --payment-verification-key-file "${UTXO_DIR}/payment${i}.vkey" \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --out-file  "${UTXO_DIR}/payment${i}.addr"
done

for i in {1..3}; do
  cardano-cli babbage stake-address build \
  --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
  --testnet-magic $NETWORK_MAGIC \
  --out-file "${UTXO_DIR}/stake${i}.addr"
done

# BUILD ADDRESSES FOR THE EXISTING KEYS, WE WILL NEED THEM FOR OUT FUTURE TRANSACTIONS

for i in {1..3}; do
  cardano-cli babbage address build \
    --payment-verification-key-file "${UTXO_DIR}/utxo${i}.vkey" \
    --out-file  "${UTXO_DIR}/utxo${i}.addr"
done

# --------------------
# FUND OUR NEWLY CREATED ADDRESSES

cardano-cli babbage transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/utxo1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/utxo2.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/utxo3.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --tx-out "$(cat ${UTXO_DIR}/payment1.addr)+500000000000" \
  --tx-out "$(cat ${UTXO_DIR}/payment2.addr)+500000000000" \
  --tx-out "$(cat ${UTXO_DIR}/payment3.addr)+500000000000" \
  --change-address "$(cat ${UTXO_DIR}/utxo1.addr)" \
  --out-file "${TRANSACTIONS_DIR}/tx.raw"

cardano-cli babbage transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/tx.raw" \
  --signing-key-file "${UTXO_DIR}/utxo1.skey" \
  --signing-key-file "${UTXO_DIR}/utxo2.skey" \
  --signing-key-file "${UTXO_DIR}/utxo3.skey" \
  --out-file "${TRANSACTIONS_DIR}/tx.signed"

cardano-cli babbage transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/tx.signed"

sleep 15

# SHOW THE UTXO DISTRIBUTION

cardano-cli babbage query utxo --whole-utxo 

sleep 10

# REGISTER STAKE ADDRESSES

for i in {1..3}; do
  cardano-cli babbage stake-address registration-certificate \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/stake${i}reg.cert"
done

cardano-cli babbage transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/stake1reg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/stake2reg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/stake3reg.cert" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/reg-stake-tx.raw"

cardano-cli babbage transaction sign --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/reg-stake-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --signing-key-file "${UTXO_DIR}/stake2.skey" \
  --signing-key-file "${UTXO_DIR}/stake3.skey" \
  --out-file "${TRANSACTIONS_DIR}/reg-stake-tx.signed"

cardano-cli babbage transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/reg-stake-tx.signed"

sleep 10

# DELEGATE STAKE KEYS TO POOLS

for i in {1..3}; do
  cardano-cli babbage stake-address stake-delegation-certificate \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --stake-pool-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/stake${i}-pool-deleg.cert"
done

cardano-cli babbage transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/stake1-pool-deleg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/stake2-pool-deleg.cert" \
  --certificate-file "${TRANSACTIONS_DIR}/stake3-pool-deleg.cert" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/stake-delegation-tx.raw"

cardano-cli babbage transaction sign --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/stake-delegation-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --signing-key-file "${UTXO_DIR}/stake2.skey" \
  --signing-key-file "${UTXO_DIR}/stake3.skey" \
  --out-file "${TRANSACTIONS_DIR}/stake-delegation-tx.raw"

cardano-cli babbage transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/stake-delegation-tx.raw"

sleep 5
