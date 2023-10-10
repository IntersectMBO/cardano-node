#!/usr/bin/env bash

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
ROOT=example
DREP_DIR=example/dreps
UTXO_DIR=example/utxo-keys
POOL_DIR=example/pools
TRANSACTIONS_DIR=example/transactions

mkdir -p "$TRANSACTIONS_DIR"
mkdir -p "$DREP_DIR"

# GENERATE DREP KEYS

for i in {1..3}; do
  $CARDANO_CLI conway governance drep key-gen \
    --verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --signing-key-file "${DREP_DIR}/drep${i}.skey"
done

# GENERATE AND SUBMIT REGISTRATION CERTIFICATES

for i in {1..3}; do
  $CARDANO_CLI conway governance drep registration-certificate \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --key-reg-deposit-amt 0 \
    --out-file "${TRANSACTIONS_DIR}/drep${i}-reg.cert"

  $CARDANO_CLI conway transaction build \
    --testnet-magic $NETWORK_MAGIC \
    --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment${i}.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
    --change-address "$(cat ${UTXO_DIR}/payment${i}.addr)" \
    --certificate-file "${TRANSACTIONS_DIR}/drep${i}-reg.cert" \
    --witness-override 2 \
    --out-file "${TRANSACTIONS_DIR}/drep-reg-tx${i}.raw"

  $CARDANO_CLI conway transaction sign \
    --testnet-magic $NETWORK_MAGIC \
    --tx-body-file "${TRANSACTIONS_DIR}/drep-reg-tx${i}.raw" \
    --signing-key-file "${UTXO_DIR}/payment${i}.skey" \
    --signing-key-file "${DREP_DIR}/drep${i}.skey" \
    --out-file "${TRANSACTIONS_DIR}/drep-reg-tx${i}.signed"

  $CARDANO_CLI conway transaction submit \
    --testnet-magic $NETWORK_MAGIC \
    --tx-file "${TRANSACTIONS_DIR}/drep-reg-tx${i}.signed"
  sleep 3
done


# DELEGATE VOTES FROM STAKE ADDRESSES TO THE DREPS

for i in {1..3}; do
  $CARDANO_CLI conway stake-address vote-delegation-certificate \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/drep-deleg${i}.cert"

  $CARDANO_CLI conway transaction build \
    --testnet-magic $NETWORK_MAGIC \
    --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment${i}.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
    --change-address "$(cat ${UTXO_DIR}/payment${i}.addr)" \
    --certificate-file "${TRANSACTIONS_DIR}/drep-deleg${i}.cert" \
    --witness-override 3 \
    --out-file "${TRANSACTIONS_DIR}/drep-deleg-tx${i}.raw"

  $CARDANO_CLI conway transaction sign \
    --testnet-magic $NETWORK_MAGIC \
    --tx-body-file "${TRANSACTIONS_DIR}/drep-deleg-tx${i}.raw" \
    --signing-key-file "${UTXO_DIR}/payment${i}.skey" \
    --signing-key-file "${UTXO_DIR}/stake${i}.skey" \
    --signing-key-file "${DREP_DIR}/drep${i}.skey" \
    --out-file "${TRANSACTIONS_DIR}/drep-deleg-tx${i}.signed"

  $CARDANO_CLI conway transaction submit \
    --testnet-magic $NETWORK_MAGIC \
    --tx-file "${TRANSACTIONS_DIR}/drep-deleg-tx${i}.signed"

  sleep 3
done

# QUERY DREP STATE FOR EACH DREP

for i in {1..3}; do
  $CARDANO_CLI conway governance query drep-state \
  --testnet-magic "$NETWORK_MAGIC" \
  --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey"
done
