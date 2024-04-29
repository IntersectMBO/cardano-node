#!/usr/bin/env bash

# Use this script to submit a governance action that adds 3 new committe members and change
# quorum to 2/3.

# This scripts uses set -x to show in terminal the commands executed by the script.
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
DREP_DIR=example/dreps
UTXO_DIR=example/utxo-keys
POOL_DIR=example/pools
TRANSACTIONS_DIR=example/transactions
CC_DIR=example/cc
CC_MULTISIG=example/cc/multisig

mkdir -p "$TRANSACTIONS_DIR"
mkdir -p "$CC_DIR"

# ----------------------

cardano-cli conway query committee-state

# Issue Authorization certificates from cold to hot keys

for i in {1..3}; do
  cardano-cli conway governance committee key-gen-hot \
    --verification-key-file "${CC_DIR}/hot${i}-cc.vkey" \
    --signing-key-file "${CC_DIR}/hot${i}-cc.skey"
done

for i in {1..3}; do
  cardano-cli conway governance committee create-hot-key-authorization-certificate \
    --cold-verification-key-file "${CC_DIR}/cold${i}-cc.vkey" \
    --hot-key-file "${CC_DIR}/hot1-cc.vkey" \
    --out-file "${CC_DIR}/cc${i}-authorization.cert"
done

sleep 5

cardano-cli conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${CC_DIR}/cc1-authorization.cert" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/cc1-auth-cert-tx.raw"

cardano-cli conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/cc1-auth-cert-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${CC_DIR}/cold1-cc.skey" \
  --out-file "${TRANSACTIONS_DIR}/cc1-auth-cert-tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/cc1-auth-cert-tx.signed"

sleep 15

cardano-cli conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${CC_DIR}/cc2-authorization.cert" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/cc2-auth-cert-tx.raw"

cardano-cli conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/cc2-auth-cert-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${CC_DIR}/cold2-cc.skey" \
  --out-file "${TRANSACTIONS_DIR}/cc2-auth-cert-tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/cc2-auth-cert-tx.signed"

# sleep 5
