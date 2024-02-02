#!/usr/bin/env bash

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
UTXO_DIR=example/utxo-keys
TRANSACTIONS_DIR=example/transactions
CC_DIR=example/cc

mkdir -p "$TRANSACTIONS_DIR"
mkdir -p "$CC_DIR"

$CARDANO_CLI conway governance committee create-cold-key-resignation-certificate \
  --cold-verification-key-file "${CC_DIR}/cold1-cc.vkey" \
  --out-file "${CC_DIR}/cold1-resignation.cert"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${CC_DIR}/cold1-resignation.cert" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/cold1-resignation-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/cold1-resignation-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${CC_DIR}/cold1-cc.skey" \
  --out-file "${TRANSACTIONS_DIR}/cold1-resignation-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/cold1-resignation-tx.signed"

sleep 5

$CARDANO_CLI conway query committee-state --testnet-magic $NETWORK_MAGIC 

sleep 5

$CARDANO_CLI conway governance committee key-gen-hot \
  --verification-key-file "${CC_DIR}/hot${i}-cc.vkey" \
  --signing-key-file "${CC_DIR}/hot1-cc.skey"

$CARDANO_CLI conway governance committee create-hot-key-authorization-certificate \
  --cold-verification-key-file "${CC_DIR}/cold${i}-cc.vkey" \
  --hot-key-file "${CC_DIR}/hot1-cc.vkey" \
  --out-file "${CC_DIR}/hot-key1-cc-authorization.cert"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${CC_DIR}/hot-key1-cc-authorization.cert" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/authorization1-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/authorization1-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${CC_DIR}/cold${i}-cc.skey" \
  --out-file "${TRANSACTIONS_DIR}/authorization1-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/authorization1-tx.signed"

# ^ this should show an error ConwayCommitteeHasPreviouslyResigned