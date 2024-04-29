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

cardano-cli conway governance committee create-cold-key-resignation-certificate \
--cold-script-hash "$(cat ${CC_MULTISIG}/multisig.hash)" \
--out-file $TRANSACTIONS_DIR/ccresignation.cert

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${CC_MULTISIG}/multisigscript.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "$TRANSACTIONS_DIR/ccresignation.cert" \
  --certificate-script-file "${CC_MULTISIG}/multisig.json" \
  --witness-override 3 \
  --out-file "${TRANSACTIONS_DIR}/resignation-cert-tx.raw"

cardano-cli transaction witness \
  --tx-body-file "${TRANSACTIONS_DIR}/resignation-cert-tx.raw" \
  --signing-key-file "${CC_MULTISIG}/cold1-multisig-cc.skey" \
  --testnet-magic 42 \
  --out-file  "${TRANSACTIONS_DIR}/key1witness"

cardano-cli transaction witness \
  --tx-body-file "${TRANSACTIONS_DIR}/resignation-cert-tx.raw" \
  --signing-key-file "${CC_MULTISIG}/cold2-multisig-cc.skey" \
  --testnet-magic 42 \
  --out-file  "${TRANSACTIONS_DIR}/key2witness"

cardano-cli transaction assemble \
  --tx-body-file "${TRANSACTIONS_DIR}/resignation-cert-tx.raw" \
  --witness-file "${TRANSACTIONS_DIR}/key1witness" \
  --witness-file "${TRANSACTIONS_DIR}/key2witness" \
  --out-file "${TRANSACTIONS_DIR}/spendMultiSig"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/spendMultiSig"