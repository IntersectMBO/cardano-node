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

UTXO_DIR=example/utxo-keys
DELEGATE_DIR=example/delegate-keys
GENESIS_DIR=example/genesis-keys
TRANSACTIONS_DIR=example/transactions

mkdir -p "$TRANSACTIONS_DIR"

epoch=$(cardano-cli query tip --testnet-magic 42 | jq .epoch)
epochplusone=$(($epoch + 1))

cardano-cli governance create-update-proposal \
--genesis-verification-key-file "${GENESIS_DIR}/genesis1.vkey" \
--genesis-verification-key-file "${GENESIS_DIR}/genesis2.vkey" \
--epoch $epochplusone \
--protocol-major-version "9" \
--protocol-minor-version "0" \
--out-file "${TRANSACTIONS_DIR}/update-v9.proposal"

cardano-cli babbage transaction build \
--tx-in "$(cardano-cli query utxo --address $(cat ${UTXO_DIR}/payment1.addr) --output-json | jq -r 'keys[]')" \
--change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
--update-proposal-file "${TRANSACTIONS_DIR}/update-v9.proposal" \
--out-file "${TRANSACTIONS_DIR}/update-v9-proposaltx.raw"

cardano-cli babbage transaction sign \
--tx-body-file "${TRANSACTIONS_DIR}/update-v9-proposaltx.raw" \
--signing-key-file "${UTXO_DIR}/payment1.skey" \
--signing-key-file "${DELEGATE_DIR}/delegate1.skey" \
--signing-key-file "${DELEGATE_DIR}/delegate2.skey" \
--out-file "${TRANSACTIONS_DIR}/update-v9-proposaltx.signed"

cardano-cli babbage transaction submit \
--tx-file "${TRANSACTIONS_DIR}/update-v9-proposaltx.signed"