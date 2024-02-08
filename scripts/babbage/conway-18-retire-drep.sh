#!/usr/bin/env bash

# Use this script after conway-setup-new-delegator-keys.sh
# Run this script to register Dreps and Delegate to Dreps.

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
DREP_DIR=example/dreps
UTXO_DIR=example/utxo-keys
TRANSACTIONS_DIR=example/transactions
POOL_DIR=example/pools

mkdir -p "$TRANSACTIONS_DIR"
mkdir -p "$DREP_DIR"

drepDeposit="$($CARDANO_CLI conway query protocol-parameters --testnet-magic $NETWORK_MAGIC | jq .dRepDeposit)"

# GENERATE AND SUBMIT RETIREMENT CERTIFICATE

$CARDANO_CLI conway governance drep retirement-certificate \
--drep-verification-key-file "${DREP_DIR}/drep1.vkey" \
--deposit-amt $drepDeposit \
--out-file "${TRANSACTIONS_DIR}/drep1-retire.cert"

balanceBeforeRetirement=$(cardano-cli query utxo --address "$(cat example/utxo-keys/payment1.addr)" --testnet-magic 42 --out-file /dev/stdout | jq -r 'to_entries | .[0] | .value.value.lovelace')
echo "$balanceBeforeRetirement"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/drep1-retire.cert" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/drep-retire-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/drep-retire-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --out-file "${TRANSACTIONS_DIR}/drep1-retire-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/drep1-retire-tx.signed"

cardano-cli query tip --testnet-magic 42

sleep 10

balanceAfterRetirement=$(cardano-cli query utxo --address "$(cat example/utxo-keys/payment1.addr)" --testnet-magic 42 --out-file /dev/stdout | jq -r 'to_entries | .[0] | .value.value.lovelace')
echo "$balanceAfterRetirement"

echo $((balanceAfterRetirement - balanceBeforeRetirement ))
cardano-cli conway query drep-state --drep-verification-key-file example/dreps/drep1.vkey --testnet-magic 42
