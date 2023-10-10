#!/usr/bin/env bash

# This scripts uses set -x to show in terminal the commands executed by the script.
# The "exec 2>" below this comment helps the user to differenciate between the commands and its outputs by changing the color
# of the set -x output (the commands).

exec 2> >(while IFS= read -r line; do echo -e "\e[34m${line}\e[0m" >&2; done)

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/

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

set -x

CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
NETWORK_MAGIC=42
ROOT=example
DREP_DIR=example/dreps
UTXO_DIR=example/utxo-keys
POOL_DIR=example/pools
TRANSACTIONS_DIR=example/transactions

mkdir -p "$TRANSACTIONS_DIR"

wget https://tinyurl.com/2p8fjbnf -O "${ROOT}/govActionJustification.txt"

$CARDANO_CLI conway governance action create-info --testnet \
  --governance-action-deposit 0 \
  --stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
  --proposal-url https://tinyurl.com/2p8fjbnf \
  --proposal-file "${ROOT}/govActionJustification.txt" \
  --out-file ${TRANSACTIONS_DIR}/info.action

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address $(cat "${UTXO_DIR}/payment1.addr") --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --proposal-file "${TRANSACTIONS_DIR}/info.action" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/info-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/info-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --out-file "${TRANSACTIONS_DIR}/info-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/info-tx.signed"

sleep 5

IDIX="$($CARDANO_CLI conway governance query gov-state --testnet-magic 42 | jq -r '.gov.curGovSnapshots.psGovActionStates | keys[0]')"
ID="${IDIX%#*}"  # This removes everything from the last # to the end
IX="${IDIX##*#}"   # This removes everything up to and including $ID

### ---------
# DREP VOTES
### ---------

for i in {1..3}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/${ID}-drep${i}.vote"

  $CARDANO_CLI conway transaction build \
    --testnet-magic $NETWORK_MAGIC \
    --tx-in "$(cardano-cli query utxo --address $(cat "${UTXO_DIR}/payment1.addr") --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
    --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
    --vote-file "${TRANSACTIONS_DIR}/${ID}-drep${i}.vote" \
    --witness-override 2 \
    --out-file "${TRANSACTIONS_DIR}/${ID}-drep${i}-tx.raw"

  $CARDANO_CLI conway transaction sign \
    --testnet-magic $NETWORK_MAGIC \
    --tx-body-file "${TRANSACTIONS_DIR}/${ID}-drep${i}-tx.raw" \
    --signing-key-file "${UTXO_DIR}/payment1.skey" \
    --signing-key-file "${DREP_DIR}/drep${i}.skey" \
    --out-file "${TRANSACTIONS_DIR}/${ID}-drep${i}-tx.signed"

  $CARDANO_CLI conway transaction submit \
    --testnet-magic $NETWORK_MAGIC \
    --tx-file "${TRANSACTIONS_DIR}/${ID}-drep${i}-tx.signed"
sleep 3
done

### ----------
# SPO VOTES
### ----------

for i in {1..3}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cold-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${POOL_DIR}/${ID}-spo${i}.vote"

  $CARDANO_CLI conway transaction build \
    --testnet-magic $NETWORK_MAGIC \
    --tx-in "$(cardano-cli query utxo --address $(cat "${UTXO_DIR}/payment1.addr") --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
    --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
    --vote-file "${POOL_DIR}/${ID}-spo${i}.vote" \
    --witness-override 2 \
    --out-file "${UTXO_DIR}/${ID}-spo${i}-tx.raw"

  $CARDANO_CLI conway transaction sign \
    --testnet-magic $NETWORK_MAGIC \
    --tx-body-file "${UTXO_DIR}/${ID}-spo${i}-tx.raw" \
    --signing-key-file "${UTXO_DIR}/payment1.skey" \
    --signing-key-file "${POOL_DIR}/cold${i}.skey" \
    --out-file "${UTXO_DIR}/${ID}-spo${i}-tx.signed"

  $CARDANO_CLI conway transaction submit \
    --testnet-magic $NETWORK_MAGIC \
    --tx-file "${UTXO_DIR}/${ID}-spo${i}-tx.signed"

  sleep 3
done

$CARDANO_CLI conway governance query gov-state --testnet-magic $NETWORK_MAGIC | jq -r '.gov.curGovActionsState'

expiresAfter=$(cardano-cli conway governance query gov-state --testnet-magic 42 | jq -r '.gov.curGovActionsState[].expiresAfter')

echo "THIS INFO GOVERNANCE ACTION EXPIRES AFTER EPOCH ${expiresAfter}; SINCE INFO ACTIONS ARE NOT REALLY ENACTED BY LEDGER, AFTER IT EXPIRES IT IS REMOVED"
echo "IT WILL BE REMOVED ON EPOCH $(($expiresAfter + 1 ))"


check_epoch() {
  while true; do
    currentEpoch=$($CARDANO_CLI conway query tip --testnet-magic $NETWORK_MAGIC | jq .epoch)

    if [ "$currentEpoch" -gt $(($expiresAfter + 1)) ]; then
      $CARDANO_CLI conway governance query gov-state --testnet-magic $NETWORK_MAGIC | jq -r '.gov.curGovActionsState'
      break
    else
      sleep 30  # Sleep when the epoch hasn't changed
    fi
  done
}

# Call the function to check the epoch
check_epoch