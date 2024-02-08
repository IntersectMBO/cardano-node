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
CC_DIR=example/cc

mkdir -p "$TRANSACTIONS_DIR"

wget https://tinyurl.com/3wrwb2as -O "${TRANSACTIONS_DIR}/govActionJustification.txt"

govActDeposit=$($CARDANO_CLI conway query gov-state --testnet-magic $NETWORK_MAGIC | jq .enactState.curPParams.govActionDeposit)
proposalHash="$($CARDANO_CLI conway governance hash --file-text ${TRANSACTIONS_DIR}/govActionJustification.txt)"

$CARDANO_CLI conway governance action create-info --testnet \
  --governance-action-deposit "$govActDeposit" \
  --stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
  --anchor-url https://tinyurl.com/3wrwb2as  \
  --anchor-data-hash "$proposalHash" \
  --out-file ${TRANSACTIONS_DIR}/info.action

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
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

ID="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals.[].actionId.txId')"
IX="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals.[].actionId.govActionIx')"

### ---------
# DREP VOTES
### ---------

for i in {1..1}; do
  $CARDANO_CLI conway governance vote create \
    --no \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/info-drep${i}.vote"
done

for i in {2..2}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/info-drep${i}.vote"
done


for i in {3..3}; do
  $CARDANO_CLI conway governance vote create \
    --abstain \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/info-drep${i}.vote"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/info-drep1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/info-drep2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/info-drep3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/info-drep-votes-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/info-drep-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --signing-key-file "${DREP_DIR}/drep2.skey" \
  --signing-key-file "${DREP_DIR}/drep3.skey" \
  --out-file "${TRANSACTIONS_DIR}/info-drep-votes-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/info-drep-votes-tx.signed"

sleep 5

### ----------
# SPO VOTES
### ----------

for i in {1..1}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cold-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/info-spo${i}.vote"
done

for i in {2..2}; do
  $CARDANO_CLI conway governance vote create \
    --no \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cold-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/info-spo${i}.vote"
done

for i in {3..3}; do
  $CARDANO_CLI conway governance vote create \
    --abstain \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cold-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/info-spo${i}.vote"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/info-spo1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/info-spo2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/info-spo3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/info-spo-votes-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/info-spo-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${POOL_DIR}/cold1.skey" \
  --signing-key-file "${POOL_DIR}/cold2.skey" \
  --signing-key-file "${POOL_DIR}/cold3.skey" \
  --out-file "${TRANSACTIONS_DIR}/info-spo-votes-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/info-spo-votes-tx.signed"

sleep 5


### ----------––––––––
# CC VOTES
### ----------––––––––

for i in {1..1}; do
  $CARDANO_CLI conway governance vote create \
    --abstain \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cc-hot-verification-key-file "${CC_DIR}/hot${i}-cc.vkey" \
    --out-file "${TRANSACTIONS_DIR}/info-cc${i}.vote"
done

for i in {2..2}; do
  $CARDANO_CLI conway governance vote create \
    --no \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cc-hot-verification-key-file "${CC_DIR}/hot${i}-cc.vkey" \
    --out-file "${TRANSACTIONS_DIR}/info-cc${i}.vote"
done

for i in {3..3}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cc-hot-verification-key-file "${CC_DIR}/hot${i}-cc.vkey" \
    --out-file "${TRANSACTIONS_DIR}/info-cc${i}.vote"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/info-cc1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/info-cc2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/info-cc3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/info-cc-votes-tx.raw"

  $CARDANO_CLI conway transaction sign \
    --testnet-magic $NETWORK_MAGIC \
    --tx-body-file "${TRANSACTIONS_DIR}/info-cc-votes-tx.raw" \
    --signing-key-file "${UTXO_DIR}/payment1.skey" \
    --signing-key-file "${CC_DIR}/hot1-cc.skey" \
    --signing-key-file "${CC_DIR}/hot2-cc.skey" \
    --signing-key-file "${CC_DIR}/hot3-cc.skey" \
    --out-file "${TRANSACTIONS_DIR}/info-cc-votes-tx.signed"

  $CARDANO_CLI conway transaction submit \
    --testnet-magic $NETWORK_MAGIC \
    --tx-file "${TRANSACTIONS_DIR}/info-cc-votes-tx.signed"

  sleep 5

expiresAfter=$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals.[].expiresAfter')

echo "THIS INFO GOVERNANCE ACTION EXPIRES AFTER EPOCH ${expiresAfter}; SINCE INFO ACTIONS ARE NOT REALLY ENACTED BY LEDGER, AFTER IT EXPIRES IT IS REMOVED"
echo "IT WILL BE REMOVED ON EPOCH $(($expiresAfter + 1 ))"

tip=$(cardano-cli query tip --testnet-magic 42 | jq .)
current_epoch=$(echo $tip | jq .epoch)
slots_to_epoch_end=$(echo $tip | jq .slotsToEpochEnd)

sleep $((60 * (expiresAfter - current_epoch) + slots_to_epoch_end / 10))

$CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals'

echo "After one more epoch the info action is removed from the state"

sleep 62

$CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals'