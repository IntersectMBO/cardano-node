#!/usr/bin/env bash

# This scripts uses set -x to show in terminal the commands executed by the script.
# The "exec 2>" below this comment helps the user to differenciate between the commands and its outputs by changing the color
# of the set -x output (the commands).

exec 2> >(while IFS= read -r line; do echo -e "\e[34m${line}\e[0m" >&2; done)


set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -x
set -o pipefail

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

mkdir -p "$TRANSACTIONS_DIR"
mkdir -p "$CC_DIR"

# --------------

wget https://tinyurl.com/3wrwb2as -O "${TRANSACTIONS_DIR}/govActionJustification.txt"

previousId="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.enactState.prevGovActionIds.pgaCommittee.txId')"
previousIx="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.enactState.prevGovActionIds.pgaCommittee.govActionIx')"

govActDeposit=$($CARDANO_CLI conway query gov-state --testnet-magic $NETWORK_MAGIC | jq .enactState.curPParams.govActionDeposit)

$CARDANO_CLI conway governance action create-no-confidence \
  --testnet \
  --governance-action-deposit "$govActDeposit" \
  --governance-action-tx-id "${previousId}" \
  --governance-action-index "${previousIx}" \
  --stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
  --proposal-anchor-url https://tinyurl.com/3wrwb2as \
  --proposal-anchor-metadata-file "${TRANSACTIONS_DIR}/govActionJustification.txt" \
  --out-file "${TRANSACTIONS_DIR}/no-confidence.action"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --proposal-file "${TRANSACTIONS_DIR}/no-confidence.action" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/no-confidence-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/no-confidence-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --out-file "${TRANSACTIONS_DIR}/no-confidence-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/no-confidence-tx.signed"

sleep 5

# LETS FIND THE ACTION ID

IDIX="$($CARDANO_CLI conway governance query gov-state --testnet-magic 42 | jq -r '.proposals.psGovActionStates | keys[0]')" # This assumes this is the only governance action in transit
ID="${IDIX%#*}"  # This removes everything from the last # to the end
IX="${IDIX##*#}"   # This removes everything up to and including $ID

# LETS VOTE AS DREPS AND AS SPOS

### ----------––––––––
# DREP VOTES
### ----------––––––––

for i in {1..1}; do
  $CARDANO_CLI conway governance vote create \
    --abstain \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/no-confidence-drep${i}.vote"
done

for i in {2..2}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/no-confidence-drep${i}.vote"
done

for i in {3..3}; do
  $CARDANO_CLI conway governance vote create \
    --no \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/no-confidence-drep${i}.vote"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/no-confidence-drep1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/no-confidence-drep2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/no-confidence-drep3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/no-confidence-drep-votes-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/no-confidence-drep-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --signing-key-file "${DREP_DIR}/drep2.skey" \
  --signing-key-file "${DREP_DIR}/drep3.skey" \
  --out-file "${TRANSACTIONS_DIR}/no-confidence-drep-votes-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/no-confidence-drep-votes-tx.signed"

sleep 5

### ----------––––––––
# SPO VOTES
### ----------––––––––

for i in {1..1}; do
  $CARDANO_CLI conway governance vote create \
    --abstain \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cold-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/no-confidence-spo${i}.vote"
done

for i in {2..2}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cold-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/no-confidence-spo${i}.vote"
done

for i in {3..3}; do
  $CARDANO_CLI conway governance vote create \
    --no \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cold-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/no-confidence-spo${i}.vote"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/no-confidence-spo1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/no-confidence-spo2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/no-confidence-spo3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/no-confidence-spos-vote-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/no-confidence-spos-vote-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${POOL_DIR}/cold1.skey" \
  --signing-key-file "${POOL_DIR}/cold2.skey" \
  --signing-key-file "${POOL_DIR}/cold3.skey" \
  --out-file "${TRANSACTIONS_DIR}/no-confidence-spos-vote-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/no-confidence-spos-vote-tx.signed"

sleep 5

expiresAfter=$($CARDANO_CLI conway governance query gov-state --testnet-magic 42 | jq -r '.proposals.psGovActionStates[] | .expiresAfter')

echo "ONCE THE VOTING PERIOD ENDS ON EPOCH ${expiresAfter}, WE SHOULD SEE COMMITTEE EMPTY IN GOV STATE"

tip=$(cardano-cli query tip --testnet-magic 42 | jq .)
current_epoch=$(echo $tip | jq .epoch)
slots_to_epoch_end=$(echo $tip | jq .slotsToEpochEnd)

sleep $((60 * (expiresAfter - current_epoch) + slots_to_epoch_end / 10))

$CARDANO_CLI conway governance query gov-state --testnet-magic $NETWORK_MAGIC | jq -r '.ratify.committee'