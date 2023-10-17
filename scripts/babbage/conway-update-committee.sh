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

mkdir -p "$TRANSACTIONS_DIR"
mkdir -p "$CC_DIR"

# ----------------------

# DOWNLOAD THE DUMMY PROPOSAL FILE
wget https://shorturl.at/asIJ6  -O "${TRANSACTIONS_DIR}/cc_proposal.txt"

# QUERY THE CURRENT EPOCH, WE WILL USE IT TO SET THE EXPIRATION DATE FOR OUR CC MEMEBER

epoch=$($CARDANO_CLI conway query tip --testnet-magic $NETWORK_MAGIC | jq -r .epoch)


# Create CC credentials

for i in {1..3}; do
  $CARDANO_CLI conway governance committee key-gen-cold \
    --cold-verification-key-file "${CC_DIR}/cold${i}-cc.vkey" \
    --cold-signing-key-file "${CC_DIR}/cold${i}-cc.skey"
done

# CREATE THE PROPOSAL

$CARDANO_CLI conway governance action create-new-committee \
  --testnet \
  --governance-action-deposit 0 \
  --stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
  --proposal-url https://shorturl.at/asIJ6 \
  --proposal-file "${TRANSACTIONS_DIR}/cc_proposal.txt" \
  --add-cc-cold-verification-key-file "${CC_DIR}/cold1-cc.vkey" \
  --epoch $((epoch + 200)) \
  --add-cc-cold-verification-key-file "${CC_DIR}/cold2-cc.vkey" \
  --epoch $((epoch + 175)) \
  --add-cc-cold-verification-key-file "${CC_DIR}/cold3-cc.vkey" \
  --epoch $((epoch + 150)) \
  --quorum 2/3 \
  --out-file "${TRANSACTIONS_DIR}/new-committee.action"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --proposal-file "${TRANSACTIONS_DIR}/new-committee.action" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/new-committee-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/new-committee-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --out-file "${TRANSACTIONS_DIR}/new-committee-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/new-committee-tx.signed"

sleep 5

# LETS FIND THE ACTION ID

IDIX="$($CARDANO_CLI conway governance query gov-state --testnet-magic 42 | jq -r '.gov.curGovSnapshots.psGovActionStates | keys[0]')" # This assumes it is the only governance action in transit
ID="${IDIX%#*}"  # This removes everything from the last # to the end
IX="${IDIX##*#}"   # This removes everything up to and including $ID

# LETS VOTE AS DREPS AND AS SPOS

### ----------––––––––
# DREP VOTES
### ----------––––––––

for i in {1..3}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/committee-drep${i}.vote"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/committee-drep1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/committee-drep2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/committee-drep3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/committee-drep-votes-tx.raw"

  $CARDANO_CLI conway transaction sign \
    --testnet-magic $NETWORK_MAGIC \
    --tx-body-file "${TRANSACTIONS_DIR}/committee-drep-votes-tx.raw" \
    --signing-key-file "${UTXO_DIR}/payment1.skey" \
    --signing-key-file "${DREP_DIR}/drep1.skey" \
    --signing-key-file "${DREP_DIR}/drep2.skey" \
    --signing-key-file "${DREP_DIR}/drep3.skey" \
    --out-file "${TRANSACTIONS_DIR}/committee-drep-votes-tx.signed"

  $CARDANO_CLI conway transaction submit \
    --testnet-magic $NETWORK_MAGIC \
    --tx-file "${TRANSACTIONS_DIR}/committee-drep-votes-tx.signed"

  sleep 5

### ----------––––––––
# SPO VOTES
### ----------––––––––

for i in {1..3}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cold-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/committee-spo${i}.vote"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/committee-spo1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/committee-spo2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/committee-spo3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/committee-spo-votes-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/committee-spo-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${POOL_DIR}/cold1.skey" \
  --signing-key-file "${POOL_DIR}/cold2.skey" \
  --signing-key-file "${POOL_DIR}/cold3.skey" \
  --out-file "${TRANSACTIONS_DIR}/committee-spo-votes-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/committee-spo-votes-tx.signed"

sleep 5

# Query gov state, looking for the votes

$CARDANO_CLI conway governance query gov-state --testnet-magic 42 | jq -r '.gov.curGovSnapshots.psGovActionStates."${IDIX}"'

expiresAfter=$(cardano-cli conway governance query gov-state --testnet-magic 42 | jq -r '.gov.curGovSnapshots.psGovActionStates[].expiresAfter')

echo "ONCE THE VOTING PERIOD ENDS ON EPOCH ${expiresAfter}, WE SHOULD SEE THE NEW CC MEMBER RATIFIED ON THE GOVERNANCE STATE"

tip=$(cardano-cli query tip --testnet-magic 42 | jq .)
current_epoch=$(echo $tip | jq .epoch)
slots_to_epoch_end=$(echo $tip | jq .slotsToEpochEnd)

sleep $((60 * (expiresAfter - current_epoch) + slots_to_epoch_end / 10 ))

$CARDANO_CLI conway governance query gov-state --testnet-magic $NETWORK_MAGIC | jq -r '.ratify.committee'

# Issue Authorization certificates from cold to hot keys

for i in {1..3}; do
  $CARDANO_CLI conway governance committee key-gen-hot \
    --verification-key-file "${CC_DIR}/hot${i}-cc.vkey" \
    --signing-key-file "${CC_DIR}/hot${i}-cc.skey"
done

for i in {1..3}; do
  $CARDANO_CLI conway governance committee create-hot-key-authorization-certificate \
    --cold-verification-key-file "${CC_DIR}/cold${i}-cc.vkey" \
    --hot-key-file "${CC_DIR}/hot${i}-cc.vkey" \
    --out-file "${CC_DIR}/hot-key${i}-cc-authorization.cert"
done

sleep 5

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${CC_DIR}/hot-key1-cc-authorization.cert" \
  --certificate-file "${CC_DIR}/hot-key2-cc-authorization.cert" \
  --certificate-file "${CC_DIR}/hot-key3-cc-authorization.cert" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/authorization-certs-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/authorization-certs-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${CC_DIR}/cold1-cc.skey" \
  --signing-key-file "${CC_DIR}/cold2-cc.skey" \
  --signing-key-file "${CC_DIR}/cold3-cc.skey" \
  --out-file "${TRANSACTIONS_DIR}/authorization-certs-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/authorization-certs-tx.signed"

sleep 5
