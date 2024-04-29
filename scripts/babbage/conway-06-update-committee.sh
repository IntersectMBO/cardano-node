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


# Create CC credentials

for i in {1..3}; do
  $CARDANO_CLI conway governance committee key-gen-cold \
    --cold-verification-key-file "${CC_DIR}/cold${i}-cc.vkey" \
    --cold-signing-key-file "${CC_DIR}/cold${i}-cc.skey"
done


# Get key-hashes

for i in {1..3}; do
   $CARDANO_CLI conway governance committee key-hash \
   --verification-key-file "${CC_DIR}/cold${i}-cc.vkey" > "${CC_DIR}/cold${i}-vkey.hash"
done

# ----------------------

# DOWNLOAD THE DUMMY PROPOSAL FILE
wget https://shorturl.at/asIJ6  -O "${TRANSACTIONS_DIR}/cc_proposal.txt"

# QUERY THE CURRENT EPOCH, WE WILL USE IT TO SET THE EXPIRATION DATE FOR OUR CC MEMEBER

epoch=$($CARDANO_CLI conway query tip --testnet-magic $NETWORK_MAGIC | jq -r .epoch)

# CREATE THE PROPOSAL

govActDeposit=$($CARDANO_CLI conway query gov-state --testnet-magic $NETWORK_MAGIC | jq -r .currentPParams.govActionDeposit)
proposalHash="$($CARDANO_CLI conway governance hash anchor-data --file-text ${TRANSACTIONS_DIR}/cc_proposal.txt)"

prevGovId=$(cardano-cli conway query gov-state | jq -r .nextRatifyState.nextEnactState.prevGovActionIds.Committee)

if [ "$prevGovId" = "null" ]; then
    $CARDANO_CLI conway governance action update-committee \
      --testnet \
      --governance-action-deposit "$govActDeposit" \
      --deposit-return-stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
      --anchor-url https://shorturl.at/asIJ6 \
      --anchor-data-hash "$proposalHash" \
      --add-cc-cold-verification-key-hash "$(cat "${CC_DIR}/cold1-vkey.hash")" \
      --epoch $((epoch + 100)) \
      --add-cc-cold-verification-key-hash "$(cat "${CC_DIR}/cold2-vkey.hash")" \
      --epoch $((epoch + 150)) \
      --add-cc-cold-verification-key-hash "$(cat "${CC_DIR}/cold3-vkey.hash")" \
      --epoch $((epoch + 150)) \
      --threshold 2/3 \
      --out-file "${TRANSACTIONS_DIR}/new-committee.action"
else
    $CARDANO_CLI conway governance action update-committee \
      --testnet \
      --governance-action-deposit "$govActDeposit" \
      --deposit-return-stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
      --anchor-url https://shorturl.at/asIJ6 \
      --anchor-data-hash "$proposalHash" \
      --add-cc-cold-verification-key-hash "$(cat "${CC_DIR}/cold1-vkey.hash")" \
      --epoch $((epoch + 100)) \
      --add-cc-cold-verification-key-hash "$(cat "${CC_DIR}/cold2-vkey.hash")" \
      --epoch $((epoch + 150)) \
      --add-cc-cold-verification-key-hash "$(cat "${CC_DIR}/cold3-vkey.hash")" \
      --epoch $((epoch + 150)) \
      --threshold 2/3 \
      --prev-governance-action-tx-id "$(echo "$prevGovId" | jq -r .txId)" \
      --prev-governance-action-index "$(echo "$prevGovId" | jq -r .govActionIx)" \
      --out-file "${TRANSACTIONS_DIR}/new-committee.action"
fi

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

sleep 3

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

cardano-cli conway query committee-state --out-file "${TRANSACTIONS_DIR}/committee-state-before-voting.json"
cat "${TRANSACTIONS_DIR}/committee-state-before-voting.json"

# LETS FIND THE ACTION ID

ID="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.txId')"
echo $ID
IX="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.govActionIx')"  
echo $IX

# LETS VOTE AS DREPS AND AS SPOS

### ----------––––––––
# DREP VOTES
### ----------––––––––

for i in {1..2}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/committee-drep${i}.vote"
done

for i in {3..3}; do
  $CARDANO_CLI conway governance vote create \
    --no \
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

  $CARDANO_CLI query tip --testnet-magic 42

  sleep 5

### ----------––––––––
# SPO VOTES
### ----------––––––––

for i in {1..2}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cold-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/committee-spo${i}.vote"
done

for i in {3..3}; do
  $CARDANO_CLI conway governance vote create \
    --no \
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

sleep 50

# Query gov state, looking for the votes1

$CARDANO_CLI conway query gov-state 
