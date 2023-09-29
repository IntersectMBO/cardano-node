#!/usr/bin/env bash

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
ROOT=example
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

EPOCH=$($CARDANO_CLI conway query tip --testnet-magic $NETWORK_MAGIC | jq -r .epoch)


# SKIP THIS BIT FOR NOW UNTIL CREATE-NEW-COMMITTEE TAKES CC CREDENTIALS

$CARDANO_CLI conway governance committee key-gen-cold \
--cold-verification-key-file "${CC_DIR}/cold1-cc.vkey" \
--cold-signing-key-file "${CC_DIR}/cold1-cc.skey"

$CARDANO_CLI conway governance committee key-gen-hot \
--verification-key-file "${CC_DIR}/hot1-cc.vkey" \
--signing-key-file "${CC_DIR}/hot1-cc.skey"


# Waiting for signing with CC to be fixed
: '
$CARDANO_CLI conway governance committee create-hot-key-authorization-certificate \
--cold-verification-key-file "${CC_DIR}/cold1-cc.vkey" \
--hot-key-file "${CC_DIR}/hot1-cc.vkey" \
--out-file "${CC_DIR}/hot-key1-cc-authorization.cert"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${CC_DIR}/hot-key1-cc-authorization.cert" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/authorization-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/authorization-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${CC_DIR}/cold1-cc.skey" \
  --out-file "${TRANSACTIONS_DIR}/authorization-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/authorization-tx.signed"
'

# CREATE THE PROPOSAL

$CARDANO_CLI conway governance action create-new-committee \
--testnet \
--governance-action-deposit 0 \
--stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
--proposal-url https://shorturl.at/asIJ6 \
--proposal-file "${TRANSACTIONS_DIR}/cc_proposal.txt" \
--add-cc-cold-verification-key-file "${CC_DIR}/cold1-cc.vkey" \
--epoch $(($EPOCH + 100)) \
--quorum 51/100 \
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

IDIX="$($CARDANO_CLI conway governance query gov-state --testnet-magic 42 | jq -r '.gov.curGovActionsState | keys_unsorted[0]')"
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
    --out-file "${TRANSACTIONS_DIR}/${ID}-drep${i}.vote"

  cat "${TRANSACTIONS_DIR}/${ID}-drep${i}.vote"

  $CARDANO_CLI conway transaction build \
    --testnet-magic $NETWORK_MAGIC \
    --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
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

### ----------––––––––
# SPO VOTES
### ----------––––––––

for i in {1..3}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cold-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/${ID}-spo${i}.vote"

  cat "${TRANSACTIONS_DIR}/${ID}-spo${i}.vote"

  $CARDANO_CLI conway transaction build \
    --testnet-magic $NETWORK_MAGIC \
    --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
    --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
    --vote-file "${TRANSACTIONS_DIR}/${ID}-spo${i}.vote" \
    --witness-override 2 \
    --out-file "${TRANSACTIONS_DIR}/${ID}-spo${i}-tx.raw"

  $CARDANO_CLI conway transaction sign \
    --testnet-magic $NETWORK_MAGIC \
    --tx-body-file "${TRANSACTIONS_DIR}/${ID}-spo${i}-tx.raw" \
    --signing-key-file "${UTXO_DIR}/payment1.skey" \
    --signing-key-file "${POOL_DIR}/cold${i}.skey" \
    --out-file "${TRANSACTIONS_DIR}/${ID}-spo${i}-tx.signed"

  $CARDANO_CLI conway transaction submit \
    --testnet-magic $NETWORK_MAGIC \
    --tx-file "${TRANSACTIONS_DIR}/${ID}-spo${i}-tx.signed"

  sleep 3

done

expiresAfter=$(cardano-cli conway governance query gov-state --testnet-magic 42 | jq -r '.gov.curGovActionsState[].expiresAfter')

echo "ONCE THE VOTING PERIOD ENDS ON EPOCH ${expiresAfter}, WE SHOULD SEE THE NEW CC MEMBER RATIFIED ON THE GOVERNANCE STATE"

check_epoch() {
  while true; do
    currentEpoch=$($CARDANO_CLI conway query tip --testnet-magic $NETWORK_MAGIC | jq .epoch)

    if [ "${currentEpoch}" -gt "${expiresAfter}" ]; then
      $CARDANO_CLI conway governance query gov-state --testnet-magic 42 | jq -r '.ratify.committee'
      break
    else
      sleep 30  # Sleep when the epoch hasn't changed
    fi
  done
}

# Call the function to check the epoch
check_epoch
