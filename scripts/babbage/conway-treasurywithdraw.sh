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
ROOT=example
DREP_DIR=example/dreps
UTXO_DIR=example/utxo-keys
CC_DIR=example/cc
TRANSACTIONS_DIR=example/transactions

mkdir -p "$TRANSACTIONS_DIR"


# First we will generate some new keys that will NOT be delegated to a pool so that it is easier to proof that the withdrawal worked.
# becuase this will not be receiving rewards so at the end of the script the only balance at the rewards account will be the funds 
# withdrown from treasury 

$CARDANO_CLI conway address key-gen \
--verification-key-file "${UTXO_DIR}/payment4.vkey" \
--signing-key-file "${UTXO_DIR}/payment4.skey" \

$CARDANO_CLI conway stake-address key-gen \
--verification-key-file "${UTXO_DIR}/stake4.vkey" \
--signing-key-file "${UTXO_DIR}/stake4.skey"

$CARDANO_CLI conway address build \
--payment-verification-key-file "${UTXO_DIR}/payment4.vkey" \
--stake-verification-key-file "${UTXO_DIR}/stake4.vkey" \
--testnet-magic $NETWORK_MAGIC \
--out-file "${UTXO_DIR}/payment4.addr"

$CARDANO_CLI conway stake-address build \
--stake-verification-key-file "${UTXO_DIR}/stake4.vkey" \
--testnet-magic $NETWORK_MAGIC \
--out-file "${UTXO_DIR}/stake4.addr"

# Fund the payment4.addr

$CARDANO_CLI conway transaction build \
    --testnet-magic $NETWORK_MAGIC \
    --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
    --tx-out "$(cat ${UTXO_DIR}/payment4.addr)+10000000" \
    --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
    --out-file "${TRANSACTIONS_DIR}/fund-pay4-tx.raw"

$CARDANO_CLI conway transaction sign --testnet-magic ${NETWORK_MAGIC} \
    --tx-body-file "${TRANSACTIONS_DIR}/fund-pay4-tx.raw" \
    --signing-key-file "${UTXO_DIR}/payment1.skey" \
    --out-file "${TRANSACTIONS_DIR}/fund-pay4-tx.signed"

$CARDANO_CLI conway transaction submit \
    --testnet-magic $NETWORK_MAGIC \
    --tx-file  "${TRANSACTIONS_DIR}/fund-pay4-tx.signed"

sleep 5

# Register stake4.addr

 $CARDANO_CLI conway stake-address registration-certificate \
    --stake-verification-key-file "${UTXO_DIR}/stake4.vkey" \
    --key-reg-deposit-amt 0 \
    --out-file "${TRANSACTIONS_DIR}/stake4-reg.cert"

  $CARDANO_CLI conway transaction build \
    --testnet-magic $NETWORK_MAGIC \
    --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment4.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
    --change-address "$(cat ${UTXO_DIR}/payment4.addr)" \
    --certificate-file "${TRANSACTIONS_DIR}/stake4-reg.cert" \
    --witness-override 2 \
    --out-file "${TRANSACTIONS_DIR}/reg-stake4-tx.raw"

  $CARDANO_CLI conway transaction sign --testnet-magic $NETWORK_MAGIC \
    --tx-body-file "${TRANSACTIONS_DIR}/reg-stake4-tx.raw" \
    --signing-key-file "${UTXO_DIR}/payment4.skey" \
    --signing-key-file "${UTXO_DIR}/stake4.skey" \
    --out-file "${TRANSACTIONS_DIR}/reg-stake4-tx.signed"

  $CARDANO_CLI conway transaction submit \
    --testnet-magic $NETWORK_MAGIC \
    --tx-file "${TRANSACTIONS_DIR}/reg-stake4-tx.signed"

sleep 5

# The rewards address is empty, show that

$CARDANO_CLI conway query stake-address-info \
--testnet-magic $NETWORK_MAGIC \
--address "$(cat "${UTXO_DIR}/stake4.addr")"

# Proceed creating a treasury withdrawal

# Download the file to use in the proposal

wget https://tinyurl.com/3wrwb2as -O "${TRANSACTIONS_DIR}/govActionJustification.txt"


TREASURY=$(cardano-cli conway query ledger-state --testnet-magic 42 | jq -r '.stateBefore.esAccountState.treasury')
echo "The Treasury currently has a balance of ${TREASURY}"

WITHDRAW_AMOUNT=$(($TREASURY / 100 ))
echo "Our proposal will attempt to withdraw 1% of the treasury: $WITHDRAW_AMOUNT"


$CARDANO_CLI conway governance action create-treasury-withdrawal \
  --testnet \
  --governance-action-deposit 0 \
  --stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
  --proposal-url https://tinyurl.com/3wrwb2as \
  --proposal-file  "${TRANSACTIONS_DIR}/govActionJustification.txt" \
  --stake-verification-key-file "${UTXO_DIR}/stake4.vkey" \
  --transfer "${WITHDRAW_AMOUNT}" \
  --out-file "${TRANSACTIONS_DIR}/treasury.action"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address $(cat "${UTXO_DIR}/payment1.addr") --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --proposal-file "${TRANSACTIONS_DIR}/treasury.action" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/treasury-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/treasury-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --out-file "${TRANSACTIONS_DIR}/treasury-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/treasury-tx.signed"

sleep 5

$CARDANO_CLI conway governance query gov-state --testnet-magic 42 | jq -r '.gov.curGovSnapshots.psGovActionStates'

IDIX="$($CARDANO_CLI conway governance query gov-state --testnet-magic 42 | jq -r '.gov.curGovSnapshots.psGovActionStates | keys[0]')" # Assumes this is the only governance action in transit
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
    --out-file "${UTXO_DIR}/${ID}-drep${i}.vote"

    cat "${UTXO_DIR}/${ID}-drep${i}.vote"

  $CARDANO_CLI conway transaction build \
    --testnet-magic $NETWORK_MAGIC \
    --tx-in "$(cardano-cli query utxo --address $(cat "${UTXO_DIR}/payment1.addr") --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
    --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
    --vote-file "${UTXO_DIR}/${ID}-drep${i}.vote" \
    --witness-override 2 \
    --out-file "${UTXO_DIR}/${ID}-drep${i}-tx.raw"

  $CARDANO_CLI conway transaction sign \
    --testnet-magic $NETWORK_MAGIC \
    --tx-body-file "${UTXO_DIR}/${ID}-drep${i}-tx.raw" \
    --signing-key-file "${UTXO_DIR}/payment1.skey" \
    --signing-key-file "${DREP_DIR}/drep${i}.skey" \
    --out-file "${UTXO_DIR}/${ID}-drep${i}-tx.signed"

  $CARDANO_CLI conway transaction submit \
    --testnet-magic $NETWORK_MAGIC \
    --tx-file "${UTXO_DIR}/${ID}-drep${i}-tx.signed"

sleep 5

done

# Check the state for Drep votes

$CARDANO_CLI conway governance query gov-state --testnet-magic 42 | jq -r '.gov.curGovSnapshots.psGovActionStates'

### ----------––––––––
# CC VOTES
### ----------––––––––

for i in {1..3}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cc-hot-verification-key-file "${CC_DIR}/hot${i}-cc.vkey" \
    --out-file "${TRANSACTIONS_DIR}/cc${i}.vote"

  $CARDANO_CLI conway transaction build \
    --testnet-magic $NETWORK_MAGIC \
    --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
    --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
    --vote-file "${TRANSACTIONS_DIR}/cc${i}.vote" \
    --witness-override 2 \
    --out-file "${TRANSACTIONS_DIR}/cc${i}-vote-tx.raw"

  $CARDANO_CLI conway transaction sign \
    --testnet-magic $NETWORK_MAGIC \
    --tx-body-file "${TRANSACTIONS_DIR}/cc${i}-vote-tx.raw" \
    --signing-key-file "${UTXO_DIR}/payment1.skey" \
    --signing-key-file "${CC_DIR}/hot${i}-cc.skey" \
    --out-file "${TRANSACTIONS_DIR}/cc${i}-vote-tx.signed"

  $CARDANO_CLI conway transaction submit \
    --testnet-magic $NETWORK_MAGIC \
    --tx-file "${TRANSACTIONS_DIR}/cc${i}-vote-tx.signed"

  sleep 5

done

# Check the state for DREP and SPO votes
$CARDANO_CLI conway governance query gov-state --testnet-magic 42 | jq -r '.gov.curGovSnapshots.psGovActionStates'

expiresAfter=$($CARDANO_CLI conway governance query gov-state --testnet-magic 42 | jq -r '.gov.curGovSnapshots.psGovActionStates[] | .expiresAfter')

check_epoch() {
  while true; do
    currentEpoch=$($CARDANO_CLI conway query tip --testnet-magic $NETWORK_MAGIC | jq -r .epoch)

    if [ "$currentEpoch" -gt "$expiresAfter" ]; then
      $CARDANO_CLI conway query stake-address-info --testnet-magic "$NETWORK_MAGIC" --address "$(cat "${UTXO_DIR}/stake4.addr")"
      break
    else
      sleep 30
    fi
  done
}

# Call the function to check the epoch

check_epoch