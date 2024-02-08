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
    --tx-out "$(cat ${UTXO_DIR}/payment4.addr)+100000000" \
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

keyDeposit=$($CARDANO_CLI conway query protocol-parameters --testnet-magic $NETWORK_MAGIC | jq .keyDeposit)

$CARDANO_CLI conway stake-address registration-certificate \
  --stake-verification-key-file "${UTXO_DIR}/stake4.vkey" \
  --key-reg-deposit-amt "$keyDeposit" \
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

WITHDRAW_AMOUNT=$((TREASURY / 100 ))
echo "Our proposal will attempt to withdraw 1% of the treasury: $WITHDRAW_AMOUNT"

govActDeposit=$($CARDANO_CLI conway query gov-state --testnet-magic $NETWORK_MAGIC | jq .enactState.curPParams.govActionDeposit)
proposalHash="$($CARDANO_CLI conway governance hash --file-text ${TRANSACTIONS_DIR}/govActionJustification.txt)"

$CARDANO_CLI conway governance action create-treasury-withdrawal \
  --testnet \
  --governance-action-deposit "$govActDeposit" \
  --deposit-return-stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
  --anchor-url "https://tinyurl.com/3wrwb2as" \
  --anchor-data-hash "$proposalHash" \
  --funds-receiving-stake-verification-key-file "${UTXO_DIR}/stake4.vkey" \
  --transfer "${WITHDRAW_AMOUNT}" \
  --out-file "${TRANSACTIONS_DIR}/treasury.action"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --proposal-file "${TRANSACTIONS_DIR}/treasury.action" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/treasury-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/treasury-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --out-file "${TRANSACTIONS_DIR}/treasury-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/treasury-tx.signed"

sleep 5

$CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals'

ID="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals.[].actionId.txId')"
IX="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals.[].actionId.govActionIx')"

### ---------
# DREP VOTES
### ---------

for i in {1..3}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/treasury-drep${i}.vote"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/treasury-drep1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/treasury-drep2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/treasury-drep3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/treasury-dreps-vote-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/treasury-dreps-vote-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --signing-key-file "${DREP_DIR}/drep2.skey" \
  --signing-key-file "${DREP_DIR}/drep3.skey" \
  --out-file "${TRANSACTIONS_DIR}/treasury-dreps-vote-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/treasury-dreps-vote-tx.signed"

sleep 5

# Check the state for Drep votes

$CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals'

### ----------––––––––
# CC VOTES
### ----------––––––––

for i in {1..3}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cc-hot-verification-key-file "${CC_DIR}/hot${i}-cc.vkey" \
    --out-file "${TRANSACTIONS_DIR}/treasury-cc${i}.vote"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/treasury-cc1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/treasury-cc2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/treasury-cc3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/treasury-cc-votes-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/treasury-cc-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${CC_DIR}/hot1-cc.skey" \
  --signing-key-file "${CC_DIR}/hot2-cc.skey" \
  --signing-key-file "${CC_DIR}/hot3-cc.skey" \
  --out-file "${TRANSACTIONS_DIR}/treasury-cc-votes-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/treasury-cc-votes-tx.signed"

sleep 5

# Check the state for DREP and SPO votes

$CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals'

expiresAfter=$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals.[].expiresAfter')

echo "ONCE THE VOTING PERIOD ENDS ON EPOCH ${expiresAfter}, WE SHOULD SEE THE FUNDS FROM THE TREASURY IN STAKE4.ADDR"

tip=$(cardano-cli query tip --testnet-magic 42 | jq .)
current_epoch=$(echo $tip | jq .epoch)
slots_to_epoch_end=$(echo $tip | jq .slotsToEpochEnd)

sleep $((60 * (expiresAfter - current_epoch) + slots_to_epoch_end / 10))

$CARDANO_CLI conway query stake-address-info --testnet-magic "$NETWORK_MAGIC" --address "$(cat "${UTXO_DIR}/stake4.addr")"

