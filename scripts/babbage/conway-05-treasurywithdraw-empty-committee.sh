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

ROOT=example
DREP_DIR=example/dreps
UTXO_DIR=example/utxo-keys
CC_DIR=example/cc
TRANSACTIONS_DIR=example/transactions

mkdir -p "$TRANSACTIONS_DIR"


# First we will generate some new keys that will NOT be delegated to a pool so that it is easier to proof that the withdrawal worked.
# becuase this will not be receiving rewards so at the end of the script the only balance at the rewards account will be the funds
# withdrown from treasury

cardano-cli conway address key-gen \
--verification-key-file "${UTXO_DIR}/payment4.vkey" \
--signing-key-file "${UTXO_DIR}/payment4.skey" \

cardano-cli conway stake-address key-gen \
--verification-key-file "${UTXO_DIR}/stake4.vkey" \
--signing-key-file "${UTXO_DIR}/stake4.skey"

cardano-cli conway address build \
--payment-verification-key-file "${UTXO_DIR}/payment4.vkey" \
--stake-verification-key-file "${UTXO_DIR}/stake4.vkey" \
--out-file "${UTXO_DIR}/payment4.addr"

cardano-cli conway stake-address build \
--stake-verification-key-file "${UTXO_DIR}/stake4.vkey" \
--out-file "${UTXO_DIR}/stake4.addr"

# Fund the payment4.addr

cardano-cli conway transaction build \
    --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --out-file /dev/stdout | jq -r 'keys[0]')" \
    --tx-out "$(cat ${UTXO_DIR}/payment4.addr)+100000000" \
    --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
    --out-file "${TRANSACTIONS_DIR}/fund-pay4-tx.raw"

cardano-cli conway transaction sign \
    --tx-body-file "${TRANSACTIONS_DIR}/fund-pay4-tx.raw" \
    --signing-key-file "${UTXO_DIR}/payment1.skey" \
    --out-file "${TRANSACTIONS_DIR}/fund-pay4-tx.signed"

cardano-cli conway transaction submit \
    --tx-file  "${TRANSACTIONS_DIR}/fund-pay4-tx.signed"

sleep 5

# Register stake4.addr

keyDeposit=$(cardano-cli conway query protocol-parameters | jq .stakeAddressDeposit)

cardano-cli conway stake-address registration-certificate \
  --stake-verification-key-file "${UTXO_DIR}/stake4.vkey" \
  --key-reg-deposit-amt "$keyDeposit" \
  --out-file "${TRANSACTIONS_DIR}/stake4-reg.cert"

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment4.addr")" --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment4.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/stake4-reg.cert" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/reg-stake4-tx.raw"

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/reg-stake4-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment4.skey" \
  --signing-key-file "${UTXO_DIR}/stake4.skey" \
  --out-file "${TRANSACTIONS_DIR}/reg-stake4-tx.signed"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/reg-stake4-tx.signed"

sleep 5

# The rewards address is empty, show that

cardano-cli conway query stake-address-info \
  --address "$(cat "${UTXO_DIR}/stake4.addr")"

# Proceed creating a treasury withdrawal

# Download the file to use in the proposal

wget https://tinyurl.com/3wrwb2as -O "${TRANSACTIONS_DIR}/govActionJustification.txt"


TREASURY=$(cardano-cli conway query ledger-state | jq -r '.stateBefore.esAccountState.treasury')
echo "The Treasury currently has a balance of ${TREASURY}"

WITHDRAW_AMOUNT=$((TREASURY / 100 ))
echo "Our proposal will attempt to withdraw 1% of the treasury: $WITHDRAW_AMOUNT"

govActDeposit=$(cardano-cli conway query gov-state | jq .currentPParams.govActionDeposit)
proposalHash="$(cardano-cli conway governance hash anchor-data --file-text ${TRANSACTIONS_DIR}/govActionJustification.txt)"

cardano-cli conway governance action create-treasury-withdrawal \
  --testnet \
  --governance-action-deposit "$govActDeposit" \
  --deposit-return-stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
  --anchor-url "https://tinyurl.com/3wrwb2as" \
  --anchor-data-hash "$proposalHash" \
  --funds-receiving-stake-verification-key-file "${UTXO_DIR}/stake4.vkey" \
  --transfer "${WITHDRAW_AMOUNT}" \
  --out-file "${TRANSACTIONS_DIR}/treasury.action"

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --proposal-file "${TRANSACTIONS_DIR}/treasury.action" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/treasury-tx.raw"

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/treasury-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --out-file "${TRANSACTIONS_DIR}/treasury-tx.signed"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/treasury-tx.signed"

sleep 5

cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals'

ID="$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.txId')"
IX="$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.govActionIx')"


### ---------
# DREP VOTES
### ---------

for i in {1..3}; do
  cardano-cli conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/treasury-drep${i}.vote"
done

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/treasury-drep1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/treasury-drep2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/treasury-drep3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/treasury-dreps-vote-tx.raw"

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/treasury-dreps-vote-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --signing-key-file "${DREP_DIR}/drep2.skey" \
  --signing-key-file "${DREP_DIR}/drep3.skey" \
  --out-file "${TRANSACTIONS_DIR}/treasury-dreps-vote-tx.signed"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/treasury-dreps-vote-tx.signed"

sleep 5

cardano-cli conway query gov-state | jq -r '.proposals'

expiresAfter=$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].expiresAfter')

echo "ONCE THE VOTING PERIOD ENDS ON EPOCH ${expiresAfter}, WE SHOULD SEE THE FUNDS FROM THE TREASURY IN STAKE4.ADDR"

tip=$(cardano-cli query tip | jq .)
current_epoch=$(echo $tip | jq .epoch)
slots_to_epoch_end=$(echo $tip | jq .slotsToEpochEnd)

sleep $((60 * (expiresAfter - current_epoch) + slots_to_epoch_end / 10))

cardano-cli conway query stake-address-info --address "$(cat "${UTXO_DIR}/stake4.addr")"

cardano-cli conway query stake-address-info --address "$(cat "${UTXO_DIR}/stake1.addr")"
