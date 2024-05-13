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

DREP_DIR=example/dreps
UTXO_DIR=example/utxo-keys
POOL_DIR=example/pools
TRANSACTIONS_DIR=example/transactions
CC_DIR=example/cc

mkdir -p "$TRANSACTIONS_DIR"

# "QUERY GOVERNANCE STATE"

echo "DOWNLOAD A PROPOSAL FILE, THIS IS WHERE WE EXPLAIN WHY THIS PROPOSAL IS RELEVANT"

wget https://tinyurl.com/3wrwb2as -O "${TRANSACTIONS_DIR}/proposal.txt"

# Query the governance action deposit amount
govActDeposit=$(cardano-cli conway query gov-state | jq -r .currentPParams.govActionDeposit)
# Calculate the hash of our proposal document (justification)
proposalHash="$(cardano-cli conway governance hash anchor-data --file-text ${TRANSACTIONS_DIR}/proposal.txt)"

# "CREATE A PROPOSAL TO HARDFORK TO PROTOCOL VERSION 10"

cardano-cli conway governance action create-hardfork \
  --testnet \
  --governance-action-deposit "$govActDeposit" \
  --deposit-return-stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
  --anchor-url "https://tinyurl.com/3wrwb2as" \
  --anchor-data-hash "$proposalHash" \
  --protocol-major-version 10 \
  --protocol-minor-version 0 \
  --out-file "${TRANSACTIONS_DIR}/hardfork10.action"

# "BUILD, SIGN AND SUBMIT THE CONSTITUTION"

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --proposal-file "${TRANSACTIONS_DIR}/hardfork10.action" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/hardfork10-tx.raw"

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/hardfork10-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --out-file "${TRANSACTIONS_DIR}/hardfork10-tx.signed"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/hardfork10-tx.signed"

sleep 10

# Get the gov action id and index

ID="$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.txId')"
IX="$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.govActionIx')"

echo "VOTE AS DREPS AND AS SPO"

### ----------––––––––
# DREP AND SPO VOTES, all yes
### ----------––––––––

# for i in {1..3}; do
#   cardano-cli conway governance vote create \
#     --yes \
#     --governance-action-tx-id "${ID}" \
#     --governance-action-index "${IX}" \
#     --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
#     --out-file "${TRANSACTIONS_DIR}/${ID}-drep${i}.vote"
# done

sleep 2

for i in {1..3}; do
  cardano-cli conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cold-verification-key-file "${POOL_DIR}/cold${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/${ID}-spo${i}.vote"
done

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/${ID}-spo1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/${ID}-spo2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/${ID}-spo3.vote" \
  --witness-override 7 \
  --out-file "${TRANSACTIONS_DIR}/hardfork10-votes-tx.raw"

  # --vote-file "${TRANSACTIONS_DIR}/${ID}-drep1.vote" \
  # --vote-file "${TRANSACTIONS_DIR}/${ID}-drep2.vote" \
  # --vote-file "${TRANSACTIONS_DIR}/${ID}-drep3.vote" \

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/hardfork10-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --signing-key-file "${DREP_DIR}/drep2.skey" \
  --signing-key-file "${DREP_DIR}/drep3.skey" \
  --signing-key-file "${POOL_DIR}/cold1.skey" \
  --signing-key-file "${POOL_DIR}/cold2.skey" \
  --signing-key-file "${POOL_DIR}/cold3.skey" \
  --out-file "${TRANSACTIONS_DIR}/hardfork10-votes-tx.signed"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/hardfork10-votes-tx.signed"

sleep 10

