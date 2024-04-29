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
DREP_DIR=example/dreps
UTXO_DIR=example/utxo-keys
TRANSACTIONS_DIR=example/transactions
CC_DIR=example/cc

mkdir -p "$TRANSACTIONS_DIR"

# "QUERY GOVERNANCE STATE"

echo "DOWNLOAD A PROPOSAL FILE, THIS IS WHERE WE EXPLAIN WHY THIS PROPOSAL IS RELEVANT"

wget https://tinyurl.com/3wrwb2as -O "${TRANSACTIONS_DIR}/proposal.txt"

# "DOWNLOAD OUR SAMPLE CONSTITUTION FILE"

wget https://tinyurl.com/mr3ferf9 -O "${TRANSACTIONS_DIR}/constitution.txt"

echo "CALCULATE THE HASH OURSELVES"

hash=$(b2sum -l 256 ${TRANSACTIONS_DIR}/constitution.txt | cut -d' ' -f1)
echo "$hash"

# "QUERY THE CURRENT CONTSTITUTION HASH"

$CARDANO_CLI conway query constitution \
  --testnet-magic $NETWORK_MAGIC

# "CREATE A PROPOSAL TO UPDATE THE CONSTITUTION"
govActDeposit=$($CARDANO_CLI conway query gov-state --testnet-magic $NETWORK_MAGIC | jq -r .currentPParams.govActionDeposit)
constitutionHash="$($CARDANO_CLI conway governance hash anchor-data --file-text ${TRANSACTIONS_DIR}/constitution.txt)"
proposalHash="$($CARDANO_CLI conway governance hash anchor-data --file-text ${TRANSACTIONS_DIR}/proposal.txt)"


$CARDANO_CLI conway governance action create-constitution \
  --testnet \
  --governance-action-deposit "$govActDeposit" \
  --deposit-return-stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
  --anchor-url "https://tinyurl.com/3wrwb2as" \
  --anchor-data-hash "$proposalHash" \
  --constitution-url "https://tinyurl.com/2pahcy6z"  \
  --constitution-hash "$constitutionHash" \
  --out-file "${TRANSACTIONS_DIR}/constitution.action"

# "BUILD, SIGN AND SUBMIT THE CONSTITUTION"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --proposal-file "${TRANSACTIONS_DIR}/constitution.action" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/constitution-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/constitution-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --out-file "${TRANSACTIONS_DIR}/constitution-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/constitution-tx.signed"

sleep 5

sleep 5

ID="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.txId')"
IX="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.govActionIx')"

### ----------––––––––
# DREP VOTES
### ----------––––––––

for i in {1..1}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/${ID}-drep${i}.vote"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/${ID}-drep1.vote" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --out-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/constitution-drep-votes-tx.signed"

sleep 5