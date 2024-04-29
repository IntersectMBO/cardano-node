#!/usr/bin/env bash

# This will need cli 8.13.0.0

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
UTXO_DIR=example/utxo-keys
TRANSACTIONS_DIR=example/transactions
CC_DIR=example/cc
DREP_DIR=example/dreps

mkdir -p "$TRANSACTIONS_DIR"
mkdir -p "$CC_DIR"

$CARDANO_CLI conway query protocol-parameters --testnet-magic 42 | jq .maxBlockHeaderSize

sleep 2

wget https://tinyurl.com/3wrwb2as -O "${TRANSACTIONS_DIR}/govActionJustification.txt"

govActDeposit=$($CARDANO_CLI conway query gov-state --testnet-magic $NETWORK_MAGIC | jq .enactState.curPParams.govActionDeposit)
proposalHash="$($CARDANO_CLI conway governance hash anchor-data --file-text ${TRANSACTIONS_DIR}/govActionJustification.txt)"

$CARDANO_CLI conway governance action create-protocol-parameters-update \
--testnet \
--governance-action-deposit "$govActDeposit" \
--deposit-return-stake-verification-key-file "${UTXO_DIR}/stake1.vkey" \
--anchor-url https://tinyurl.com/3wrwb2as  \
--anchor-data-hash "$proposalHash" \
--key-reg-deposit-amt 3000000 \
--max-block-header-size 1200 \
--prev-governance-action-tx-id "$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r .enactState.prevGovActionIds.EnactedPParamUpdate.txId)" \
--prev-governance-action-index "$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r .enactState.prevGovActionIds.EnactedPParamUpdate.govActionIx)" \
--out-file "${TRANSACTIONS_DIR}/pparams.action" \


$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --proposal-file "${TRANSACTIONS_DIR}/pparams.action" \
  --out-file "${TRANSACTIONS_DIR}/pparams-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/pparams-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --out-file "${TRANSACTIONS_DIR}/pparams-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/pparams-tx.signed"

sleep 5

ID="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.txId')"
IX="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].actionId.govActionIx')"

### ---------
# DREP VOTES
### ---------

for i in {1..3}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --drep-verification-key-file "${DREP_DIR}/drep${i}.vkey" \
    --out-file "${TRANSACTIONS_DIR}/pparams-drep${i}.vote"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/pparams-drep1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/pparams-drep2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/pparams-drep3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/pparams-dreps-vote-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/pparams-dreps-vote-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --signing-key-file "${DREP_DIR}/drep2.skey" \
  --signing-key-file "${DREP_DIR}/drep3.skey" \
  --out-file "${TRANSACTIONS_DIR}/pparams-dreps-vote-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/pparams-dreps-vote-tx.signed"

sleep 5

# Check the state for Drep votes

$CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals'

# ### ----------––––––––
# # CC VOTES
# ### ----------––––––––

for i in {1..3}; do
  $CARDANO_CLI conway governance vote create \
    --yes \
    --governance-action-tx-id "${ID}" \
    --governance-action-index "${IX}" \
    --cc-hot-verification-key-file "${CC_DIR}/hot${i}-cc.vkey" \
    --out-file "${TRANSACTIONS_DIR}/pparams-cc${i}.vote"
done

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --vote-file "${TRANSACTIONS_DIR}/pparams-cc1.vote" \
  --vote-file "${TRANSACTIONS_DIR}/pparams-cc2.vote" \
  --vote-file "${TRANSACTIONS_DIR}/pparams-cc3.vote" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/pparams-cc-votes-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \  
  --tx-body-file "${TRANSACTIONS_DIR}/pparams-cc-votes-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${CC_DIR}/hot1-cc.skey" \
  --signing-key-file "${CC_DIR}/hot2-cc.skey" \
  --signing-key-file "${CC_DIR}/hot3-cc.skey" \
  --out-file "${TRANSACTIONS_DIR}/pparams-cc-votes-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/pparams-cc-votes-tx.signed"

sleep 5

# Check the state for DREP and SPO votes

$CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals'

# expiresAfter=$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r '.proposals[0].expiresAfter')

# echo "ONCE THE VOTING PERIOD ENDS ON EPOCH ${expiresAfter}, WE SHOULD SEE THE NEW PROTOCOL PARAMETERS RATIFIED"

# tip=$(cardano-cli query tip --testnet-magic 42 | jq .)
# current_epoch=$(echo $tip | jq .epoch)
# slots_to_epoch_end=$(echo $tip | jq .slotsToEpochEnd)

# sleep $((60 * (expiresAfter - current_epoch) + (slots_to_epoch_end / 10)))

$CARDANO_CLI conway query gov-state --testnet-magic $NETWORK_MAGIC | jq -r '.enactState.curPParams'
