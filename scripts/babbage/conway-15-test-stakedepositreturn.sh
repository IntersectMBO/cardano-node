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
--verification-key-file "${UTXO_DIR}/payment5.vkey" \
--signing-key-file "${UTXO_DIR}/payment5.skey" \

$CARDANO_CLI conway stake-address key-gen \
--verification-key-file "${UTXO_DIR}/stake5.vkey" \
--signing-key-file "${UTXO_DIR}/stake5.skey"

$CARDANO_CLI conway address build \
--payment-verification-key-file "${UTXO_DIR}/payment5.vkey" \
--stake-verification-key-file "${UTXO_DIR}/stake5.vkey" \
--testnet-magic $NETWORK_MAGIC \
--out-file "${UTXO_DIR}/payment5.addr"

$CARDANO_CLI conway stake-address build \
--stake-verification-key-file "${UTXO_DIR}/stake5.vkey" \
--testnet-magic $NETWORK_MAGIC \
--out-file "${UTXO_DIR}/stake5.addr"

# Fund the payment5.addr

$CARDANO_CLI conway transaction build \
    --testnet-magic $NETWORK_MAGIC \
    --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
    --tx-out "$(cat ${UTXO_DIR}/payment5.addr)+10000000" \
    --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
    --out-file "${TRANSACTIONS_DIR}/fund-pay5-tx.raw"

$CARDANO_CLI conway transaction sign --testnet-magic ${NETWORK_MAGIC} \
    --tx-body-file "${TRANSACTIONS_DIR}/fund-pay5-tx.raw" \
    --signing-key-file "${UTXO_DIR}/payment1.skey" \
    --out-file "${TRANSACTIONS_DIR}/fund-pay5-tx.signed"

$CARDANO_CLI conway transaction submit \
    --testnet-magic $NETWORK_MAGIC \
    --tx-file  "${TRANSACTIONS_DIR}/fund-pay5-tx.signed"

sleep 5

# Register stake5.addr

keyDeposit="$($CARDANO_CLI conway query protocol-parameters --testnet-magic 42 | jq .keyDeposit)"

$CARDANO_CLI conway stake-address registration-certificate \
  --stake-verification-key-file "${UTXO_DIR}/stake5.vkey" \
    --key-reg-deposit-amt $keyDeposit \
    --out-file "${TRANSACTIONS_DIR}/stake5-reg.cert"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment5.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment5.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/stake5-reg.cert" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/reg-stake5-tx.raw"

$CARDANO_CLI conway transaction sign --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/reg-stake5-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment5.skey" \
  --signing-key-file "${UTXO_DIR}/stake5.skey" \
  --out-file "${TRANSACTIONS_DIR}/reg-stake5-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/reg-stake5-tx.signed"

sleep 5


$CARDANO_CLI conway query stake-address-info \
--testnet-magic $NETWORK_MAGIC \
--address "$(cat "${UTXO_DIR}/stake5.addr")"


wget https://tinyurl.com/3wrwb2as -O "${TRANSACTIONS_DIR}/govActionJustification.txt"

govActDeposit=$($CARDANO_CLI conway query gov-state --testnet-magic $NETWORK_MAGIC | jq .enactState.curPParams.govActionDeposit)
proposalHash="$($CARDANO_CLI conway governance hash --file-text ${TRANSACTIONS_DIR}/govActionJustification.txt)"

$CARDANO_CLI conway governance action create-protocol-parameters-update \
--testnet \
--governance-action-deposit "$govActDeposit" \
--stake-verification-key-file "${UTXO_DIR}/stake5.vkey" \
--anchor-url https://tinyurl.com/3wrwb2as  \
--anchor-data-hash "$proposalHash" \
--key-reg-deposit-amt 5000000 \
--governance-action-tx-id "$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r .enactState.prevGovActionIds.pgaPParamUpdate.txId)" \
--governance-action-index "$(cardano-cli conway query gov-state --testnet-magic 42 | jq -r .enactState.prevGovActionIds.pgaPParamUpdate.govActionIx)" \
--out-file "${TRANSACTIONS_DIR}/pparams.action"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --proposal-file "${TRANSACTIONS_DIR}/pparams.action" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/pparams-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/pparams-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake5.skey" \
  --out-file "${TRANSACTIONS_DIR}/pparams-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/pparams-tx.signed"


sleep 5

$CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals'

ID="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals.[].actionId.txId')"
IX="$($CARDANO_CLI conway query gov-state --testnet-magic 42 | jq -r '.proposals.[].actionId.govActionIx')"

### ---------
# DREP VOTES
### ---------

for i in {1..3}; do
  $CARDANO_CLI conway governance vote create \
    --abstain \
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
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/pparams-dreps-vote-tx.raw"

$CARDANO_CLI conway transaction sign \
  --testnet-magic $NETWORK_MAGIC \
  --tx-body-file "${TRANSACTIONS_DIR}/pparams-dreps-vote-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${DREP_DIR}/drep1.skey" \
  --out-file "${TRANSACTIONS_DIR}/pparams-dreps-vote-tx.signed"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/pparams-dreps-vote-tx.signed"

sleep 5

  $CARDANO_CLI conway query stake-address-info \
--testnet-magic $NETWORK_MAGIC \
--address "$(cat "${UTXO_DIR}/stake5.addr")"