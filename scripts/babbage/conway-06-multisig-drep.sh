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
mkdir -p "$CC_MULTISIG"

# ----------------------

# Create CC credentials for a multisig simple script

for i in {1..3}; do
  cardano-cli conway governance drep key-gen \
    --verification-key-file "${DREP_DIR}/drep${i}-multisig.vkey" \
    --signing-key-file "${DREP_DIR}/drep${i}-multisig.skey"
done


# Get key-hashes

for i in {1..3}; do
   cardano-cli conway governance drep id \
   --drep-verification-key-file "${DREP_DIR}/drep${i}-multisig.vkey" \
   --output-format "hex" \
   --out-file "${DREP_DIR}/drep${i}-multisig.hash"
done

# Array to hold the hash filenames
files=("${DREP_DIR}/drep1-multisig.hash" "${DREP_DIR}/drep2-multisig.hash" "${DREP_DIR}/drep3-multisig.hash")

# The script will need 2 out of 3 signatures

json_template='{
  "type": "atLeast",
  "required": 2,
  "scripts": [
    {
      "type": "sig",
      "keyHash": "%s"
    },
    {
      "type": "sig",
      "keyHash": "%s"
    },
    {
      "type": "sig",
      "keyHash": "%s"
    }
  ]
}'

# Read hashes from files and populate JSON
hashes=()
for file in "${files[@]}"; do
    hash=$(<"$file")
    hashes+=("$hash")
done

# Populate JSON template with hashes
json=$(printf "$json_template" "${hashes[@]}")

# Save JSON to a file
echo "$json" > "${DREP_DIR}/multisig.json"

# Get script hash \ 

cardano-cli conway governance hash script \
  --script-file ${DREP_DIR}/multisig.json \
  --out-file ${DREP_DIR}/multisig.hash

# GENERATE AND SUBMIT REGISTRATION CERTIFICATE

drepDeposit=$(cardano-cli conway query gov-state | jq -r .currentPParams.dRepDeposit)

cardano-cli conway governance drep registration-certificate \
  --drep-script-hash "$(cat ${DREP_DIR}/multisig.hash)" \
  --key-reg-deposit-amt "$drepDeposit" \
  --out-file "${TRANSACTIONS_DIR}/drep-multisig-reg.cert"

cardano-cli conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat "${UTXO_DIR}/payment1.addr")" \
  --certificate-file "${TRANSACTIONS_DIR}/drep-multisig-reg.cert" \
  --certificate-script-file "${DREP_DIR}/multisig.json" \
  --witness-override 4 \
  --out-file "${TRANSACTIONS_DIR}/drep-multisig-reg-tx.raw"

cardano-cli conway transaction witness \
  --tx-body-file "${TRANSACTIONS_DIR}/drep-multisig-reg-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --testnet-magic 42 \
  --out-file  "${TRANSACTIONS_DIR}/payment1.witness"

cardano-cli conway transaction witness \
  --tx-body-file "${TRANSACTIONS_DIR}/drep-multisig-reg-tx.raw" \
  --signing-key-file "${DREP_DIR}/drep1-multisig.skey" \
  --testnet-magic 42 \
  --out-file  "${TRANSACTIONS_DIR}/drep1.witness"

cardano-cli conway transaction witness \
  --tx-body-file "${TRANSACTIONS_DIR}/drep-multisig-reg-tx.raw" \
  --signing-key-file "${DREP_DIR}/drep2-multisig.skey" \
  --testnet-magic 42 \
  --out-file  "${TRANSACTIONS_DIR}/drep2.witness"

cardano-cli conway transaction witness \
  --tx-body-file "${TRANSACTIONS_DIR}/drep-multisig-reg-tx.raw" \
  --signing-key-file "${DREP_DIR}/drep3-multisig.skey" \
  --testnet-magic 42 \
  --out-file  "${TRANSACTIONS_DIR}/drep3.witness"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/drep-multisig-reg-tx.signed"

sleep 8

# GENERATE DELEGATION CERTIFICATES FROM STAKE ADDRESSES TO THE DREPS

for i in {1..1}; do
  cardano-cli conway stake-address vote-delegation-certificate \
    --stake-verification-key-file "${UTXO_DIR}/stake${i}.vkey" \
    --drep-script-hash "$(cat "${DREP_DIR}/multisig.hash")" \
    --out-file "${TRANSACTIONS_DIR}/drep-deleg${i}.cert"
done

cardano-cli conway transaction build \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --certificate-file "${TRANSACTIONS_DIR}/drep-deleg1.cert" \
  --witness-override 2 \
  --out-file "${TRANSACTIONS_DIR}/drep-deleg-tx.raw"

cardano-cli conway transaction sign \
  --tx-body-file "${TRANSACTIONS_DIR}/drep-deleg-tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --signing-key-file "${UTXO_DIR}/stake1.skey" \
  --out-file "${TRANSACTIONS_DIR}/drep-deleg-tx.signed"

cardano-cli conway transaction submit \
  --tx-file "${TRANSACTIONS_DIR}/drep-deleg-tx.signed"

cardano-cli query tip --testnet-magic 42

sleep 5

# QUERY DREP STATE TO CONFIRM

cardano-cli conway query drep-state --all-dreps --include-stake