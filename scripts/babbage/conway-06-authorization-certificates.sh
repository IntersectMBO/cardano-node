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

# ----------------------

$CARDANO_CLI conway query gov-state --testnet-magic $NETWORK_MAGIC | jq -r '.enactState.committee'

# Create CC hot credentials for a multisig simple script

for i in {1..3}; do
  $CARDANO_CLI conway governance committee key-gen-hot \
    --verification-key-file "${CC_MULTISIG}/hot${i}-multisig-cc.vkey" \
    --signing-key-file "${CC_MULTISIG}/hot${i}-multisig-cc.skey"
done

# Get key-hashes

for i in {1..3}; do
   $CARDANO_CLI conway governance committee key-hash \
   --verification-key-file "${CC_MULTISIG}/hot${i}-multisig-cc.vkey" > "${CC_MULTISIG}/hot${i}-multisig-vkey.hash"
done

# Array to hold the hash filenames
files=("${CC_MULTISIG}/hot1-multisig-vkey.hash" "${CC_MULTISIG}/hot2-multisig-vkey.hash" "${CC_MULTISIG}/hot3-multisig-vkey.hash")

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
echo "$json" > "${CC_MULTISIG}/hot-multisig.json"

cardano-cli conway address build \
  --payment-script-file ${CC_MULTISIG}/hot-multisig.json \
  --testnet-magic 42 \
  --out-file "${CC_MULTISIG}/hot-multisigscript.addr"

cardano-cli conway transaction build \
  --testnet-magic 42 \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --tx-out $(cat ${CC_MULTISIG}/hot-multisigscript.addr)+100000000 \
  --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
  --out-file "${CC_MULTISIG}/tx.raw"

cardano-cli conway transaction sign \
  --testnet-magic 42 \
  --tx-body-file "${CC_MULTISIG}/tx.raw" \
  --signing-key-file "${UTXO_DIR}/payment1.skey" \
  --out-file "${CC_MULTISIG}/tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 42 \
  --tx-file "${CC_MULTISIG}/tx.signed"

# Get script hash \ 

cardano-cli conway governance hash script \
  --script-file ${CC_MULTISIG}/hot-multisig.json \
  --out-file ${CC_MULTISIG}/hot-multisig.hash

# Issue Authorization certificates from cold to hot keys


cardano-cli conway governance committee create-hot-key-authorization-certificate \
--cold-script-file ${CC_MULTISIG}/multisig.json \
--hot-script-file ${CC_MULTISIG}/hot-multisig.json \
--out-file "${CC_MULTISIG}/hot-script-cc-authorization.cert"

$CARDANO_CLI conway transaction build \
  --testnet-magic $NETWORK_MAGIC \
  --tx-in "$(cardano-cli query utxo --address "$(cat "${CC_MULTISIG}/multisigscript.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
  --change-address "$(cat ${CC_MULTISIG}/multisigscript.addr)" \
  --certificate-file "${CC_MULTISIG}/hot-script-cc-authorization.cert" \
  --certificate-script-file "${CC_MULTISIG}/multisig.json" \
  --witness-override 3 \
  --out-file "${TRANSACTIONS_DIR}/hot-script-authorization-certs-tx.raw"

cardano-cli transaction witness \
  --tx-body-file "${TRANSACTIONS_DIR}/hot-script-authorization-certs-tx.raw" \
  --signing-key-file "${CC_MULTISIG}/cold1-multisig-cc.skey" \
  --testnet-magic 42 \
  --out-file  "${TRANSACTIONS_DIR}/key1witness"

cardano-cli transaction witness \
  --tx-body-file "${TRANSACTIONS_DIR}/hot-script-authorization-certs-tx.raw" \
  --signing-key-file "${CC_MULTISIG}/cold2-multisig-cc.skey" \
  --testnet-magic 42 \
  --out-file  "${TRANSACTIONS_DIR}/key2witness"

cardano-cli transaction assemble \
  --tx-body-file "${TRANSACTIONS_DIR}/hot-script-authorization-certs-tx.raw" \
  --witness-file "${TRANSACTIONS_DIR}/key1witness" \
  --witness-file "${TRANSACTIONS_DIR}/key2witness" \
  --out-file "${TRANSACTIONS_DIR}/spendMultiSig"

$CARDANO_CLI conway transaction submit \
  --testnet-magic $NETWORK_MAGIC \
  --tx-file "${TRANSACTIONS_DIR}/spendMultiSig"


# for i in {1..2}; do
#   $CARDANO_CLI conway governance committee key-gen-hot \
#     --verification-key-file "${CC_DIR}/hot${i}-cc.vkey" \
#     --signing-key-file "${CC_DIR}/hot${i}-cc.skey"
# done

# for i in {1..2}; do
#   $CARDANO_CLI conway governance committee create-hot-key-authorization-certificate \
#     --cold-verification-key-file "${CC_DIR}/cold${i}-cc.vkey" \
#     --hot-key-file "${CC_DIR}/hot${i}-cc.vkey" \
#     --out-file "${CC_DIR}/hot-key${i}-cc-authorization.cert"
# done

# sleep 5

# $CARDANO_CLI conway transaction build \
#   --testnet-magic $NETWORK_MAGIC \
#   --tx-in "$(cardano-cli query utxo --address "$(cat "${UTXO_DIR}/payment1.addr")" --testnet-magic $NETWORK_MAGIC --out-file /dev/stdout | jq -r 'keys[0]')" \
#   --change-address "$(cat ${UTXO_DIR}/payment1.addr)" \
#   --certificate-file "${CC_DIR}/hot-key1-cc-authorization.cert" \
#   --certificate-file "${CC_DIR}/hot-key2-cc-authorization.cert" \
#   --witness-override 3 \
#   --out-file "${TRANSACTIONS_DIR}/authorization-certs-tx.raw"

# $CARDANO_CLI conway transaction sign \
#   --testnet-magic $NETWORK_MAGIC \
#   --tx-body-file "${TRANSACTIONS_DIR}/authorization-certs-tx.raw" \
#   --signing-key-file "${UTXO_DIR}/payment1.skey" \
#   --signing-key-file "${CC_DIR}/cold1-cc.skey" \
#   --signing-key-file "${CC_DIR}/cold2-cc.skey" \
#   --out-file "${TRANSACTIONS_DIR}/authorization-certs-tx.signed"

# $CARDANO_CLI conway transaction submit \
#   --testnet-magic $NETWORK_MAGIC \
#   --tx-file "${TRANSACTIONS_DIR}/authorization-certs-tx.signed"

# sleep 5


