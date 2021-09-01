#!/usr/bin/env bash

set -eo pipefail

export WORK="${WORK:-example/work}"
export BASE="${BASE:-.}"
export CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export UTXO_VKEY="${UTXO_VKEY:-example/shelley/utxo-keys/utxo1.vkey}"
export UTXO_SKEY="${UTXO_SKEY:-example/shelley/utxo-keys/utxo1.skey}"
export RESULT_FILE="${RESULT_FILE:-$WORK/result.out}"

echo "Socket path: $CARDANO_NODE_SOCKET_PATH"
echo "Socket path: $(pwd)"

ls -al "$CARDANO_NODE_SOCKET_PATH"

plutusscriptinuse="$BASE/scripts/plutus/scripts/always-fails.plutus"
# This datum hash is the hash of the untyped 42
scriptdatumhash="9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"
#ExUnits {exUnitsMem = 11300, exUnitsSteps = 45070000}))
datumfilepath="$BASE/scripts/plutus/data/42.datum"
redeemerfilepath="$BASE/scripts/plutus/data/42.redeemer"
echo "Always succeeds Plutus script in use. Any datum and redeemer combination will succeed."
echo "Script at: $plutusscriptinuse"

# Step 1: Create a tx ouput with a datum hash at the script address. In order for a tx ouput to be locked
# by a plutus script, it must have a datahash. We also need collateral tx inputs so we split the utxo
# in order to accomodate this.

plutusscriptaddr=$($CARDANO_CLI address build --payment-script-file "$plutusscriptinuse"  --testnet-magic "$TESTNET_MAGIC")

mkdir -p "$WORK"

utxoaddr=$($CARDANO_CLI address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY")

$CARDANO_CLI query utxo --address "$utxoaddr" --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-1.json
cat $WORK/utxo-1.json

txin=$(jq -r 'keys[]' $WORK/utxo-1.json)
lovelaceattxin=$(jq -r ".[\"$txin\"].value.lovelace" $WORK/utxo-1.json)
lovelaceattxindiv3=$(expr $lovelaceattxin / 3)

$CARDANO_CLI query protocol-parameters --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/pparams.json

$CARDANO_CLI transaction hash-script-data --script-data-value 42 > $WORK/datum.hash

$CARDANO_CLI address key-gen \
  --verification-key-file "$WORK/payment.vkey" \
  --signing-key-file "$WORK/payment.skey"

$CARDANO_CLI stake-address key-gen \
  --verification-key-file "$WORK/stake.vkey" \
  --signing-key-file "$WORK/stake.skey"

$CARDANO_CLI node key-gen-KES \
  --verification-key-file "$WORK/kes.vkey" \
  --signing-key-file "$WORK/kes.skey"

$CARDANO_CLI node key-gen-VRF \
  --verification-key-file "$WORK/vrf.vkey" \
  --signing-key-file "$WORK/vrf.skey"

$CARDANO_CLI node key-gen \
  --cold-verification-key-file "$WORK/cold.vkey" \
  --cold-signing-key-file "$WORK/cold.skey" \
  --operational-certificate-issue-counter "$WORK/node.counter"

$CARDANO_CLI stake-address build \
  --stake-verification-key-file "$WORK/stake.vkey" \
  --out-file "$WORK/stake.addr" \
  --testnet-magic "$TESTNET_MAGIC"

$CARDANO_CLI address build \
  --payment-verification-key-file "$WORK/payment.vkey" \
  --stake-verification-key-file "$WORK/stake.vkey" \
  --out-file "$WORK/paymentwithstake.addr" \
  --testnet-magic "$TESTNET_MAGIC"

$CARDANO_CLI stake-address registration-certificate \
  --stake-verification-key-file "$WORK/stake.vkey" \
  --out-file "$WORK/stake.cert"

cat > "$WORK/testpool.json" <<EOF
{
  "name": "TestPool",
  "description": "The pool that tests all the pools",
  "ticker": "TEST",
  "homepage": "https://teststakepool.com"
}
EOF

# $CARDANO_CLI shelley query tip \
#   --testnet-magic "$TESTNET_MAGIC" \
#   > "$WORK/query-tip.json"

# {
#     "blockNo": 27470,
#     "headerHash": "bd954e753c1131a6cb7ab3a737ca7f78e2477bea93db54511cedefe8899ebed0",
#     "slotNo": 656260
# }

$CARDANO_CLI shelley node issue-op-cert \
  --kes-verification-key-file "$WORK/kes.vkey" \
  --cold-signing-key-file "$WORK/cold.skey" \
  --operational-certificate-issue-counter "$WORK/node.counter" \
  --kes-period 182 \
  --out-file "$WORK/node.cert"

$CARDANO_CLI shelley stake-pool metadata-hash \
  --pool-metadata-file "$WORK/testPool.json"

$CARDANO_CLI shelley stake-pool registration-certificate \
  --cold-verification-key-file "$WORK/cold.vkey" \
  --vrf-verification-key-file "$WORK/vrf.vkey" \
  --pool-pledge 70000000000 \
  --pool-cost 4321000000 \
  --pool-margin 0.04 \
  --pool-reward-account-verification-key-file "$WORK/stake.vkey" \
  --pool-owner-stake-verification-key-file "$WORK/stake.vkey" \
  --testnet-magic "$TESTNET_MAGIC" \
  --pool-relay-ipv4 123.121.123.121 \
  --pool-relay-port 3000 \
  --metadata-url "https://git.io/JJWdJ" \
  --metadata-hash "d0e21b420a554d7d2d0d85c4e62a1980d6b9d8f7a2d885b3de0fff472e37241b" \
  --out-file "$WORK/pool-registration.cert"

$CARDANO_CLI transaction build \
  --alonzo-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --tx-in "$txin" \
  --change-address "$utxoaddr" \
  --protocol-params-file "$WORK/pparams.json" \
  --certificate-file "$WORK/pool-registration.cert" \
  --witness-override 3 \
  --out-file "$WORK/tx.body"

$CARDANO_CLI transaction sign \
  --tx-body-file "$WORK/tx.body" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY" \
  --signing-key-file "$WORK/cold.skey" \
  --signing-key-file "$WORK/stake.skey" \
  --out-file "$WORK/tx.tx"

# SUBMIT
$CARDANO_CLI transaction submit \
  --tx-file "$WORK/tx.tx" \
  --testnet-magic "$TESTNET_MAGIC"

echo "Pausing for 5 seconds..."
sleep 5

