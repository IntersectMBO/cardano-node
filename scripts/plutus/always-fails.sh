#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

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

# Step 1: Create a tx output with a datum hash at the script address. In order for a tx output to be locked
# by a plutus script, it must have a datahash. We also need collateral tx inputs so we split the utxo
# in order to accommodate this.

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

$CARDANO_CLI transaction build --alonzo-era \
  --tx-in "$txin" \
  --tx-out "$plutusscriptaddr+$lovelaceattxindiv3" \
  --tx-out-datum-hash "$scriptdatumhash" \
  --change-address "$utxoaddr" \
  --protocol-params-file "$WORK/pparams.json" \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file $WORK/create-datum-output.body

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/create-datum-output.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file $UTXO_SKEY \
  --out-file $WORK/create-datum-output.tx

# SUBMIT
$CARDANO_CLI transaction submit --tx-file $WORK/create-datum-output.tx --testnet-magic "$TESTNET_MAGIC"
echo "Pausing for 5 seconds..."
sleep 5

# Step 2
# After "locking" the tx output at the script address, we can now can attempt to spend
# the "locked" tx output below.

$CARDANO_CLI query utxo --address $plutusscriptaddr --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/plutusutxo.json

plutusutxotxin=$(jq -r 'keys[]' $WORK/plutusutxo.json)

$CARDANO_CLI query utxo --address $utxoaddr --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-2.json
cat $WORK/utxo-2.json
txinCollateral=$(jq -r 'keys[0]' $WORK/utxo-2.json)


dummyaddress=addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4

lovelaceatplutusscriptaddr=$(jq -r ".[\"$plutusutxotxin\"].value.lovelace" $WORK/plutusutxo.json)

echo "Plutus txin"
echo "$plutusutxotxin"

echo "Collateral"
echo "$txinCollateral"

$CARDANO_CLI transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$plutusutxotxin" \
  --tx-in-collateral "$txinCollateral" \
  --tx-out "$dummyaddress+100000" \
  --tx-in-script-file "$plutusscriptinuse" \
  --tx-in-datum-file "$datumfilepath"  \
  --protocol-params-file "$WORK/pparams.json" \
  --tx-in-redeemer-file "$redeemerfilepath" \
  --script-invalid \
  --out-file $WORK/test-alonzo.body

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/test-alonzo.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "${UTXO_SKEY}" \
  --out-file $WORK/alonzo.tx

echo "UTxO for collateral address before submission"
$CARDANO_CLI query utxo --address "$utxoaddr"  --testnet-magic "$TESTNET_MAGIC"

# SUBMIT $WORK/alonzo.tx
echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/alonzo.tx --testnet-magic "$TESTNET_MAGIC"

sleep 5

echo ""
echo "UTxO for collateral address after submission.  If there is no ADA at the address the collateral was successfully taken!"
$CARDANO_CLI query utxo --address "$utxoaddr"  --testnet-magic "$TESTNET_MAGIC"
echo ""

echo ""
echo "Querying UTxO at $dummyaddress."
echo ""
$CARDANO_CLI query utxo --address "$dummyaddress"  --testnet-magic "$TESTNET_MAGIC" \
  | tee "$RESULT_FILE"
