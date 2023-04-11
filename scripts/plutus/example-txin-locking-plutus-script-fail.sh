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

if [ "$1" == "guessinggame" ]; then
 # NB: This plutus script uses a "typed" redeemer and "typed" datum.
 plutusscriptinuse="$BASE/scripts/plutus/scripts/custom-guess-42-datum-42.plutus"
 # This datum hash is the hash of the typed 42
 scriptdatumhash="fcaa61fb85676101d9e3398a484674e71c45c3fd41b492682f3b0054f4cf3273"
 datumfilepath="$BASE/scripts/plutus/data/typed-42.datum"
 redeemerfilepath="$BASE/scripts/plutus/data/typed-43.redeemer"
 echo "Guessing game Plutus script in use. The datum and redeemer must be equal to 42."
 echo "Script at: $plutusscriptinuse"

elif [ "$1" == "" ]; then
 plutusscriptinuse="$BASE/scripts/plutus/scripts/always-succeeds-spending.plutus"
 # This datum hash is the hash of the untyped 42
 scriptdatumhash="9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"
 datumfilepath="$BASE/scripts/plutus/data/42.datum"
 redeemerfilepath="$BASE/scripts/plutus/data/42.redeemer"
 echo "Always succeeds Plutus script in use. Any datum and redeemer combination will succeed."
 echo "Script at: $plutusscriptinuse"
fi


# Step 1: Create a tx output with a datum hash at the script address. In order for a tx output to be locked
# by a plutus script, it must have a datahash. We also need collateral tx inputs so we split the utxo
# in order to accommodate this.


plutusscriptaddr=$($CARDANO_CLI address build --payment-script-file "$plutusscriptinuse"  --testnet-magic "$TESTNET_MAGIC")

mkdir -p "$WORK"

utxoaddr=$($CARDANO_CLI address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY")

$CARDANO_CLI query utxo --address "$utxoaddr" --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-1.json
cat $WORK/utxo-1.json

txin=$(jq -r 'keys[0]' $WORK/utxo-1.json)
lovelaceattxin=$(jq -r ".[\"$txin\"].value.lovelace" $WORK/utxo-1.json)
lovelaceattxindiv3=$(expr $lovelaceattxin / 3)

$CARDANO_CLI query protocol-parameters --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/pparams.json

$CARDANO_CLI transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$plutusscriptaddr+$lovelaceattxindiv3" \
  --tx-out-datum-hash "$scriptdatumhash" \
  --tx-out "$utxoaddr+$lovelaceattxindiv3" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/create-datum-output.body"

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

plutusutxotxin=$(jq -r 'keys[0]' $WORK/plutusutxo.json)

$CARDANO_CLI query utxo --address $utxoaddr --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-2.json
cat $WORK/utxo-2.json
txinCollateral=$(jq -r 'to_entries | map(select(.value.value.lovelace > 10000000000))[0].key' $WORK/utxo-2.json)


dummyaddress=addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4

lovelaceatplutusscriptaddr=$(jq -r ".[\"$plutusutxotxin\"].value.lovelace" $WORK/plutusutxo.json)

echo "Plutus txin"
echo "$plutusutxotxin|"

echo "Collateral"
echo "$txinCollateral|"

$CARDANO_CLI transaction build-raw \
  --alonzo-era \
  --fee 20000000 \
  --tx-in "$plutusutxotxin" \
  --tx-in-collateral "$txinCollateral" \
  --tx-out "$dummyaddress+299980000000" \
  --tx-in-script-file "$plutusscriptinuse" \
  --tx-in-datum-file "$datumfilepath"  \
  --protocol-params-file "$WORK/pparams.json" \
  --tx-in-redeemer-file "$redeemerfilepath" \
  --tx-in-execution-units '(1, 1)' \
  --out-file $WORK/test-alonzo.body

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/test-alonzo.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "${UTXO_SKEY}" \
  --out-file $WORK/alonzo.tx

# SUBMIT $WORK/alonzo.tx
echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/alonzo.tx --testnet-magic "$TESTNET_MAGIC" --error-detail-out "$WORK/error-detail.json"
sleep 5
echo ""
echo "Querying UTxO at $dummyaddress. If there is ADA at the address the Plutus script successfully executed!"
echo ""
$CARDANO_CLI query utxo --address "$dummyaddress"  --testnet-magic "$TESTNET_MAGIC" \
  | tee "$RESULT_FILE"
