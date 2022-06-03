#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

export WORK="${WORK:-example/work}"
export BASE="${BASE:-.}"
export CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/node-spo1/node.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export UTXO_VKEY="${UTXO_VKEY:-example/stake-delegator-keys/payment1.vkey}"
export UTXO_SKEY="${UTXO_SKEY:-example/stake-delegator-keys/payment1.skey}"
export RESULT_FILE="${RESULT_FILE:-$WORK/result.out}"

echo "Socket path: $CARDANO_NODE_SOCKET_PATH"
echo "Socket path: $(pwd)"

ls -al "$CARDANO_NODE_SOCKET_PATH"

plutusscriptinuse="$BASE/scripts/plutus/scripts/v2/required-redeemer.plutus"
## This datum hash is the hash of the untyped 42
scriptdatumhash="9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"
datumfilepath="$BASE/scripts/plutus/data/42.datum"
redeemerfilepath="$BASE/scripts/plutus/data/42.redeemer"
echo "Script at: $plutusscriptinuse"
#
#
#
## Step 1: Create a tx output with a datum hash at the script address. In order for a tx output to be locked
## by a plutus script, it must have a datahash. We also need collateral tx inputs so we split the utxo
## in order to accommodate this.
#
#
plutusscriptaddr=$($CARDANO_CLI address build --payment-script-file "$plutusscriptinuse"  --testnet-magic "$TESTNET_MAGIC")
echo "Plutus Script Address"
echo "$plutusscriptaddr"
mkdir -p "$WORK"

utxoaddr=$($CARDANO_CLI address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY" --stake-verification-key-file example/stake-delegator-keys/staking1.vkey)

$CARDANO_CLI query utxo --address "$utxoaddr" --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-1.json
cat $WORK/utxo-1.json

txin=$(jq -r 'keys[0]' $WORK/utxo-1.json)
lovelaceattxin=$(jq -r ".[\"$txin\"].value.lovelace" $WORK/utxo-1.json)
lovelaceattxindiv3=$(expr $lovelaceattxin / 4)

$CARDANO_CLI query protocol-parameters --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/pparams.json
dummyaddress=addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4

# We first:
# - Create the reference script at the utxoaddr
# - Send ADA and a datum to the reference script address
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$utxoaddr+$lovelaceattxindiv3" \
  --tx-out "$plutusscriptaddr+$lovelaceattxindiv3" \
  --tx-out-datum-hash "$scriptdatumhash" \
  --tx-out "$dummyaddress+$lovelaceattxindiv3" \
  --reference-script-file "$plutusscriptinuse" \
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

$CARDANO_CLI query utxo --address "$dummyaddress" --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/dummy-address-ref-script.json
cat $WORK/dummy-address-ref-script.json
# Get reference script txin
plutusreferencescripttxin=$(jq -r 'keys[0]' $WORK/dummy-address-ref-script.json)

# Step 2
# After "locking" the tx output at the script address, let's spend the utxo as the script address using the corresponding reference script

## Get funding inputs
$CARDANO_CLI query utxo --address "$utxoaddr" --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-2.json

txin1=$(jq -r 'keys[0]' $WORK/utxo-2.json)
txinCollateral=$(jq -r 'keys[1]' $WORK/utxo-2.json)
lovelaceattxin1=$(jq -r ".[\"$txin1\"].value.lovelace" $WORK/utxo-2.json)
lovelaceattxindiv31=$(expr $lovelaceattxin1 / 3)


# Get input at plutus script that we will attempt to spend
$CARDANO_CLI query utxo --address $plutusscriptaddr --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/plutusutxo.json
plutuslockedutxotxin=$(jq -r 'keys[0]' $WORK/plutusutxo.json)
lovelaceatplutusscriptaddr=$(jq -r ".[\"$plutuslockedutxotxin\"].value.lovelace" $WORK/plutusutxo.json)

dummyaddress2=addr_test1vzq57nyrwdwne9vzjxr908qqkdxwuavlgzl20qveua303vq024qkk


echo "Plutus txin"
echo "$plutuslockedutxotxin"

echo "Collateral"
echo "$txinCollateral"

echo "Funding utxo"
echo "$txin1"

echo "Plutus reference script txin"
echo "$plutusreferencescripttxin"

echo "Plutus input we are trying to spend"
echo "$plutuslockedutxotxin"

# Alternative build-raw method
#$CARDANO_CLI transaction build-raw \
#  --babbage-era \
#  --script-valid \
#  --tx-in "$txin1" \
#  --tx-in-collateral "$txinCollateral" \
#  --out-file "$WORK/test-alonzo-ref-script.body" \
#  --tx-in "$plutuslockedutxotxin" \
#  --tx-in-reference "$plutusreferencescripttxin" \
#  --plutus-script-v2 \
#  --reference-tx-in-datum-file "$datumfilepath"  \
#  --reference-tx-in-redeemer-file "$redeemerfilepath" \
#  --reference-tx-in-execution-units "(1000022165, 1000065)" \
#  --tx-out "$dummyaddress2+10000000" \
#  --tx-out "$utxoaddr+149988741087" \
#  --fee "1000000" \
#  --protocol-params-file "$WORK/pparams.json"

$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin1" \
  --tx-in-collateral "$txinCollateral" \
  --out-file "$WORK/test-alonzo-ref-script.body" \
  --tx-in "$plutuslockedutxotxin" \
  --tx-in-reference "$plutusreferencescripttxin" \
  --plutus-script-v2 \
  --reference-tx-in-datum-file "$datumfilepath"  \
  --reference-tx-in-redeemer-file "$redeemerfilepath" \
  --tx-out "$dummyaddress2+10000000" \
  --protocol-params-file "$WORK/pparams.json"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/test-alonzo-ref-script.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "${UTXO_SKEY}" \
  --out-file $WORK/alonzo-ref-script.tx

# SUBMIT $WORK/alonzo.tx
echo "Submit the tx using reference script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/alonzo-ref-script.tx --testnet-magic "$TESTNET_MAGIC"
sleep 5
echo ""
echo "Querying UTxO at $dummyaddress2. If there is ADA at the address the Plutus reference script successfully executed!"
echo ""
$CARDANO_CLI query utxo --address "$dummyaddress2"  --testnet-magic "$TESTNET_MAGIC" \
  | tee "$RESULT_FILE"
