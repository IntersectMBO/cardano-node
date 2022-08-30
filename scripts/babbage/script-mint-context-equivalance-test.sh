#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

export WORK="${WORK:-example/work}"
export BASE="${BASE:-.}"
export CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/main.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export UTXO_VKEY="${UTXO_VKEY:-example/utxo-keys/utxo1.vkey}"
export UTXO_SKEY="${UTXO_SKEY:-example/utxo-keys/utxo1.skey}"
export RESULT_FILE="${RESULT_FILE:-$WORK/result.out}"

mkdir -p "$WORK"

echo "Socket path: $CARDANO_NODE_SOCKET_PATH"

ls -al "$CARDANO_NODE_SOCKET_PATH"

# NB: This plutus script uses a "typed" redeemer and "typed" datum.
plutusscriptinuse="$BASE/scripts/plutus/scripts/v2/minting-context-equivalance-test.plutus"
policyid=$(cardano-cli transaction policyid --script-file $plutusscriptinuse)
redeemerfilepath="$BASE/scripts/plutus/data/script-context.redeemer"

mkdir -p "$WORK"

utxoaddr=$($CARDANO_CLI address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY")

$CARDANO_CLI query utxo --address "$utxoaddr" --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-1.json
cat $WORK/utxo-1.json

txin=$(jq -r 'keys[]' $WORK/utxo-1.json)
lovelaceattxin=$(jq -r ".[\"$txin\"].value.lovelace" $WORK/utxo-1.json)
lovelaceattxindiv6=$(expr $lovelaceattxin / 6)

$CARDANO_CLI query protocol-parameters --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/pparams.json

$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$utxoaddr+$lovelaceattxindiv6" \
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


$CARDANO_CLI query utxo --address $utxoaddr --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-2.json
cat $WORK/utxo-2.json
txinCollateral=$(jq -r 'keys[0]' $WORK/utxo-2.json)
txinfunding1=$(jq -r 'keys[1]' $WORK/utxo-2.json)


dummyaddress=addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4


# We need to generate a dummy redeemer (the cli demands a redeemer)  in order to create a txbody from which we can generate
# a tx and then derive the correct redeemer.
create-script-context --plutus-v2 --out-file "$WORK/script-context.redeemer"

correctredeemer="$WORK/script-context.redeemer"

echo "Constructing dummy tx..."

# DUMMY TX! We generate the actual redeemer from this!
redeemerfilepath="$BASE/scripts/plutus/data/42.redeemer"
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --script-invalid \
  --change-address "$utxoaddr" \
  --tx-in-collateral "$txinCollateral" \
  --tx-in "$txinfunding1" \
  --mint "5 $policyid.4D696C6C6172436F696E" \
  --mint-script-file "$plutusscriptinuse" \
  --mint-redeemer-file "$correctredeemer" \
  --tx-out "$dummyaddress+2000000 + 5 $policyid.4D696C6C6172436F696E" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file $WORK/test-alonzo.body

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/test-alonzo.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "${UTXO_SKEY}" \
  --out-file $WORK/test-alonzo.tx

# Generate the "real" redeeemer!

create-script-context \
  --generate-tx "$WORK/test-alonzo.tx" \
  --plutus-v2 \
  --out-file "$WORK/script-context.redeemer" \
  --cardano-mode \
  --testnet-magic 42 \

echo "Constructing real tx..."
# REAL TX!

$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in-collateral "$txinCollateral" \
  --tx-in "$txinfunding1" \
  --mint "5 $policyid.4D696C6C6172436F696E" \
  --mint-script-file "$plutusscriptinuse" \
  --mint-redeemer-file "$correctredeemer" \
  --tx-out "$dummyaddress+2000000 + 5 $policyid.4D696C6C6172436F696E" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file $WORK/test-alonzo-final.body

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/test-alonzo-final.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "${UTXO_SKEY}" \
  --out-file $WORK/alonzo.tx

# SUBMIT $WORK/alonzo.tx containing the correct redeemer

echo "Submit the tx with plutus script and wait 5 seconds..."
$CARDANO_CLI transaction submit --tx-file $WORK/alonzo.tx --testnet-magic "$TESTNET_MAGIC"
sleep 5
echo ""
echo "Querying UTxO at $dummyaddress. If there is ADA at the address the Plutus script successfully executed!"
echo ""
$CARDANO_CLI query utxo --address "$dummyaddress"  --testnet-magic "$TESTNET_MAGIC" \
  | tee "$RESULT_FILE"
