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
plutusscriptinuse="$BASE/scripts/plutus/scripts/v2/context-equivalence-test.plutus"
# This datum hash is the hash of the typed 42
scriptdatumhash="fcaa61fb85676101d9e3398a484674e71c45c3fd41b492682f3b0054f4cf3273"
datumfilepath="$BASE/scripts/plutus/data/typed-42.datum"
redeemerfilepath="$BASE/scripts/plutus/data/script-context.redeemer"
certifyingscript="scripts/plutus/scripts/v2/stake-script.plutus"

# Create certificate
cardano-cli stake-address registration-certificate \
  --stake-script-file "$certifyingscript" \
  --out-file "$WORK/script.regcert"

# Step 1: Create a tx output with a datum hash at the script address. In order for a tx output to be locked
# by a plutus script, it must have a datahash. We also need collateral tx inputs so we split the utxo
# in order to accommodate this.


plutusscriptaddr=$($CARDANO_CLI address build --payment-script-file "$plutusscriptinuse"  --testnet-magic "$TESTNET_MAGIC")
# The input at the readonlyaddress will be used as a reference input
readonlyaddress=addr_test1vz3t3f2kgy2re66tnhgxc4t8jgylw2cqfnxdwlrq9agfmtstxxkm5

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
  --tx-out "$readonlyaddress+$lovelaceattxindiv6" \
  --tx-out "$plutusscriptaddr+$lovelaceattxindiv6" \
  --tx-out-datum-hash "$scriptdatumhash" \
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

# Get read only reference input
$CARDANO_CLI query utxo --address "$readonlyaddress" --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/read-only-ref-input-utxo.json
readonlyrefinput=$(jq -r 'keys[0]' $WORK/read-only-ref-input-utxo.json)



# We need to generate a dummy redeemer (the cli demands a redeemer) in order to create a txbody from which we can generate
# a tx and then derive the correct redeemer.
create-script-context --plutus-v2 --out-file "$WORK/script-context.redeemer"

correctredeemer="$WORK/script-context.redeemer"

echo "Constructing dummy tx..."

# Create dummy certificate
certifyingscript="scripts/plutus/scripts/v2/stake-script.plutus"
cardano-cli stake-address registration-certificate \
  --stake-script-file "$certifyingscript" \
  --out-file "$WORK/script.regcert"

requiredsignerhash=$(cardano-cli address key-hash --payment-verification-key-file ${UTXO_VKEY})

# DUMMY TX! We generate the actual redeemer from this!
#   --certificate-file "$WORK/script.regcert" \
redeemerfilepath="$BASE/scripts/plutus/data/42.redeemer"
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --script-invalid \
  --change-address "$utxoaddr" \
  --invalid-before 1 \
  --invalid-hereafter 400 \
  --certificate-file "$WORK/script.regcert" \
  --read-only-tx-in-reference "$readonlyrefinput" \
  --tx-in "$plutusutxotxin" \
  --tx-in-collateral "$txinCollateral" \
  --tx-out "$dummyaddress+10000000" \
  --tx-in-script-file "$plutusscriptinuse" \
  --tx-in-datum-file "$datumfilepath"  \
  --protocol-params-file "$WORK/pparams.json" \
  --tx-in-redeemer-file "$redeemerfilepath" \
  --required-signer-hash "$requiredsignerhash" \
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
  --script-valid \
  --invalid-before 1 \
  --invalid-hereafter 400 \
  --change-address "$utxoaddr" \
  --certificate-file "$WORK/script.regcert" \
  --read-only-tx-in-reference "$readonlyrefinput" \
  --tx-in "$plutusutxotxin" \
  --tx-in-collateral "$txinCollateral" \
  --tx-out "$dummyaddress+10000000" \
  --tx-in-script-file "$plutusscriptinuse" \
  --tx-in-datum-file "$datumfilepath"  \
  --protocol-params-file "$WORK/pparams.json" \
  --tx-in-redeemer-file "$correctredeemer" \
  --required-signer-hash "$requiredsignerhash" \
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
