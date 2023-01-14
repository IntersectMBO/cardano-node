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
export UTXO_VKEY="${UTXO_VKEY:-example/utxo-keys/utxo1.vkey}"
export UTXO_SKEY="${UTXO_SKEY:-example/utxo-keys/utxo1.skey}"
export RESULT_FILE="${RESULT_FILE:-$WORK/result.out}"

echo "Socket path: $CARDANO_NODE_SOCKET_PATH"

ls -al "$CARDANO_NODE_SOCKET_PATH"

plutusspendingscript="$BASE/scripts/plutus/scripts/v2/required-redeemer.plutus"
plutusmintingscript="$BASE/scripts/plutus/scripts/v2/minting-script.plutus"
plutusstakescript="scripts/plutus/scripts/v2/stake-script.plutus"
mintpolicyid=$($CARDANO_CLI transaction policyid --script-file $plutusmintingscript)
## This datum hash is the hash of the untyped 42
scriptdatumhash="9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"
datumfilepath="$BASE/scripts/plutus/data/42.datum"
redeemerfilepath="$BASE/scripts/plutus/data/42.redeemer"
echo "Script at: $plutusspendingscript"
#
#
#
## Step 1: Create a tx output with a datum hash at the script address. In order for a tx output to be locked
## by a plutus script, it must have a datahash. We also need collateral tx inputs so we split the utxo
## in order to accommodate this.
#
#
plutusspendingscriptaddr=$($CARDANO_CLI address build --payment-script-file "$plutusspendingscript"  --testnet-magic "$TESTNET_MAGIC")
echo "Plutus Script Address"
echo "$plutusspendingscriptaddr"
mkdir -p "$WORK"

utxoaddr=$($CARDANO_CLI address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY")

$CARDANO_CLI query utxo --address "$utxoaddr" --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-1.json
cat $WORK/utxo-1.json

txin=$(jq -r 'keys[0]' $WORK/utxo-1.json)
lovelaceattxin=$(jq -r ".[\"$txin\"].value.lovelace" $WORK/utxo-1.json)
lovelaceattxindiv6=$(expr $lovelaceattxin / 6)

$CARDANO_CLI query protocol-parameters --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/pparams.json
dummyaddress=addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4
dummyaddress2=addr_test1vzq57nyrwdwne9vzjxr908qqkdxwuavlgzl20qveua303vq024qkk
addressformintingrefscript=addr_test1vq73yuplt9c5zmgw4ve7qhu49yxllw7q97h4smwvfgst32qrkwupd
readonlyaddress=addr_test1vz3t3f2kgy2re66tnhgxc4t8jgylw2cqfnxdwlrq9agfmtstxxkm5

# We first:
# - Create the reference script at the utxoaddr
# - Send ADA and a datum to the reference script address
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$readonlyaddress+$lovelaceattxindiv6" \
  --tx-out-reference-script-file "$plutusstakescript" \
  --tx-out-inline-datum-file "$datumfilepath" \
  --tx-out "$utxoaddr+$lovelaceattxindiv6" \
  --tx-out "$plutusspendingscriptaddr+$lovelaceattxindiv6" \
  --tx-out-inline-datum-file "$datumfilepath" \
  --tx-out "$dummyaddress+$lovelaceattxindiv6" \
  --tx-out-inline-datum-file "$datumfilepath" \
  --tx-out-reference-script-file "$plutusspendingscript" \
  --tx-out "$addressformintingrefscript+$lovelaceattxindiv6" \
  --tx-out-reference-script-file "$plutusmintingscript" \
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
suppliedCollateral=$(jq -r ".[\"$txinCollateral\"].value.lovelace" $WORK/utxo-2.json)

# Get input at plutus script that we will attempt to spend
$CARDANO_CLI query utxo --address $plutusspendingscriptaddr --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/plutusutxo.json
plutuslockedutxotxin=$(jq -r 'keys[0]' $WORK/plutusutxo.json)
lovelaceatplutusspendingscriptaddr=$(jq -r ".[\"$plutuslockedutxotxin\"].value.lovelace" $WORK/plutusutxo.json)

# Get read only reference input
$CARDANO_CLI query utxo --address "$readonlyaddress" --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/read-only-ref-input-utxo.json
readonlyrefinput=$(jq -r 'keys[0]' $WORK/read-only-ref-input-utxo.json)

#Get minting reference script input
$CARDANO_CLI query utxo --address "$addressformintingrefscript" --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/minting-script-ref-input-utxo.json
mintingscriptrefinput=$(jq -r 'keys[0]' $WORK/minting-script-ref-input-utxo.json)

echo "Plutus txin"
echo "$plutuslockedutxotxin"
echo ""
echo "Collateral"
echo "$txinCollateral"
echo "$suppliedCollateral"
echo ""
echo "Funding utxo"
echo "$txin1"
echo ""
echo "Plutus reference script txin"
echo "$plutusreferencescripttxin"
echo ""
echo "Plutus input we are trying to spend"
echo "$plutuslockedutxotxin"

echo "Policy id"
echo "$mintpolicyid"

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
#   --tx-out-return-collateral
returncollateral=$(expr $suppliedCollateral - 529503)

echo "Return collateral amount"
echo "$returncollateral"

$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --read-only-tx-in-reference  "$readonlyrefinput" \
  --tx-in "$txin1" \
  --tx-in-collateral "$txinCollateral" \
  --tx-total-collateral 529503 \
  --tx-out-return-collateral "$utxoaddr+$returncollateral" \
  --out-file "$WORK/test-alonzo-ref-script.body" \
  --tx-in "$plutuslockedutxotxin" \
  --spending-tx-in-reference "$plutusreferencescripttxin" \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file "$redeemerfilepath" \
  --mint "5 $mintpolicyid.4D696C6C6172436F696E" \
  --mint-tx-in-reference "$mintingscriptrefinput" \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file "$redeemerfilepath" \
  --policy-id "$mintpolicyid" \
  --tx-out "$dummyaddress2+10000000 + 5 $mintpolicyid.4D696C6C6172436F696E" \
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
