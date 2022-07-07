#!/usr/bin/env bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

# This is an example plutus minting script derived from the validator in
# Cardano.CLI.Plutus.SimpleMintingScript. Essentially we demand that the
# script owner is allowed to mint "MillarCoin" and you can only mint
# a single "MillarCoin" per tx.

work=example/minting
mkdir -p $work

# Step 1 - Send ADA to token script owner
export WORKD="${WORKD:-example/work}"
export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/main.sock}"
export PV=v1 # Plutus Script Version

mkdir -p "$WORKD"
plutusscriptinuse=scripts/plutus/scripts/$PV/minting-context-equivalance-test.plutus

utxovkey=example/utxo-keys/utxo1.vkey
utxoskey=example/utxo-keys/utxo1.skey
utxoaddr=$(cardano-cli address build --testnet-magic 42 --payment-verification-key-file $utxovkey)
cardano-cli query utxo --address $utxoaddr --cardano-mode --testnet-magic 42 --out-file utxo.json
txin=$(jq -r 'keys[]' utxo.json)

lovelaceattxin=$(jq -r ".[\"$txin\"].value.lovelace" utxo.json)
lovelaceattxindiv3=$(expr $lovelaceattxin / 3)

cardano-cli address key-gen \
  --normal-key \
  --verification-key-file "$work/minting.vkey" \
  --signing-key-file "$work/minting.skey"

targetvkey="$work/minting.vkey"
targetskey="$work/minting.skey"
targetaddr=$(cardano-cli address build --testnet-magic 42 --payment-verification-key-file $targetvkey)

cardano-cli query protocol-parameters --testnet-magic 42 --out-file example/pparams.json

cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 42 \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$targetaddr+$lovelaceattxindiv3" \
  --tx-out "$targetaddr+$lovelaceattxindiv3" \
  --protocol-params-file example/pparams.json \
  --out-file "$work/fund-script-owner.body"

cardano-cli transaction sign \
  --tx-body-file "$work/fund-script-owner.body" \
  --testnet-magic 42 \
  --signing-key-file "$utxoskey" \
  --out-file "$work/fund-script-owner.tx"

# SUBMIT
cardano-cli transaction submit --tx-file "$work/fund-script-owner.tx" --testnet-magic 42

echo "Pausing for 5 seconds..."
sleep 5

cardano-cli query utxo --address "$targetaddr" --testnet-magic 42 --out-file "$work/updatedutxo.json"
scriptownertxin=$(jq -r 'keys[0]' "$work/updatedutxo.json")
scriptownerCollateral=$(jq -r 'keys[1]' "$work/updatedutxo.json")

# Step 2: Mint a single MillarCoin

# We need the script policy ID
policyid=$(cardano-cli transaction policyid --script-file $plutusscriptinuse)
redeemer=scripts/plutus/data/42.redeemer
lovelaceatplutusscriptaddr=$(jq -r ".[\"$scriptownertxin\"].value.lovelace" "$work/updatedutxo.json")

dummyaddress=addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4

echo "Lovelace at address: $lovelaceatplutusscriptaddr"

# Dummy tx
cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 42 \
  --script-invalid \
  --invalid-before 5 \
  --invalid-hereafter 500 \
  --required-signer "cardano-cli/test/data/golden/shelley/keys/payment_keys/signing_key" \
  --change-address "$utxoaddr" \
  --tx-in "$scriptownertxin" \
  --tx-in-collateral "$scriptownerCollateral" \
  --mint-script-file "$plutusscriptinuse" \
  --mint-redeemer-file "$redeemer" \
  --tx-out "$dummyaddress+1000000 + 5 $policyid.MillarCoin" \
  --mint "5 $policyid.MillarCoin" \
  --protocol-params-file example/pparams.json \
  --out-file "$work/plutusmint.body"

cardano-cli transaction sign \
  --tx-body-file "$work/plutusmint.body" \
  --testnet-magic 42 \
  --signing-key-file "$targetskey" \
  --signing-key-file "cardano-cli/test/data/golden/shelley/keys/payment_keys/signing_key" \
  --out-file "$work/plutusmint.tx"

# Generate the "real" redeeemer!
correctredeemer="$work/script-context.redeemer"

create-script-context \
  --generate-tx "$work/plutusmint.tx" \
  --cardano-mode \
  --testnet-magic 42 \
  --out-file "$correctredeemer" \


cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic 42 \
  --script-valid \
  --invalid-before 5 \
  --invalid-hereafter 500 \
  --required-signer "cardano-cli/test/data/golden/shelley/keys/payment_keys/signing_key" \
  --change-address "$utxoaddr" \
  --tx-in "$scriptownertxin" \
  --tx-in-collateral "$scriptownerCollateral" \
  --mint-script-file "$plutusscriptinuse" \
  --mint-redeemer-file "$correctredeemer" \
  --tx-out "$dummyaddress+1000000 + 5 $policyid.MillarCoin" \
  --mint "5 $policyid.MillarCoin" \
  --protocol-params-file example/pparams.json \
  --out-file "$work/plutusmint-final.body"

cardano-cli transaction sign \
  --tx-body-file "$work/plutusmint-final.body" \
  --testnet-magic 42 \
  --signing-key-file "$targetskey" \
  --signing-key-file "cardano-cli/test/data/golden/shelley/keys/payment_keys/signing_key" \
  --out-file "$work/plutusmint-final.tx"

# SUBMIT
cardano-cli transaction submit --tx-file "$work/plutusmint-final.tx" --testnet-magic 42

echo "Pausing for 5 seconds..."
sleep 5
cardano-cli query utxo --whole-utxo --testnet-magic 42