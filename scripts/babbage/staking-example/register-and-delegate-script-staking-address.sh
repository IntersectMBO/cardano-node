#!/usr/bin/env bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail


export WORK="${WORK:-example/work}"
export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/main.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export UTXO_VKEY1="${UTXO_VKEY1:-example/utxo-keys/utxo1.vkey}"
export UTXO_SKEY1="${UTXO_SKEY1:-example/utxo-keys/utxo1.skey}"
export UTXO_VKEY2="${UTXO_VKEY2:-example/utxo-keys/utxo2.vkey}"
export UTXO_STAKING_VKEY2="${UTXO_STAKING_VKEY2:=example/utxo-keys/utxo2-stake.vkey}"
export PV=v2 # Plutus Script Version

mkdir -p "$WORK"
nodepool1dir=example/node-spo1

# Pool 1 related
poolownerstakekey="example/pools/staking-reward1.vkey"


# Generate stake keys
cardano-cli stake-address key-gen \
  --verification-key-file example/utxo-keys/utxo-stake.vkey \
  --signing-key-file example/utxo-keys/utxo-stake.skey

cardano-cli stake-address key-gen \
  --verification-key-file example/utxo-keys/utxo2-stake.vkey \
  --signing-key-file example/utxo-keys/utxo2-stake.skey


# We want to delegagate the stake from UTXO_STAKING_VKEY2 to node pool 1
utxoaddrwithstaking=$(cardano-cli address build --payment-verification-key-file "$UTXO_VKEY2" --stake-verification-key-file "$UTXO_STAKING_VKEY2" --testnet-magic 42)
keystakeaddress=$(cardano-cli stake-address build --stake-verification-key-file "$UTXO_STAKING_VKEY2" --testnet-magic 42)

utxoaddr=$(cardano-cli address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY1")


cardano-cli query utxo \
  --address "$utxoaddr" \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/utxo-1.json"

echo "UTxO"
cat "$WORK/utxo-1.json"
echo ""

txin=$(jq -r 'keys[]' "$WORK/utxo-1.json")
lovelaceattxin=$(jq -r ".[\"$txin\"].value.lovelace" "$WORK/utxo-1.json")
lovelaceattxindiv3=$((lovelaceattxin / 3))
certifyingscript="scripts/plutus/scripts/v2/stake-script.plutus"
scriptpaymentaddrwithstakecred=$(cardano-cli address build --payment-verification-key-file "$UTXO_VKEY1"  --stake-script-file $certifyingscript --testnet-magic 42)
dummyaddress=addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4

# Node Pool 1
poolcoldkey="example/pools/cold1.vkey"

# Delegate Plutus staking script address to stake pool.

cardano-cli query protocol-parameters --testnet-magic "$TESTNET_MAGIC" --out-file "$WORK/pparams.json"

# Step 1: Create certifying reference script at tx out

cardano-cli transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$scriptpaymentaddrwithstakecred+999978" \
  --tx-out "$dummyaddress+$lovelaceattxindiv3" \
  --tx-out-reference-script-file "$certifyingscript" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/create-stake-reference-script.body"

cardano-cli transaction sign \
  --tx-body-file "$WORK/create-stake-reference-script.body" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY1" \
  --out-file "$WORK/create-stake-reference-script.tx"

cardano-cli transaction submit \
  --tx-file "$WORK/create-stake-reference-script.tx" \
  --testnet-magic "$TESTNET_MAGIC"

echo ""
echo "Wait 5 seconds..."
echo ""
sleep 5

# Step 2: Create and submit registration certificate for the staking script
cardano-cli query utxo \
  --address "$utxoaddr" \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/utxo-2.json"

cat "$WORK/utxo-2.json"
# We also create collateral.

txin2=$(jq -r 'keys[0]' "$WORK/utxo-2.json")
lovelaceattxin2=$(jq -r ".[\"$txin2\"].value.lovelace" "$WORK/utxo-2.json")
lovelaceattxin2div3=$(($lovelaceattxin2 / 3))

cardano-cli stake-address registration-certificate \
  --stake-script-file "$certifyingscript" \
  --out-file "$WORK/script.regcert"

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin2" \
  --tx-out "$utxoaddr+$lovelaceattxin2div3" \
  --witness-override 3 \
  --certificate-file "$WORK/script.regcert" \
  --out-file "$WORK/script-registration-cert.txbody"

cardano-cli transaction sign \
  --tx-body-file "$WORK/script-registration-cert.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY1" \
  --out-file "$WORK/script-registration-cert.tx"

cardano-cli transaction submit \
  --tx-file "$WORK/script-registration-cert.tx" \
  --testnet-magic "$TESTNET_MAGIC"

stakingscriptaddr=$(cardano-cli stake-address build --stake-script-file $certifyingscript --testnet-magic 42)

echo ""
echo "Staking script address"
echo "$stakingscriptaddr"
echo "Waiting 10 seconds..."
sleep 10
echo "Check to see if the SCRIPT staking address was successfully REGISTERED"

cardano-cli query stake-address-info \
  --address "$stakingscriptaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/scriptregistration.json"

registeredscr=$(jq -r '.[0]' "$WORK/scriptregistration.json")
echo "$registeredscr"

# We have successfully registered our script staking address.

# We need to delegate the script staking address

# Retrieve plutus reference script tx in
cardano-cli query utxo --address "$dummyaddress" --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/dummy-address-ref-script.json
cat $WORK/dummy-address-ref-script.json
# Get reference script txin
plutusreferencescripttxin=$(jq -r 'keys[0]' $WORK/dummy-address-ref-script.json)


cardano-cli stake-address delegation-certificate \
  --stake-script-file "$certifyingscript" \
  --cold-verification-key-file "$poolcoldkey" \
  --out-file "$WORK/script.delegcert"


# We also need collateral

cardano-cli query utxo \
  --address "$utxoaddr" \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/utxo-2.json"

cat "$WORK/utxo-2.json"

txinupdated3=$(jq -r 'keys[0]' "$WORK/utxo-2.json")
txincollateral=$(jq -r 'keys[1]' "$WORK/utxo-2.json")
echo ""
echo "Selected txin: $txinupdated3"

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txinupdated3" \
  --tx-in-collateral "$txincollateral" \
  --tx-out "$scriptpaymentaddrwithstakecred+999978" \
  --witness-override 3 \
  --certificate-file "$WORK/script.delegcert" \
  --certificate-tx-in-reference "$plutusreferencescripttxin" \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file "scripts/plutus/data/42.redeemer" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/script-delegation-cert.txbody"

cardano-cli transaction sign \
  --tx-body-file "$WORK/script-delegation-cert.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY1" \
  --out-file "$WORK/script-delegation-cert.tx"

echo "Submitting staking script delegation certificate..."

cardano-cli transaction submit \
  --tx-file "$WORK/script-delegation-cert.tx" \
  --testnet-magic "$TESTNET_MAGIC"

echo ""
echo "!!!!!!!!!!!!"
echo "You need to wait 2 epochs after the current epoch for rewards"
cardano-cli query tip --testnet-magic 42
echo "!!!!!!!!!!!!"
echo ""
echo "Waiting 10 seconds..."
sleep 10
echo "Check to see if staking script was successfully delegated..."


cardano-cli query stake-address-info \
  --address "$stakingscriptaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/scriptdelegation.json"

delegatedscript=$(jq -r '.[0]' "$WORK/scriptdelegation.json")
echo "$delegatedscript"
echo ""
echo "Staking script payment address"
echo "$scriptpaymentaddrwithstakecred"
echo ""
echo "If a staking script address is displayed after this message, the registration of the Plutus staking script was successful"
echo "$stakingscriptaddr"
echo ""
echo "Use the following command to check when rewards have been paid to the staking script address"
echo ""
echo "cardano-cli query stake-address-info --testnet-magic 42 --address $stakingscriptaddr"
