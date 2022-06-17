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
export UTXO_STAKING_VKEY2="${UTXO_STAKING_VKEY2:=example/utxo-keys/utxo2-stake.vkey}"
export PV=v1 # Plutus Script Version

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
scriptpaymentaddrwithstakecred=$(cardano-cli address build --payment-verification-key-file "$UTXO_VKEY1"  --stake-script-file "scripts/plutus/scripts/$PV/guess-42-stake.plutus" --testnet-magic 42)

# Node Pool 1
poolcoldkey="example/pools/cold1.vkey"

# Delegate Plutus staking script address to stake pool

# Update UTxO again

cardano-cli query utxo \
  --address "$utxoaddr" \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/utxo-2.json"

cat "$WORK/utxo-2.json"

txinupdated2=$(jq -r 'keys[0]' "$WORK/utxo-2.json")
echo ""
echo "Selected txin: $txinupdated2"
# Step 1: Create registration certificate for the staking script

# We also create collateral.

txin=$(jq -r 'keys[]' "$WORK/utxo-2.json")
lovelaceattxin=$(jq -r ".[\"$txin\"].value.lovelace" "$WORK/utxo-2.json")
lovelaceattxindiv3=$((lovelaceattxin / 3))

cardano-cli stake-address registration-certificate \
  --stake-script-file "scripts/plutus/scripts/$PV/guess-42-stake.plutus" \
  --out-file "$WORK/script.regcert"

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$scriptpaymentaddrwithstakecred+999978" \
  --tx-out "$utxoaddr+$lovelaceattxindiv3" \
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

stakingscriptaddr=$(cardano-cli stake-address build --stake-script-file scripts/plutus/scripts/$PV/guess-42-stake.plutus --testnet-magic 42)

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

cardano-cli stake-address delegation-certificate \
  --stake-script-file "scripts/plutus/scripts/$PV/guess-42-stake.plutus" \
  --cold-verification-key-file "$poolcoldkey" \
  --out-file "$WORK/script.delegcert"

cardano-cli query protocol-parameters --testnet-magic "$TESTNET_MAGIC" --out-file "$WORK/pparams.json"

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
echo "Selected txin: $txinupdated2"

cardano-cli transaction build \
  --babbage-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txinupdated3" \
  --tx-in-collateral "$txincollateral" \
  --tx-out "$scriptpaymentaddrwithstakecred+999978" \
  --witness-override 3 \
  --certificate-file "$WORK/script.delegcert" \
  --certificate-script-file "scripts/plutus/scripts/$PV/guess-42-stake.plutus" \
  --certificate-redeemer-file "scripts/plutus/data/42.redeemer" \
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
echoi ""
echo "If a staking script address is displayed after this message, the registration of the Plutus staking script was successful"
echo "$stakingscriptaddr"
echo ""
echo "Use the following command to check when rewards have been paid to the staking script address"
echo ""
echo "cardano-cli query stake-address-info --testnet-magic 42 --address $stakingscriptaddr"
