#!/usr/bin/env bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail


export BASE="${BASE:-.}"
export WORK="${WORK:-example/work}"
export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export UTXO_VKEY1="${UTXO_VKEY1:-example/shelley/utxo-keys/utxo1.vkey}"
export UTXO_SKEY1="${UTXO_SKEY1:-example/shelley/utxo-keys/utxo1.skey}"
export UTXO_VKEY2="${UTXO_VKEY1:-example/shelley/utxo-keys/utxo2.vkey}"
export UTXO_SKEY2="${UTXO_SKEY1:-example/shelley/utxo-keys/utxo2.skey}"
export UTXO_STAKING_VKEY1="${UTXO_STAKING_VKEY1:=example/shelley/utxo-keys/utxo-stake.vkey}"
export UTXO_STAKING_SKEY1="${UTXO_STAKING_SKEY1:=example/shelley/utxo-keys/utxo-stake.skey}"
export UTXO_STAKING_VKEY2="${UTXO_STAKING_VKEY2:=example/shelley/utxo-keys/utxo2-stake.vkey}"
export UTXO_STAKING_SKEY2="${UTXO_STAKING_SKEY2:=example/shelley/utxo-keys/utxo2-stake.skey}"

mkdir -p "$WORK"

utxoaddr=$(cardano-cli address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY1")
nodepool1dir=example/node-pool1
utxoaddrwithstaking=$(cardano-cli address build --payment-verification-key-file "$UTXO_VKEY2" --stake-verification-key-file "$UTXO_STAKING_VKEY2" --testnet-magic 42)
keystakeaddress=$(cardano-cli stake-address build --stake-verification-key-file "$UTXO_STAKING_VKEY2" --testnet-magic 42)


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
scriptpaymentaddrwithstakecred=$(cardano-cli address build --payment-verification-key-file "$UTXO_VKEY1"  --stake-script-file "scripts/plutus/scripts/guess-42-stake.plutus" --testnet-magic 42)
poolownerstakekey="example/addresses/pool-owner1-stake.vkey"
poolowneraddresswstakecred=$(cardano-cli address build --payment-verification-key-file  example/addresses/pool-owner1.vkey --stake-verification-key-file example/addresses/pool-owner1-stake.vkey --testnet-magic 42)
poolcoldkey="example/node-pool1/shelley/operator.vkey"

#Register stake pool

# We need to submit the stake pool registration certificate and
# also submit the delegation certificate of the pledger

# STEP 1
# Create registration certificate of pledger AND FUND THE POOL OWNER'S ADDRESS

cardano-cli stake-address registration-certificate \
  --stake-verification-key-file "$poolownerstakekey" \
  --out-file "$WORK/pledger.regcert"

cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-out "$scriptpaymentaddrwithstakecred+5000000" \
  --tx-out "$poolowneraddresswstakecred+5000000" \
  --tx-out "$utxoaddrwithstaking+5000000" \
  --witness-override 3 \
  --certificate-file "$WORK/pledger.regcert" \
  --out-file "$WORK/pledge-registration-cert.txbody"

cardano-cli transaction sign \
  --tx-body-file "$WORK/pledge-registration-cert.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY1" \
  --out-file "$WORK/pledge-registration-cert.tx"

echo "Submitting pool owner/pledge stake registration cert and funding stake pool owner address..."

cardano-cli transaction submit \
  --tx-file "$WORK/pledge-registration-cert.tx" \
  --testnet-magic "$TESTNET_MAGIC"

poolownerstakeaddr=$(cardano-cli stake-address build --stake-verification-key-file $poolownerstakekey --testnet-magic 42)

echo ""
echo "Pool owner/pledger stake address"
echo "$poolownerstakeaddr"
echo "Waiting 10 seconds..."

sleep 10

# Check the stake address was registered
cardano-cli query stake-address-info \
  --address "$poolownerstakeaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/pledgeownerregistration.json"
registered=$(jq -r '.[0]' "$WORK/pledgeownerregistration.json")

echo ""
echo "Registered pool owner/pledger address. If null it was not successfully registered"
echo "$registered"
sleep 2

#Register key staking address
echo ""
echo "Register key staking address"
echo "$poolownerstakeaddr"
echo ""

cardano-cli query utxo \
  --address "$utxoaddrwithstaking" \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/staking-key-utxo-1.json"

echo "Staking key UTxO"
cat "$WORK/staking-key-utxo-1.json"
echo ""

keytxin=$(jq -r 'keys[]' "$WORK/staking-key-utxo-1.json")

cardano-cli stake-address registration-certificate \
  --stake-verification-key-file "$UTXO_STAKING_VKEY2" \
  --out-file "$WORK/stakekey.regcert"

cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddrwithstaking" \
  --tx-in "$keytxin" \
  --tx-out "$utxoaddrwithstaking+999978" \
  --witness-override 3 \
  --certificate-file "$WORK/stakekey.regcert" \
  --out-file "$WORK/key-registration-cert.txbody"

cardano-cli transaction sign \
  --tx-body-file "$WORK/key-registration-cert.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY2" \
  --signing-key-file "$UTXO_STAKING_SKEY2" \
  --out-file "$WORK/key-registration-cert.tx"

echo "Submitting key stake registration cert..."

cardano-cli transaction submit \
  --tx-file "$WORK/key-registration-cert.tx" \
  --testnet-magic "$TESTNET_MAGIC"
echo "Wait 10 seconds..."
sleep 10

echo "Check to see if it was registered..."
cardano-cli query stake-address-info \
  --address "$keystakeaddress" \
  --testnet-magic 42 \
  --out-file "$WORK/keyregistration.json"

registeredkey=$(jq -r '.[0]' "$WORK/keyregistration.json")
echo ""
echo "Registered key staking address. If null it was not successfully registered"
echo "$registeredkey"


# Update UTxO

cardano-cli query utxo \
  --address "$utxoaddr" \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/utxo-1.json"

cat "$WORK/utxo-1.json"

txinupdated=$(jq -r 'keys[0]' "$WORK/utxo-1.json")


# STEP 2
# Create delegation certificate of pledger

cardano-cli stake-address delegation-certificate \
  --stake-verification-key-file "$poolownerstakekey" \
  --cold-verification-key-file "$poolcoldkey" \
  --out-file "$WORK/pledger.delegcert"

# TODO: We use witness override because the build command does not take certificates
# into account and underestimates the tx fee.

# STEP 3
# REGISTER STAKE POOL AND DELEGATE THE PLEDGER TO THE STAKE POOL IN ONE TX

cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txinupdated" \
  --tx-out "$scriptpaymentaddrwithstakecred+999978" \
  --witness-override 3 \
  --certificate-file "example/node-pool1/registration.cert" \
  --certificate-file "$WORK/pledger.delegcert" \
  --out-file "$WORK/register-stake-pool.txbody"

# UTxO Payment key
# Staking key
# Cold key
cardano-cli transaction sign \
  --tx-body-file "$WORK/register-stake-pool.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY1" \
  --signing-key-file "$nodepool1dir/shelley/operator.skey" \
  --signing-key-file "$nodepool1dir/owner.skey" \
  --out-file "$WORK/register-stake-pool.tx"

echo "Registering stake pool and delegating to said stake pool"

cardano-cli transaction submit \
  --tx-file "$WORK/register-stake-pool.tx" \
  --testnet-magic "$TESTNET_MAGIC"

echo "Wait 5 seconds for UTxO to update..."
sleep 5
currentstakepools=$(cardano-cli query stake-pools --testnet-magic 42)

# Check the stake address was delegated
cardano-cli query stake-address-info \
  --address "$poolownerstakeaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/pledgeownerregistration.json"
delegated=$(jq -r '.[0]' "$WORK/pledgeownerregistration.json")

echo ""
echo "Currently registered stake pools"
echo "$currentstakepools"
echo ""
echo "We check if the pool owner/pledger has successfully delegated"
echo "$delegated"


echo ""
echo "Delegate staking key to stake pool"
echo ""

cardano-cli stake-address delegation-certificate \
  --stake-verification-key-file "$UTXO_STAKING_VKEY2" \
  --cold-verification-key-file "$poolcoldkey" \
  --out-file "$WORK/stakekey.delegcert"

cardano-cli query utxo \
  --address "$utxoaddrwithstaking" \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/staking-key-utxo-2.json"

echo "Staking key UTxO"
cat "$WORK/staking-key-utxo-2.json"
echo ""

keytxin2=$(jq -r 'keys[0]' "$WORK/staking-key-utxo-2.json")

cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddrwithstaking" \
  --tx-in "$keytxin2" \
  --tx-out "$utxoaddrwithstaking+999978" \
  --witness-override 3 \
  --certificate-file "$WORK/stakekey.delegcert" \
  --out-file "$WORK/key-deleg-cert.txbody"

cardano-cli transaction sign \
  --tx-body-file "$WORK/key-deleg-cert.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY2" \
  --signing-key-file "$UTXO_STAKING_SKEY2" \
  --out-file "$WORK/key-deleg-cert.tx"

echo "Submitting key stake delegation cert"

cardano-cli transaction submit \
  --tx-file "$WORK/key-deleg-cert.tx" \
  --testnet-magic "$TESTNET_MAGIC"

echo "Wait 10 seconds..."
sleep 10

echo "Check to see if it was delegated..."
cardano-cli query stake-address-info \
  --address "$keystakeaddress" \
  --testnet-magic 42 \
  --out-file "$WORK/keydelegation.json"

delegatedkey=$(jq -r '.[0]' "$WORK/keydelegation.json")
echo ""
echo "Delegating key staking address. If null it was not successfully registered"
echo "$delegatedkey"



# UP TO HERE IN THEORY WE ARE FINE. THE POOL GETS REGISTERED AND THE PLEDGE IS DELEGATED TO THE POOL
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
  --stake-script-file "scripts/plutus/scripts/guess-42-stake.plutus" \
  --out-file "$WORK/script.regcert"

cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txinupdated2" \
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

stakingscriptaddr=$(cardano-cli stake-address build --stake-script-file scripts/plutus/scripts/guess-42-stake.plutus --testnet-magic 42)

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
  --stake-script-file "scripts/plutus/scripts/guess-42-stake.plutus" \
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
  --alonzo-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txinupdated3" \
  --tx-in-collateral "$txincollateral" \
  --tx-out "$scriptpaymentaddrwithstakecred+999978" \
  --witness-override 3 \
  --certificate-file "$WORK/script.delegcert" \
  --certificate-script-file "scripts/plutus/scripts/guess-42-stake.plutus" \
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
echo "Stake payment address"
echo "$scriptpaymentaddrwithstakecred"
