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

utxoaddr=$(cardano-cli address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY1")
scriptpaymentaddrwithstakecred=$(cardano-cli address build --payment-verification-key-file "$UTXO_VKEY1"  --stake-script-file "scripts/plutus/scripts/guess-42-stake.plutus" --testnet-magic 42)
stakingscriptaddr=$(cardano-cli stake-address build --stake-script-file scripts/plutus/scripts/guess-42-stake.plutus --testnet-magic 42)

# DEREGISTRATION

# Update UTxO again
echo ""
echo "Script staking address deregistration"
echo ""
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

# Create deregistration certificate
cardano-cli stake-address deregistration-certificate \
  --stake-script-file "scripts/plutus/scripts/guess-42-stake.plutus" \
  --out-file "$WORK/script.deregcert"



# Get PParams

cardano-cli query protocol-parameters --testnet-magic "$TESTNET_MAGIC" --out-file "$WORK/pparams.json"

cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txinupdated3" \
  --tx-in-collateral "$txincollateral" \
  --tx-out "$scriptpaymentaddrwithstakecred+500" \
  --witness-override 3 \
  --certificate-file "$WORK/script.deregcert" \
  --certificate-script-file "scripts/plutus/scripts/guess-42-stake.plutus" \
  --certificate-redeemer-file "scripts/plutus/data/42.redeemer" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/script-deregistration-cert.txbody"

cardano-cli transaction sign \
  --tx-body-file "$WORK/script-deregistration-cert.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY1" \
  --out-file "$WORK/script-deregistration-cert.tx"

cardano-cli transaction submit \
  --tx-file "$WORK/script-deregistration-cert.tx" \
  --testnet-magic "$TESTNET_MAGIC"

echo "Staking script adress"
echo "$stakingscriptaddr"
echo "Waiting 5 seconds..."
sleep 5
echo "Check to see if the script staking address was successfully deregistered"

cardano-cli query stake-address-info \
  --address "$stakingscriptaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/scriptderegistration.json"

deregistered=$(jq -r '.[0]' "$WORK/scriptderegistration.json")
echo "$deregistered"