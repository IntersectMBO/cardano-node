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
export PV=v1 # Plutus Script Version

utxoaddr=$(cardano-cli address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY1")


cardano-cli query utxo \
  --address "$utxoaddr" \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/utxo-1.json"

echo "UTxO"
cat "$WORK/utxo-1.json"
echo ""

txin=$(jq -r 'keys[0]' $WORK/utxo-1.json)
txin1=$(jq -r 'keys[1]' $WORK/utxo-1.json)
txinlovelace=$(jq -r ".[\"$txin\"].value.lovelace" $WORK/utxo-1.json)
txincollateral=$(jq -r 'keys[1]' $WORK/utxo-1.json)
withdrawingscript="scripts/plutus/scripts/v2/stake-script.plutus"
scriptpaymentaddrwithstakecred=$(cardano-cli address build --payment-verification-key-file $UTXO_VKEY1  --stake-script-file "$withdrawingscript" --testnet-magic 42)
stakingscriptaddr=$(cardano-cli stake-address build --stake-script-file $withdrawingscript --testnet-magic 42)

# STEP 1 - Get reward account balance

cardano-cli query stake-address-info \
  --address "$stakingscriptaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/scriptdelegationstatusrewards.json"

rewardamt=$(jq -r '.[0].rewardAccountBalance' $WORK/scriptdelegationstatusrewards.json)

totalspendable=$(expr $rewardamt + $txinlovelace - 289563)
echo "Lovelace at utxo: $txinlovelace"
echo "Rewards: $rewardamt"
echo "Combined: $totalspendable"

# Get plutus script reference input

plutusreferencescripttxin=$(jq -r 'keys[0]' $WORK/dummy-address-ref-script.json)


cardano-cli transaction build \
  --babbage-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-in "$txin1" \
  --tx-in-collateral "$txincollateral" \
  --tx-out "$scriptpaymentaddrwithstakecred+$totalspendable" \
  --withdrawal "$stakingscriptaddr+$rewardamt" \
  --withdrawal-tx-in-reference "$plutusreferencescripttxin" \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file "scripts/plutus/data/42.redeemer" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/script-withdrawal.txbody"

cardano-cli transaction sign \
  --tx-body-file "$WORK/script-withdrawal.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY1" \
  --out-file "$WORK/script-withdrawal.tx"

echo "Submitting withdrawal..."



echo ""
echo "Reward balance before withdrawal"

cardano-cli query stake-address-info \
  --address "$stakingscriptaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/scriptrewardscheck-before.json"
scriptrewardscheckbefore=$(jq -r '.[0]' $WORK/scriptrewardscheck-before.json)

echo "$scriptrewardscheckbefore"

cardano-cli transaction submit \
  --tx-file "$WORK/script-withdrawal.tx" \
  --testnet-magic "$TESTNET_MAGIC"

echo ""
echo "Waiting 5 seconds...."
sleep 5
echo ""
cardano-cli query stake-address-info \
  --address "$stakingscriptaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/scriptrewardscheck.json"

scriptrewardscheck=$(jq -r '.[0]' $WORK/scriptrewardscheck.json)
echo "Checking if script rewards withdrawal was successful...balance should be 0"
echo "$scriptrewardscheck"
