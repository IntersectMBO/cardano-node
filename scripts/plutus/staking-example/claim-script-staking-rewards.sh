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
txinlovelace=$(jq -r ".[\"$txin\"].value.lovelace" $WORK/utxo-1.json)
txincollateral=$(jq -r 'keys[1]' $WORK/utxo-1.json)
scriptpaymentaddrwithstakecred=$(cardano-cli address build --payment-verification-key-file $UTXO_VKEY1  --stake-script-file "scripts/plutus/scripts/guess-42-stake.plutus" --testnet-magic 42)
stakingscriptaddr=$(cardano-cli stake-address build --stake-script-file scripts/plutus/scripts/guess-42-stake.plutus --testnet-magic 42)

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

cardano-cli transaction build \
  --alonzo-era \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$txin" \
  --tx-in-collateral "$txincollateral" \
  --tx-out "$scriptpaymentaddrwithstakecred+$totalspendable" \
  --withdrawal "$stakingscriptaddr+$rewardamt" \
  --withdrawal-script-file "scripts/plutus/scripts/guess-42-stake.plutus" \
  --withdrawal-redeemer-file "scripts/plutus/data/42.redeemer" \
  --protocol-params-file "$WORK/pparams.json" \
  --out-file "$WORK/script-withdrawal.txbody"

cardano-cli transaction sign \
  --tx-body-file "$WORK/script-withdrawal.txbody" \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file "$UTXO_SKEY1" \
  --out-file "$WORK/script-withdrawal.tx"

echo "Submitting withdrawal..."

cardano-cli transaction submit \
  --tx-file "$WORK/script-withdrawal.tx" \
  --testnet-magic "$TESTNET_MAGIC"

echo "Waiting 5 seconds...."
sleep 5

cardano-cli query stake-address-info \
  --address "$stakingscriptaddr" \
  --testnet-magic 42 \
  --out-file "$WORK/scriptrewardscheck.json"

scriptrewardscheck=$(jq -r '.[0]' $WORK/scriptrewardscheck.json)
echo "Checking if script rewards withdrawal was successful..."
echo "$scriptrewardscheck"
