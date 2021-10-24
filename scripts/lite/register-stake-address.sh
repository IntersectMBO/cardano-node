#!/usr/bin/env bash

set -eo pipefail

export WORK="${WORK:-example/work}"
export BASE="${BASE:-.}"
export CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export UTXO_VKEY="${UTXO_VKEY:-example/shelley/utxo-keys/utxo1.vkey}"
export UTXO_SKEY="${UTXO_SKEY:-example/shelley/utxo-keys/utxo1.skey}"
export UTXO_STAKE_VKEY="${UTXO_STAKE_VKEY:-example/shelley/utxo-keys/utxo-stake.vkey}"
export UTXO_STAKE_SKEY="${UTXO_STAKE_SKEY:-example/shelley/utxo-keys/utxo-stake.skey}"

[ "${BASH_VERSINFO:-0}" -ge 4 ] || {
  echo "Need at least bash version 4"
  exit 1
}

mkdir -p "$WORK"

utxo_address="$(scripts/lite/address-of.sh $UTXO_VKEY)"

$CARDANO_CLI query protocol-parameters --testnet-magic "$TESTNET_MAGIC" > "$WORK/protocol-params.json"

$CARDANO_CLI stake-address registration-certificate \
  --stake-verification-key-file "$UTXO_STAKE_VKEY" \
  --out-file example/shelley/utxo-keys/utxo-stake.cert

current_slot=$($CARDANO_CLI query tip --testnet-magic "$TESTNET_MAGIC" | jq -r '.slot')

$CARDANO_CLI query utxo --address "$utxo_address" --testnet-magic "$TESTNET_MAGIC" --out-file "$WORK/utxo1.balance"

utxo_total="$(cat "$WORK/utxo1.balance" | jq -r 'to_entries | map(.value.value.lovelace) | add')"
utxo_count="$(cat "$WORK/utxo1.balance" | jq -r length)"
stake_address_deposit="$(cat "$WORK/protocol-params.json" | jq -r '.stakeAddressDeposit')"

read -a utxo_as_txins < <(cat "$WORK/utxo1.balance" | jq -r 'to_entries | map(["--tx-in", .key]) | flatten | @tsv')

echo "Current Slot: $current_slot"
echo "Total ADA Balance: $utxo_total"
echo "UTxO Count: $utxo_count"
echo "UTxO args: ${utxo_as_txins[@]}"
echo "Stake Address Deposit: $stake_address_deposit"

$CARDANO_CLI transaction build-raw \
  ${utxo_as_txins[@]} \
  --tx-out "$utxo_address+0" \
  --invalid-hereafter "$(( $current_slot + 10000 ))" \
  --fee 0 \
  --out-file "$WORK/tx.tmp" \
  --certificate "example/shelley/utxo-keys/utxo-stake.cert"

min_fee="$(cardano-cli transaction calculate-min-fee \
    --tx-body-file "$WORK/tx.tmp" \
    --tx-in-count "$utxo_count" \
    --tx-out-count 1 \
    --mainnet \
    --witness-count 2 \
    --byron-witness-count 0 \
    --protocol-params-file "$WORK/protocol-params.json" | awk '{ print $1 }')"

echo "Minimum fee: $min_fee"

tx_out="$((${utxo_total}-${stake_address_deposit}-${min_fee}))"

echo "Change Output: ${tx_out}"

$CARDANO_CLI transaction build-raw \
  ${utxo_as_txins[@]} \
  --tx-out "$utxo_address+$tx_out" \
  --invalid-hereafter "$(( ${currentSlot} + 10000))" \
  --fee "$min_fee" \
  --certificate-file "example/shelley/utxo-keys/utxo-stake.cert" \
  --out-file "$WORK/tx.raw"

$CARDANO_CLI transaction sign \
  --tx-body-file "$WORK/tx.raw" \
  --signing-key-file "$UTXO_SKEY" \
  --signing-key-file "$UTXO_STAKE_SKEY" \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$WORK/tx.signed"

$CARDANO_CLI transaction submit \
  --tx-file "$WORK/tx.signed" \
  --testnet-magic "$TESTNET_MAGIC"
