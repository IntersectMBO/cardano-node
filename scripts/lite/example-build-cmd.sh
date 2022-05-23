#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

export WORK="${WORK:-example/work}"
export BASE="${BASE:-.}"
export CARDANO_CLI="${CARDANO_CLI:-cardano-cli}"
export CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-example/node-bft1/node.sock}"
export TESTNET_MAGIC="${TESTNET_MAGIC:-42}"
export UTXO_VKEY="${UTXO_VKEY:-example/stake-delegator-keys/payment1.vkey}"
export UTXO_SKEY="${UTXO_SKEY:-example/stake-delegator-keys/payment1.skey}"
export RESULT_FILE_TARGET="${RESULT_FILE:-$WORK/target.out}"
export RESULT_FILE_CHANGE="${RESULT_FILE:-$WORK/change.out}"
# cardano-cli address build --payment-verification-key-file example/stake-delegator-keys/payment1.vkey --testnet-magic 42
echo "Socket path: $CARDANO_NODE_SOCKET_PATH"
echo "Socket path: $(pwd)"

ls -al "$CARDANO_NODE_SOCKET_PATH"


mkdir -p "$WORK"

utxoaddr=$($CARDANO_CLI address build --testnet-magic "$TESTNET_MAGIC" --payment-verification-key-file "$UTXO_VKEY" --stake-verification-key-file example/stake-delegator-keys/staking1.vkey)

$CARDANO_CLI query utxo --address "$utxoaddr" --cardano-mode --testnet-magic "$TESTNET_MAGIC" --out-file $WORK/utxo-1.json
cat $WORK/utxo-1.json
echo "here 1"
txin=$(jq -r 'keys[]' $WORK/utxo-1.json)
echo "$txin"
lovelaceattxin=$(jq -r ".[\"$txin\"].value.lovelace" $WORK/utxo-1.json)
echo "$lovelaceattxin"
lovelaceattxindiv2=$(expr $lovelaceattxin / 2)
echo "here 2"
changeaddr=addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh
targetaddr=addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4
echo "here 3"
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$changeaddr" \
  --tx-in $txin \
  --tx-out "$targetaddr+10000000" \
  --out-file $WORK/build.body

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/build.body \
  --testnet-magic "$TESTNET_MAGIC" \
  --signing-key-file $UTXO_SKEY \
  --out-file $WORK/build.tx

# SUBMIT
$CARDANO_CLI transaction submit --tx-file $WORK/build.tx --testnet-magic "$TESTNET_MAGIC"
echo "Pausing for 5 seconds..."
sleep 5

echo "Querying UTxO at change address and target address."
echo ""
echo "Target address"
echo ""
$CARDANO_CLI query utxo --address "$targetaddr"  --testnet-magic "$TESTNET_MAGIC" \
  | tee "$RESULT_FILE_TARGET"
echo "Change address"
echo ""
$CARDANO_CLI query utxo --address "$changeaddr"  --testnet-magic "$TESTNET_MAGIC" \
  | tee "$RESULT_FILE_CHANGE"
