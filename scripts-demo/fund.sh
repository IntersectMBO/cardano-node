#!/usr/bin/env bash
# Example usage:
# ./scripts-demo/fund.sh addr_test1vq922scgdwrrfa3n2pzu3empkju9ekregg0tza0xnveya3gfl0ycn 1000000000

set -euo pipefail

trap 'rm -f fundint.txbody' EXIT
rm -f funding.txbody funding.tx

run_cardano_cli() {
  cabal run -v0 cardano-cli -- "$@"
}

export CARDANO_NODE_SOCKET_PATH=./testnet-data/socket/node1/sock
export CARDANO_NODE_NETWORK_ID=42
SRC_ADDR=$(cat testnet-data/utxo-keys/utxo1/utxo.addr)
SRC_UTXO=$(run_cardano-cli latest query utxo --address "$SRC_ADDR" | jq -r 'keys[0]')
run_cardano-cli latest transaction build --tx-in "$SRC_UTXO" --tx-out "$1+$2" --change-address "$SRC_ADDR" --out-file funding.txbody
run_cardano-cli latest transaction sign --tx-body-file funding.txbody --signing-key-file testnet-data/utxo-keys/utxo1/utxo.skey --out-file funding.tx
run_cardano-cli latest transaction submit --tx-file funding.tx
