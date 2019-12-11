#!/usr/bin/env bash

. $(dirname $0)/lib-node.sh

# CMD="stack exec trace-acceptor-node -- "
# CMD="./trace-acceptor.exe -- "
CMD="cabal v2-run exe:trace-acceptor -- "
#TODO: Confirm if db path is necessary for trace acceptor
set -x
${CMD} \
    --topology "./configuration/simple-topology.json" \
    --database-path "./db/" \
    --genesis-file "configuration/mainnet-genesis.json" \
    --genesis-hash "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb" \
    --socket-dir "./socket" \
    --config "configuration/log-config-acceptor.yaml" \
    --port 1234 \
    $@
