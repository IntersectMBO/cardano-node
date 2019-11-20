#!/usr/bin/env bash

# CMD="stack exec trace-acceptor-node -- "
# CMD="./trace-acceptor.exe -- "
CMD="cabal new-run exe:trace-acceptor -- "
#TODO: Confirm if db path is necessary for trace acceptor
set -x
${CMD} \
    --topology "./configuration/simple-topology-0.json" \
    --database-path "./db/" \
    --genesis-file "configuration/mainnet-genesis.json" \
    --socket-dir "./socket" \
    --config "configuration/log-config-acceptor.yaml" \
    --port 1234 \
    $@
