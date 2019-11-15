#!/usr/bin/env bash

BASEDIR=`pwd`

mkdir -p "state-node-mainnet"
cd "state-node-mainnet"

CMD="cabal new-run exe:trace-acceptor -- "

set -x
${CMD} \
    --config ${BASEDIR}/launch_mainnet.d/log-config-acceptor.yaml \
    --topology "./configuration/simple-topology.json" \
    --database-path "./db/" \
    --genesis-file "configuration/mainnet-genesis.json" \
    --socket-dir "./socket" \
    --port 1234 \
           $@
