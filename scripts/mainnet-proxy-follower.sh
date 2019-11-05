#!/usr/bin/env bash

set -e

RUNNER=${RUNNER:-cabal new-run --}
TOPOLOGY=${TOPOLOGY:-"configuration/topology-proxy-follower.json"}

ARGS=(
        --database-path           "./db/"
        --genesis-file            "configuration/mainnet-genesis.json"
        --topology                "${TOPOLOGY}"
        --socket-dir              "./socket/"
        --config                  "./configuration/mainnet-proxy-follower.yaml"
)

${RUNNER} exe:cardano-node "${ARGS[@]}"
