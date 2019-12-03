#!/usr/bin/env bash

set -e

RUNNER=${RUNNER:-cabal new-run --}
TOPOLOGY=${TOPOLOGY:-"configuration/mainnet-topology.json"}

ARGS=(
        --database-path           "./db/"
        --genesis-file            "configuration/mainnet-genesis.json"
        --topology                "${TOPOLOGY}"
        --socket-dir              "./socket/"
        --config                  "./configuration/mainnet.yaml"
        --port                    7776
)

${RUNNER} exe:cardano-node "${ARGS[@]}"
