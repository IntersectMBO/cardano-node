#!/usr/bin/env bash

set -xe

RUNNER=${RUNNER:-cabal v2-run --}
TOPOLOGY=${TOPOLOGY:-"configuration/mainnet-topology.json"}

ARGS=(
        --database-path           "./db-0/"
        --genesis-file            "configuration/mainnet-genesis.json"
        --genesis-hash            "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
        --topology                "${TOPOLOGY}"
        --socket-dir              "./socket/"
        --config                  "./configuration/configuration-mainnet.yaml"
        --port                    7776
)

${RUNNER} exe:cardano-node "${ARGS[@]}" $*
