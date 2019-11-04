#!/usr/bin/env bash

set -e

RUNNER=${RUNNER:-cabal new-run --}
TOPOLOGY=${TOPOLOGY:-"configuration/topology-proxy-follower.json"}

ARGS=(
        --log-config              "configuration/log-configuration.yaml"
        --genesis-file            "configuration/mainnet-genesis.json"
        --genesis-hash            "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
        --node-id                 "0"
        --topology                "${TOPOLOGY}"
        --port                    "7776"
        --real-pbft
        --config-yaml             "configuration/log-configuration.yaml"
)

${RUNNER} exe:cardano-node "${ARGS[@]}"
