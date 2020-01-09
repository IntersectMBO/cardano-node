#!/usr/bin/env bash

set -e

. $(dirname $0)/lib-mode.sh
NODE="$(executable_runner cardano-node)"

TOPOLOGY=${TOPOLOGY:-"configuration/mainnet-topology.json"}

ARGS=(
        --database-path           "./db/"
        --genesis-file            "configuration/mainnet-genesis.json"
        --genesis-hash            "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
        --topology                "${TOPOLOGY}"
        --socket-dir              "./socket/"
        --config                  "./configuration/configuration-mainnet.yaml"
        --port                    7776

        --trace-block-fetch-decisions
        --trace-block-fetch-client
        --trace-block-fetch-server
        --trace-tx-inbound
        --trace-tx-outbound
        --trace-local-tx-submission-server
        --trace-mempool
        --trace-forge
        --trace-chain-sync-protocol
        --trace-block-fetch-protocol
        --trace-tx-submission-protocol
        --trace-local-chain-sync-protocol
        --trace-local-tx-submission-protocol
)

${NODE} "${ARGS[@]}"
