#!/usr/bin/env bash

set -e

. $(dirname $0)/lib.sh
NODE="$(executable_runner cardano-node)"

TOPOLOGY=${TOPOLOGY:-"${configuration}/mainnet-topology.json"}

ARGS=(  run
        --database-path           "${root}/db/"
        --genesis-file            "${configuration}/mainnet-genesis.json"
        --genesis-hash            "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
        --topology                "${TOPOLOGY}"
        --socket-dir              "${root}/socket/"
        --config                  "${configuration}/configuration-mainnet.yaml"
        --port                    7776
)

${NODE} "${ARGS[@]}"
