#!/usr/bin/env bash

set -e

. $(dirname $0)/lib.sh
NODE="$(executable_runner cardano-node)"

TOPOLOGY=${TOPOLOGY:-"${configuration}/mainnet-topology.json"}

ARGS=(  run
        --database-path           "${root}/db/"
        --topology                "${TOPOLOGY}"
        --socket-path              "${root}/socket/mainnet-socket"
        --config                  "${configuration}/configuration-mainnet.yaml"
        --port                    7776
)

${NODE} "${ARGS[@]}"
