#!/usr/bin/env bash

set -e

. $(dirname $0)/lib.sh
NODE="$(executable_runner cardano-node)"

TOPOLOGY=${TOPOLOGY:-"${configuration}/defaults/mainnet/topology.json"}

ARGS=(  run
        --database-path           "${root}/db/"
        --topology                "${TOPOLOGY}"
        --socket-path              "${root}/socket/mainnet-socket"
        --config                  "${configuration}/defaults/mainnet/configuration.yaml"
        --port                    7776
       # --use-activated-sockets

)

${NODE} "${ARGS[@]}"
