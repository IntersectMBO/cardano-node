#!/usr/bin/env bash

set -e

. $(dirname $0)/lib-node.sh
CHAIRMAN="$(executable_runner chairman)"

set -x
${CHAIRMAN} \
        --core-node-id 0 --core-node-id 1 --core-node-id 2 \
        -k 10 -s 250 \
        -t 1000 \
        --genesis-file "${genesis_file}" \
        --genesis-hash "${genesis_hash}" \
        --socket-dir "./socket/" \
        --config "${configuration}/log-config-0.yaml"
