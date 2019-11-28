#!/usr/bin/env bash

[ $# -ne 1 ] && echo "Usage: $(basename $0) TargetSocketFilePath" 1>&2 && exit 1

SOCKET=$1

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
        --socket-path "${1}" \
        --config "${configuration}/log-config-0.yaml"
