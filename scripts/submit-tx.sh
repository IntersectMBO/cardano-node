#!/usr/bin/env bash

test -z "$1" -o ! -f "$1" -o ! -r "$1" && {
        cat >&1 <<EOF
Usage:  $(basename $0) TX-FILE
EOF
        exit 1
}
TX="$1"
shift

. $(dirname $0)/lib-node.sh
CLI="$(executable_runner cardano-cli)"

ALGO="real-pbft"
NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS=(
        submit-tx
        --tx           "$TX"
        --${ALGO}
        --genesis-file "${genesis_file}"
        --genesis-hash "${genesis_hash}"
        --socket-dir   "./socket/"
        --topology     "${configuration}/simple-topology.json"
)


set -x
${CLI} ${NETARGS[*]} "$@"
