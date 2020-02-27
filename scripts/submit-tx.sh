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

CONFIG="configuration/log-config-0.liveview.yaml"
NOW=`date "+%Y-%m-%d 00:00:00"`
SOCKET="socket/0"
NETARGS=(
        submit-tx
        --tx           "$TX"
        --config       "$CONFIG"
        --socket-path  "$SOCKET"
)


set -x
${CLI} ${NETARGS[*]} "$@"
