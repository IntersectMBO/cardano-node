#!/usr/bin/env bash
#
# ./scripts/chairman.sh ./socket/0 ./socket/1 ./socket/2

# it does not make sense to run chairman just for a single node
[ $# -le 1 ] && echo "Usage: $(basename $0) TargetSocketFilePath" 1>&2 && exit 1

set -e

SOCKET_PATHS=${@/#/--socket-path }

. $(dirname $0)/lib-node.sh
CHAIRMAN="$(executable_runner chairman)"

set -x

${CHAIRMAN} \
        -k 10 -s 250 \
        -t 1000 \
        --config "${configuration}/log-config-0.yaml" \
        $SOCKET_PATHS
