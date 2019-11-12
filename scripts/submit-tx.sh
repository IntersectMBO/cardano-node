#!/usr/bin/env bash

test -z "$1" -o ! -f "$1" -o ! -r "$1" && {
        cat >&1 <<EOF
Usage:  $(basename $0) TX-FILE
EOF
        exit 1
}
TX="$1"
shift

#CMD="stack exec --nix cardano-node -- "
CMD="cabal new-run"

. $(dirname $0)/lib-node.sh

ALGO="real-pbft"
NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS=(
        --${ALGO}
        --genesis-file "${genesis_file}"
        --genesis-hash "${genesis_hash}"
        submit-tx
        --topology      "configuration/simple-topology.json"
        --node-id      "0"
        --tx           "$TX"
)

function mkdlgkey () {
  printf -- "--signing-key            ${genesis_root}/delegate-keys.%03d.key"    "$1"
}
function mkdlgcert () {
  printf -- "--delegation-certificate ${genesis_root}/delegation-cert.%03d.json" "$1"
}

set -x
${CMD} -- cardano-cli --log-config configuration/log-configuration.yaml ${NETARGS[*]} "$@"
