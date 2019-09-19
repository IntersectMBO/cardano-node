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

genesis="33873"
genesis_root="configuration/${genesis}"
genesis_file="${genesis_root}/genesis.json"
if test ! -f "${genesis_file}"
then echo "ERROR: genesis ${genesis_file} does not exist!">&1; exit 1; fi
genesis_hash="$(${CMD} -v0 -- cardano-cli --log-config configuration/log-configuration.yaml --real-pbft print-genesis-hash --genesis-json ${genesis_file} --socket-path socket/node1.socket)"

ALGO="real-pbft"
NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS=(
        --${ALGO}
        submit-tx
        --genesis-file "${genesis_file}"
        --genesis-hash "${genesis_hash}"
        --topology     "configuration/simple-topology.json"
        --node-id      "0"
        --tx           "$TX"
        --socket-path  socket/node1.socket
)

function mkdlgkey () {
  printf -- "--signing-key            ${genesis_root}/delegate-keys.%03d.key"    "$1"
}
function mkdlgcert () {
  printf -- "--delegation-certificate ${genesis_root}/delegation-cert.%03d.json" "$1"
}

set -x
${CMD} -- cardano-cli --log-config configuration/log-configuration.yaml ${NETARGS[*]} "$@" --socket-path socket/node1.socket
