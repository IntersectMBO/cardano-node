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
CMD="cabal new-exec cardano-cli -- "

ALGO="real-pbft"
NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS=(
        ${ALGO}
        submit-tx
        --genesis-file "configuration/Test.Cardano.Chain.Genesis.Dummy.dummyConfig.configGenesisData.json"
        --genesis-hash "fc32ebdf3c9bfa2ebf6bdcac98649f610601ddb266a2a2743e787dc9952a1aeb"
        --topology     "configuration/simple-topology.json"
        --node-id      "0"
        --tx           "$TX"
)

function mkdlgkey () {
  printf -- "--signing-key configuration/delegate-keys.%03d.key" "$1"
}
function mkdlgcert () {
  printf -- "--delegation-certificate configuration/delegation-cert.%03d.json" "$1"
}

set -x
${CMD} \
    ${NETARGS[*]} \
           $@
