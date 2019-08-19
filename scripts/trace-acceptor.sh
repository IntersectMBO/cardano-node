#!/usr/bin/env bash

NOW=`date "+%Y-%m-%d 00:00:00"`

# CMD="stack exec cardano-node -- "
# CMD="./cardano-node.exe -- "
CMD="cabal new-exec cardano-node -- "

NETARGS=(
        --slot-duration 2
        --genesis-file "configuration/Test.Cardano.Chain.Genesis.Dummy.dummyConfig.configGenesisData.json"
        --genesis-hash "fc32ebdf3c9bfa2ebf6bdcac98649f610601ddb266a2a2743e787dc9952a1aeb"
        trace-acceptor
)

function mkdlgkey () {
  printf -- "--signing-key configuration/delegate-keys.%03d.key" "$1"
}
function mkdlgcert () {
  printf -- "--delegation-certificate configuration/delegation-cert.%03d.json" "$1"
}

set -x
${CMD} \
    --log-config configuration/log-config-acceptor.yaml \
    $(mkdlgkey 0) \
    $(mkdlgcert 0) \
    ${NETARGS[*]} \
           $@
