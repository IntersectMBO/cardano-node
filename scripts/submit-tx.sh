#!/usr/bin/env bash

#CMD="stack exec --nix cardano-node -- "
CMD="cabal new-exec cardano-node -- "

ALGO="--real-pbft"
NOW=`date "+%Y-%m-%d 00:00:00"`
NETARGS=(
        --slot-duration 2
        --genesis-file "configuration/Test.Cardano.Chain.Genesis.Dummy.dummyConfig.configGenesisData.json"
        --genesis-hash "fc32ebdf3c9bfa2ebf6bdcac98649f610601ddb266a2a2743e787dc9952a1aeb"
        submit
        --topology "configuration/simple-topology.json"
        ${ALGO}
)

function mkdlgkey () {
  printf -- "--signing-key configuration/delegate-keys.%03d.key" "$1"
}
function mkdlgcert () {
  printf -- "--delegation-certificate configuration/delegation-cert.%03d.json" "$1"
}

set -x
${CMD} \
    --log-config configuration/log-configuration.yaml \
    $(mkdlgkey 0) \
    $(mkdlgcert 0) \
    ${NETARGS[*]} \
           $@
