#!/usr/bin/env bash

NOW=`date "+%Y-%m-%d 00:00:00"`
GENHASH="968b0012ac326c048c7b7cc3f143085f820aad051e387156dbcb227d2a7f913f"

# CMD="stack exec cardano-node -- "
# CMD="./cardano-node.exe -- "
CMD="cabal new-exec cardano-node -- "

NETARGS=(
        --slot-duration 2
        --genesis-file "configuration/${GENHASH:0:5}/genesis.json"
        --genesis-hash "${GENHASH}"
        trace-acceptor
)

function mkdlgkey () {
  printf -- "--signing-key configuration/${GENHASH:0:5}/delegate-keys.%03d.key" "$1"
}
function mkdlgcert () {
  printf -- "--delegation-certificate configuration/${GENHASH:0:5}/delegation-cert.%03d.json" "$1"
}

set -x
${CMD} \
    --log-config configuration/log-config-acceptor.yaml \
    $(mkdlgkey 0) \
    $(mkdlgcert 0) \
    ${NETARGS[*]} \
           $@
