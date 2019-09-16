#!/usr/bin/env bash

NOW=`date "+%Y-%m-%d 00:00:00"`
GENHASH="33873aeaf8a47fefc7c2ea3f72e98a04459e07ec3edfb63c9ca709f540f69503"

# CMD="stack exec trace-acceptor-node -- "
# CMD="./trace-acceptor.exe -- "
CMD="cabal new-run exe:trace-acceptor -- "

NETARGS=(
        --slot-duration 2
        --genesis-file "configuration/${GENHASH:0:5}/genesis.json"
        --genesis-hash "${GENHASH}"
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
