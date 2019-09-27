#!/usr/bin/env bash

# CMD="stack exec trace-acceptor-node -- "
# CMD="./trace-acceptor.exe -- "
CMD="cabal new-run exe:trace-acceptor -- "

set -x
${CMD} \
    --log-config configuration/log-config-acceptor.yaml \
    $@
