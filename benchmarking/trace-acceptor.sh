#!/usr/bin/env bash

BASEDIR=`pwd`

mkdir -p "state-node-mainnet"
cd "state-node-mainnet"

CMD="cabal new-run exe:trace-acceptor -- "

set -x
${CMD} \
    --log-config ${BASEDIR}/launch_mainnet.d/log-config-acceptor.yaml \
           $@
