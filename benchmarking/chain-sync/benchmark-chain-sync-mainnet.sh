#!/usr/bin/env bash

# >> cpu time limit in seconds
CPU_TIME_LIMIT=$((1*60*60))

BASEDIR=`pwd`

DATADIR=state-node-mainnet
mkdir -p $DATADIR
cd $DATADIR

# remove blockchain
if [ -d db-mainnet-0 ]; then
  rm -rf db-mainnet-0
fi

# remove old log files
rm node-0*

#set -euo pipefail

ulimit -t $CPU_TIME_LIMIT

date --iso-8601=seconds > STARTTIME

if [[ $1 == 'stack' ]]; then
  NODE="stack --nix exec cardano-node -- "
  shift
elif [[ $1 == 'cabal' ]]; then
  NODE="cabal v2-run exe:cardano-node -- "
  shift
else
  # Default to stack
  NODE="stack --nix exec cardano-node -- "
fi

exec ${NODE} \
  --genesis-file ${BASEDIR}/../../configuration/mainnet-genesis.json \
  --genesis-hash "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb" \
  --config ${BASEDIR}/configuration/log-configuration.yaml \
  --database-path .//db-mainnet \
  --socket-dir /tmp/socket-bm-chain-sync \
  --topology ${BASEDIR}/configuration/topology-local.yaml \
  --host-addr 127.0.0.1 \
  --port 7778 \
   \
  $@

#  --socket-dir ${BASEDIR}/${DATADIR}/socket \

../analyse-logs.sh
