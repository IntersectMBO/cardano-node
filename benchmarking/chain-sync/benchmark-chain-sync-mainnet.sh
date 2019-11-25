#!/usr/bin/env bash

# >> cpu time limit in seconds
CPU_TIME_LIMIT=3600

BASEDIR=`pwd`

DATADIR=state-node-mainnet
mkdir -p $DATADIR
cd $DATADIR

if [ -d db-mainnet-0 ]; then
  rm -rf db-mainnet-0
fi
rm node-0*

#set -euo pipefail

ulimit -t $CPU_TIME_LIMIT

date --iso-8601=seconds > STARTTIME

NODE="cabal new-run exe:cardano-node -- "

exec ${NODE} \
  --genesis-file ${BASEDIR}/../../configuration/mainnet-genesis.json \
  --config ${BASEDIR}/configuration/log-configuration.yaml \
  --database-path .//db-mainnet \
  --socket-dir /tmp/socket-bm-chain-sync \
  --topology ${BASEDIR}/configuration/topology-local.yaml \
  --host-addr 127.0.0.1 \
  --port 7777 \
  --tracing-verbosity-maximal \
  --trace-mempool \
  --trace-forge \
   \
 $@

#  --socket-dir ${BASEDIR}/${DATADIR}/socket \
# this will render the events in textual format
#  --trace-chain-db \

#  --trace-block-fetch-client \
#  --trace-chain-sync-protocol \
#  --trace-block-fetch-protocol \
#  --trace-block-fetch-decisions \
#  --trace-block-fetch-server \
#  --trace-chain-sync-header-server \
#  --trace-tx-inbound \
#  --trace-tx-outbound \
#  --trace-local-tx-submission-server \
#  --trace-local-chain-sync-protocol \
#  --trace-tx-submission-protocol \
#  --trace-local-tx-submission-protocol \
#  --trace-ip-subscription \
#  --trace-dns-subscription \
#  --trace-dns-resolver \

../analyse-logs.sh
