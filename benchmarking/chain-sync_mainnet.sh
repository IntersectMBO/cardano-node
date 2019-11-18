#!/usr/bin/env bash
set -euo pipefail

BASEDIR=`pwd`

mkdir -p "state-node-mainnet"
cd "state-node-mainnet"

NODE="cabal new-run exe:cardano-node -- "

exec ${NODE} \
  --genesis-file ${BASEDIR}/../configuration/mainnet-genesis.json \
  --config ${BASEDIR}/launch_mainnet.d/log-configuration.yaml \
  --database-path .//db-mainnet \
  --socket-dir socket \
  --topology ${BASEDIR}/launch_mainnet.d/topology-local.yaml \
  --host-addr 127.0.0.1 \
  --port 7777 \
  --tracing-verbosity-maximal \
  --trace-mempool \
  --trace-forge \
   \
 $@
 &

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

NODEPID=$!

SLOTLIMIT="24000"

tail -F state-node-mainnet/node-0.log | grep --line-buffered -e '.*:cardano.node.ChainDB:.*TraceCopyToImmDBEvent.CopiedBlockToImmDB.*"tip":"[a-z0-9]*@'${SLOTLIMIT} | {

  read a
  echo "got: $a"
  echo "killing $NODEPID"
  kill $NODEPID
  
}
