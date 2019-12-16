#!/usr/bin/env bash

TARGETDIR=cardano-byron-proxy.git
if [ ! -d ${TARGETDIR} ]; then
  echo "cannot find byron-proxy. run './prepare-byron-proxy.sh' first."
  exit 1
fi

BASEDIR=$(realpath $(dirname $0))
set -euo pipefail

cd ${TARGETDIR}

cabal v2-run exe:cardano-byron-proxy -- \
  +RTS -T -RTS \
  --database-path state-proxy-mainnet/db \
  --index-path state-proxy-mainnet/index \
  --configuration-file ./configuration/configuration.yaml \
  --configuration-key mainnet_full \
  --topology ${BASEDIR}/configuration/topology-byron-proxy.yaml \
  --logger-config ${BASEDIR}/configuration/log-config-byron-proxy.yaml \
  --local-addr [127.0.0.1]:7777 \
   \
   \
   \
   \
   $@

cd ${BASEDIR}
