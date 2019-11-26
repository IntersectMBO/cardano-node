#!/bin/sh

BASEDIR=$(realpath $(dirname $0))

CONFIGFILE=${BASEDIR}/configuration/log-config-explorer.yaml

. ${BASEDIR}/configuration/psql-settings.sh

GENESISHASH=`cat ${BASEDIR}/configuration/latest-genesis/GENHASH`
GENESISJSON="${BASEDIR}/configuration/latest-genesis/genesis.json"

sed -i 's/^GenesisHash: .*$/GenesisHash: '${GENESISHASH}'/' ${CONFIGFILE}


cd cardano-explorer.git

EXPLORER="cabal new-run exe:cardano-explorer-node -- "
exec ${EXPLORER} \
  --genesis-file ${GENESISJSON} \
  --config ${CONFIGFILE} \
  --socket-path /tmp/cluster3nodes-socket/node-core-0.socket \
  --schema-dir ${BASEDIR}/cardano-explorer.git/schema \

