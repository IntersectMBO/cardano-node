#!/bin/sh

set -e

BASEDIR=$(realpath $(dirname $0))
#GITURL="https://github.com/input-output-hk/cardano-byron-proxy.git"
TARGETDIR=cardano-byron-proxy.git

#rm -rf ${TARGETDIR}
#git clone -b master ${GITURL} ${TARGETDIR}
git submodule update --init --remote ${TARGETDIR}

cd ${TARGETDIR}

cabal v2-build all

cd ${BASEDIR}
