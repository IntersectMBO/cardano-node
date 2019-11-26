#!/bin/sh

set -e

GITURL="https://github.com/input-output-hk/cardano-explorer.git"

rm -rf cardano-explorer.git
git clone -b master ${GITURL} cardano-explorer.git

cd cardano-explorer.git

cabal new-build all


