#!/usr/bin/env bash

now=`date "+%Y-%m-%d 00:00:00"`
#CMD="stack exec --nix cardano-node -- "
CMD="cabal new-exec cardano-node -- "

set -x
${CMD} \
    --system-start "$now" --slot-duration 2 \
    --log-config configuration/log-configuration.yaml \
    submit -t configuration/simple-topology.json \
           $@
