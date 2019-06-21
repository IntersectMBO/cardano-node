#!/usr/bin/env bash

now=`date "+%Y-%m-%d 00:00:00"`

set -x
cabal new-run cardano-node -- \
    --system-start "$now" --slot-duration 2 \
    --log-config configuration/log-configuration.yaml \
    submit -t configuration/simple-topology.json \
           $@
