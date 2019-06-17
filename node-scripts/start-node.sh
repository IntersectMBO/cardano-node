#!/usr/bin/env bash

now=`date "+%Y-%m-%d 00:00:00"`

set -x
cabal new-run demo-playground -- \
    --system-start "$now" --slot-duration 2 \
    node -t node-scripts/simple-topology.json \
         $@
