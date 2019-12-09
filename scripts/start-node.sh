#!/usr/bin/env bash

now=`date "+%Y-%m-%d 00:00:00"`

set -x
cabal v2-run exe:cardano-node -- \
    --slot-duration 2 \
    --log-config configuration/log-configuration.yaml \
    --topology configuration/simple-topology.json \
         $@
