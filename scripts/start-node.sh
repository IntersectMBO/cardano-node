#!/usr/bin/env bash

now=`date "+%Y-%m-%d 00:00:00"`

. $(dirname $0)/lib-mode.sh
NODE="$(executable_runner cardano-node)"

set -x
${NODE} \
    --slot-duration 2 \
    --log-config configuration/log-configuration.yaml \
    --topology configuration/simple-topology.json \
         $@
