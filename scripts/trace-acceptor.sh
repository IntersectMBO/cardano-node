#!/usr/bin/env bash

now=`date "+%Y-%m-%d 00:00:00"`

set -x
stack exec cardano-node -- \
    --system-start "$now" --slot-duration 2 \
    --log-config configuration/log-config-acceptor.yaml \
    trace-acceptor \
           $@
