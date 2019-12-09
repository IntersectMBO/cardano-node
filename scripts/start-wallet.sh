#!/usr/bin/env bash

set -x
cabal v2-run wallet-client -- \
    --log-config configuration/log-configuration.yaml \
    $@
