#!/usr/bin/env bash

set -x
cabal new-run wallet-client -- \
    --log-config configuration/log-configuration.yaml \
    $@
