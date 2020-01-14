#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yj

set -euo pipefail

BASEDIR="$(dirname $0)"

# >> cpu time limit in seconds
CPU_TIME_LIMIT=$((1*60*60))

CLUSTER="$1"

LOG_CONFIG="$(yj < $BASEDIR/configuration/log-configuration.yaml)"

CUSTOM_CONFIG="{nodeConfig = builtins.fromJSON ''$LOG_CONFIG'';}"

nix build --out-link ./launch_node -f $BASEDIR/../.. scripts.$CLUSTER.node --arg customConfig "$CUSTOM_CONFIG"

rm -rf "./state-node-$CLUSTER"

ulimit -t $CPU_TIME_LIMIT

./launch_node || true

$BASEDIR/analyse-logs.sh | tee benchmark-results.log
