#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yj

if [ $# -lt 1 ]; then
  echo "call: $0 mainnet|testnet"
  exit 1
fi

set -euo pipefail

BASEDIR="$(dirname $0)"
HOURS="$2"

# >> cpu time limit in seconds
TIME_LIMIT=$((${HOURS}*60*60))

CLUSTER="$1"

LOG_CONFIG="$(yj < $BASEDIR/configuration/log-config-ci.yaml)"

CUSTOM_CONFIG="{nodeConfig = builtins.fromJSON ''$LOG_CONFIG'';}"

nix build --out-link ./launch_node -f $BASEDIR/../.. scripts.$CLUSTER.node --arg customConfig "$CUSTOM_CONFIG"

rm -rf "./state-node-$CLUSTER"

timeout ${TIME_LIMIT} ./launch_node || true

$BASEDIR/analyse-logs.sh ${CLUSTER} | tee benchmark-full-sync-results.log
