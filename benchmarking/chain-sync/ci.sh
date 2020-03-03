#!/usr/bin/env nix-shell
#!nix-shell -i bash -p yj

if [ $# -lt 1 ]; then
  echo "call: $0 mainnet|testnet"
  exit 1
fi

set -euo pipefail

BASEDIR="$(dirname $0)"

# >> cpu time limit in seconds
TIME_LIMIT=$((60*60))

CLUSTER="$1"

LOG_CONFIG="$(yj < $BASEDIR/configuration/log-config-ci.yaml)"

CUSTOM_CONFIG="{nodeConfig = builtins.fromJSON ''$LOG_CONFIG'';}"

mkdir -p state-ci-chain-sync
nix build --out-link ./state-ci-chain-sync/launch_node -f $BASEDIR/../.. scripts.$CLUSTER.node --arg customConfig "$CUSTOM_CONFIG"
cd state-ci-chain-sync

rm -rf "./state-node-$CLUSTER"

echo
echo "configuration"
echo "============="
echo "${LOG_CONFIG}"
echo
echo "topology"
echo "========"
TOPOLOGY=`cat launch_node | sed -ne 's/.* --topology \([^ ]\+\) .*/\1/p;' | tail -1`
cat "${TOPOLOGY}"
echo
echo

RTS="+RTS -T -I0 -N2 -A16m -RTS"
RTS=""
timeout ${TIME_LIMIT} ./launch_node ${RTS} || true

$BASEDIR/analyse-logs.sh | tee benchmark-results.log
