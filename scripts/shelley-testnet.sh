#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2034

DEFAULT_VERBOSE=t
echo "========= shell-test-net.sh ========="
. "$(dirname "$0")"/common.sh
. "$(dirname "$0")"/lib-cli.sh
. "$(dirname "$0")"/lib-node.sh
. "$(dirname "$0")"/lib-cluster.sh

run_3node_cluster 'simpleview'
