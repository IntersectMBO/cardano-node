#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2154,SC2034,SC2124,SC2086
# ./scripts/chairman.sh --nix ./socket/node-[012]-socket

DEFAULT_VERBOSE=t
. "$(dirname "$0")"/common.sh
. "$(dirname "$0")"/lib-cli.sh

setup_genesis_for_config 'liveview'

# it does not make sense to run chairman just for a single node
[ $# -le 1 ] && echo "Usage: $(basename "$0") TargetSocketFilePath" 1>&2 && exit 1

SOCKET_PATHS=${@/#/--socket-path }

run cardano-node chairman \
        -p 25 \
        -t 1000 \
        --config "${configuration_root}/config-0.yaml" \
        $SOCKET_PATHS
