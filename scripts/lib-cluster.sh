#!/usr/bin/env bash
# shellcheck disable=SC2154

TMUX_CONFIG="$(mktemp -t --suffix=tmuXXXXXXXX)"
run_3node_cluster() {
        dprint "run_3node_cluster: $*"
        local config="$1" topo="${2:-indexed}" delegate="${3:-indexed}"

        setup_genesis_for_config "${config}"
        prebuild 'cardano-node'

        tmux \
          -f "${TMUX_CONFIG}" \
          new-session \
          -E \
          -s "3node-cluster-${config}" \
          -n 'Main' \
          "${scripts}/shelley-testnet.tmux0.sh '${config}' '${topo}' '${delegate}'"
}
