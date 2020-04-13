#!/usr/bin/env bash
# shellcheck disable=SC2154

run_3node_cluster() {
        dprint "run_3node_cluster: $*"
        local config="$1" topo="${2:-indexed}" delegate="${3:-indexed}"

        setup_genesis_for_config "${config}"
        prebuild 'cardano-node'

        self=$(basename "$0")
        tmux new-session \
          -E \
          -s "${self%.sh}" \
          -n 'Main' \
          "${scripts}/shelley-testnet.tmux0.sh '${config}' '${topo}' '${delegate}'"
}
