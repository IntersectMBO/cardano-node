# shellcheck shell=bash
#
# Modular (Nix-based) genesis backend.
# Used when WB_MODULAR_GENESIS=1.

profile-cache-key-input-modular() {
    set -euo pipefail
    local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}

    # nix wants absolute paths
    profile_json=$(realpath "$profile_json")

    # NOTE: jq is only used for formatting
    evaluate --profile "${profile_json}" genesis.cache-key-input | jq
}

profile-cache-key-modular() {
    local usage="USAGE: wb genesis profile-cache-key PROFILE-JSON"
    local profile_json=${1:?$usage}

    # nix wants absolute paths
    profile_json=$(realpath "$profile_json")

    # NOTE:
    # - the hash is different because nix cannot reproduce jq's pretty-printing
    # - jq is only used for formatting
    evaluate --profile "${profile_json}" genesis.cache-key | jq -r
}

spec-modular() {
    set -euo pipefail
    local era=${1:?missing era}
    local profile_json=${2:?missing profile_json}
    local node_specs=${3:?missing node_specs}

    # nix wants absolute paths
    profile_json=$(realpath "$profile_json")
    node_specs=$(realpath "$node_specs")

    # TODO: check for errors
    # NOTE: jq is only used for to keep the same formatting as the jq version
    evaluate --profile "$profile_json" --node-specs "$node_specs" "genesis.$era" | jq
}

pool-relays-modular() {
    set -euo pipefail
    local profile_json=${1:?missing profile_json}
    local node_specs=${2:?missing node_specs}

    # nix wants absolute paths
    profile_json=$(realpath "$profile_json")
    node_specs=$(realpath "$node_specs")

    # NOTE: jq is only used for to keep the same formatting as the jq version
    evaluate --profile "$profile_json" --node-specs "$node_specs" genesis.pool-relays | jq
}

genesis-byron-modular() {
    local system_start_epoch=$1
    local dir=$2
    local profile_json=$3

    # nix wants absolute paths
    profile_json=$(realpath "$profile_json")

    evaluate --profile "$profile_json" genesis.byron > "$dir"/byron-protocol-params.json
    read -r -a args <<< "$(evaluate --profile "${profile_json}" genesis.byron-genesis-args | jq -r)"

    cli_args=(
        --genesis-output-dir         "$dir"/byron
        --protocol-parameters-file   "$dir"/byron-protocol-params.json
        --start-time                 "$system_start_epoch"
        "${args[@]}"
    )
    rm -rf "$dir"/byron

    verbose "genesis" "$(colorise cardano-cli byron genesis genesis "${cli_args[@]}")"
    cardano-cli byron genesis genesis "${cli_args[@]}"
}

