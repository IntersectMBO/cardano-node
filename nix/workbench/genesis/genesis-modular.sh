# shellcheck shell=bash
#
# Modular (Nix-based) genesis backend.
# Used when WB_MODULAR_GENESIS=1.
#
# Implements the backend interface:
#   profile-cache-key-input-modular, profile-cache-key-modular,
#   genesis-cache-hit-modular, genesis-create-cache-modular, derive-from-cache-modular

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

# Same cache-hit test as legacy (uses jq's layout.version marker).
genesis-cache-hit-modular() {
    genesis-cache-hit-jq "$@";
}

# Entry point for genesis creation (delegates to legacy; modular only
# replaces cache-key/byron generation, not the create-testnet-data call).
genesis-create-cache-modular() {
    genesis-create-cache-jq "$@";
}

# Same derive-from-cache as legacy.
derive-from-cache-modular() {
    derive-from-cache-jq "$@";
}

