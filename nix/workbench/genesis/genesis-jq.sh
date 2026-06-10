# shellcheck shell=bash
# shellcheck disable=SC2154  # global_basedir is set externally by the sourcing script (wb)
#
# Legacy (jq-based) genesis backend.
# Used when WB_MODULAR_GENESIS=0 and WB_GENESIS_RIPPER=0 (the defaults).
#
# Implements the backend interface:
#   profile-cache-key-input-jq, profile-cache-key-jq,
#   genesis-create-jq, derive-from-cache-jq

profile-cache-key-input-jq() {
    set -euo pipefail
    local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}

    local args=(
        --slurpfile profile "$profile_json"
        --arg profile_json "$profile_json"
        --sort-keys
        --null-input
        -L "$global_basedir/profile"
        -L "$global_basedir/genesis"
    )
    jq 'include "genesis"; profile_genesis_cache_key($profile[0]; $profile_json)' "${args[@]}"
}

profile-cache-key-jq() {
    set -euo pipefail
    local usage="USAGE: wb genesis profile-cache-key PROFILE-JSON"
    local profile_json=${1:?$usage}
    local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}

    local args=(
        --arg params_hash "$(genesis profile-cache-key-input "$profile_json" | sha1sum | cut -c-7)"
        --slurpfile profile "$profile_json"
        --raw-output
        -L "$global_basedir/profile"
        -L "$global_basedir/genesis"
    )
    jq 'include "genesis"; profile_genesis_cache_entry_name($profile[0]; $params_hash)' "${args[@]}" "$profile_json"
}

# Entry point for genesis creation.
# Keeps in the provided directory all the output of `create-testnet-data`.
genesis-create-jq() {
    local profile_json=$1
    local dir=$2

    mkdir -p "$dir"

    jq   '.genesis.shelley // {}' "$profile_json" > "$dir/shelley-genesis.spec.json"
    jq   '.genesis.alonzo  // {}' "$profile_json" > "$dir/alonzo-genesis.spec.json"
    # Conway spec: use the profile's block when present, else a zero stub.
    # cardano-cli requires `--spec-conway` unconditionally, and the spec must
    # parse as a full conway genesis (not just {}).
    if [[ "$(jq -r '.genesis.conway // "null"' "$profile_json")" != "null" ]]; then
      jq '.genesis.conway'        "$profile_json" > "$dir/conway-genesis.spec.json"
    else
      # Stub that is parseable but it should never be activated to stay inert.
      # Activation check in service/nodes.nix (TestConwayHardForkAtEpoch).
      genesis conway-stub-spec                    > "$dir/conway-genesis.spec.json"
    fi

    # TODO if profile_json.composition.dense_pool_density != 1 -> create-testnet-data does not support dense pools
    read -r -a args <<<"$(jq --raw-output '.cli_args.createTestnetDataArgs | join(" ")' "$profile_json")"
    create_testnet_data_args=(
        --spec-shelley "$dir/shelley-genesis.spec.json"
        --spec-alonzo  "$dir/alonzo-genesis.spec.json"
        --spec-conway  "$dir/conway-genesis.spec.json"
        --out-dir      "$dir"
        "${args[@]}"
    )
    # Use `cardano-cli latest` rather than workbench's `eraName`:
    # - The era passed to `cardano-cli genesis create-testnet-data` only selects
    #   which CLI flavour is invoked and the output is the same.
    # - The workbench's `eraName` controls which Test<Era>HardForkAtEpoch flags
    #   the node config sets (see service/nodes.nix), independently from the
    #   spec generation step here.
    progress genesis "$(colorise cardano-cli latest genesis create-testnet-data "${create_testnet_data_args[@]}")"
    cardano-cli latest genesis create-testnet-data "${create_testnet_data_args[@]}"
    info genesis "create-testnet-data genesis available in $dir"
}

derive-from-cache-jq() {
    local usage="USAGE: wb genesis derive-from-cache PROFILE-OUT TIMING-JSON-EXPR CACHE-ENTRY-DIR OUTDIR"
    local profile_json=${1:?$usage}
    local timing=${2:?$usage}
    local cache_entry=${3:?$usage}
    local outdir=${4:?$usage} # output directory (run dir's genesis/, e.g. run/current/genesis).

    mkdir -p "$outdir"

    progress "genesis" "deriving from cache:  $cache_entry -> $outdir"

    ln -s "$profile_json"                     "$outdir"/profile.json
    ln -s "$cache_entry"                      "$outdir"/cache-entry
    ln -s "$cache_entry"/cache.key            "$outdir"
    ln -s "$cache_entry"/cache.key.input      "$outdir"
    ln -s "$cache_entry"/layout.version       "$outdir"

    # Directories: everything create-testnet-data can produce as symlinks.
    for keydir in byron-gen-command delegate-keys drep-keys genesis-keys pools-keys stake-delegators utxo-keys
    do
      if [[ -d "$cache_entry/$keydir" ]]; then
        ln -s "$cache_entry/$keydir" "$outdir/$keydir"
      else
        # Create an empty entry if directory does not exist.
        mkdir -p "$outdir/$keydir"
      fi
    done

    ## Files: all the genesis JSON files create-testnet-data produces.
    # Normalize create-testnet-data output names to genesis.<era>.json
    cp    "$cache_entry"/byron-genesis.json    "$outdir"/genesis.byron.json
    cp    "$cache_entry"/shelley-genesis.json  "$outdir"/genesis.shelley.json
    cp    "$cache_entry"/alonzo-genesis.json   "$outdir"/genesis.alonzo.json
    cp    "$cache_entry"/conway-genesis.json   "$outdir"/genesis.conway.json
    cp    "$cache_entry"/dijkstra-genesis.json "$outdir"/genesis.dijkstra.json
    chmod u+w                                  "$outdir"/genesis.*.json

    progress "genesis" "finalizing retrieved cache entry in:  $outdir"

    # The genesis cache entry in $outdir needs post-processing:
    # * the system start date might get an adjustment offset into the future
    # * some workbench profile content not captured by the genesis cache key will be patched in

    # For devs: this should cover all fields modified by
    # * nix/workbench/profile/pparams/delta-*.jq (workbench)
    # * bench/cardano-profile/data/genesis/overlays/*.json (cardano-profile)
    # These modifications are small deltas to base profiles, which do not, and should
    # not, result in recreation of the entire staked genesis, as that is large on-disk
    # and takes long to create.

    # Byron: patch startTime (same approach as shelley's systemStart below)
    local system_start_epoch
    system_start_epoch="$(jq '.start' -r <<<"$timing")"
    jq --argjson start "$system_start_epoch" '.startTime = $start' \
      "$outdir"/genesis.byron.json |
      sponge "$outdir"/genesis.byron.json

    # Shelley: startTime, protocolVersion, maxBlockBodySize
    jq '$prof[0].genesis.shelley as $shey
         | $shey.protocolParams.protocolVersion   as $pver
         | $shey.protocolParams.maxBlockBodySize  as $bsize
         | . * { systemStart: $timing.systemStart }
         | if $pver  != null then . * { protocolParams: { protocolVersion:  $pver  } } else . end
         | if $bsize != null then . * { protocolParams: { maxBlockBodySize: $bsize } } else . end' \
      --argjson timing "$timing" \
      --slurpfile prof "$profile_json" \
      "$outdir"/genesis.shelley.json |
      sponge "$outdir"/genesis.shelley.json

    # Alonzo: Execution budgets
    # NB. PlutusV1 and PlutusV2 cost models are *NOT* covered here; they're encoded
    #     as key-value-map in the profile, but need to be [Integer] in genesis.
    jq '$prof[0].genesis.alonzo as $alzo
         | $alzo.maxBlockExUnits.exUnitsMem   as $bl_mem
         | $alzo.maxBlockExUnits.exUnitsSteps as $bl_steps
         | $alzo.maxTxExUnits.exUnitsMem      as $tx_mem
         | $alzo.maxTxExUnits.exUnitsSteps    as $tx_steps
         | if $bl_mem   != null then . * { maxBlockExUnits: { memory: $bl_mem}   } else . end
         | if $bl_steps != null then . * { maxBlockExUnits: { steps:  $bl_steps} } else . end
         | if $tx_mem   != null then . * { maxTxExUnits:    { memory: $tx_mem}   } else . end
         | if $tx_steps != null then . * { maxTxExUnits:    { steps:  $tx_steps} } else . end' \
      --slurpfile prof "$profile_json" \
      "$outdir"/genesis.alonzo.json |
      sponge "$outdir"/genesis.alonzo.json

    # Conway: plutusV3CostModel
    jq '$prof[0].genesis.conway as $coay
         | $coay.plutusV3CostModel as $pv3cost
         | if $pv3cost != null then . * { plutusV3CostModel: $pv3cost } else . end' \
      --slurpfile prof "$profile_json" \
      "$outdir"/genesis.conway.json |
      sponge "$outdir"/genesis.conway.json

}

