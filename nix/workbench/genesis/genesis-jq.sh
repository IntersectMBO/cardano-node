# shellcheck shell=bash
# shellcheck disable=SC2154  # global_basedir is set externally by the sourcing script (wb)
#
# Legacy (jq-based) genesis backend.
# Used when WB_MODULAR_GENESIS=0 (the default).
#
# Implements the backend interface:
#   profile-cache-key-input-jq, profile-cache-key-jq,
#   spec-jq, pool-relays-jq,
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

spec-jq() {
    set -euo pipefail
    local era=${1:?missing era}
    local profile_json=${2:?missing profile_json}
    # not needed in the jq version
    # local node_specs=${3:?missing node_specs}

    case "$1" in
    shelley)
        # We pass the network magic as an argument to cardano-cli, but, despite being required,
        # it is only used to amend a default template while we are bringing our own. So we put it into
        # the template too.
        jq '$prof[0] as $p
            | . * ($p.genesis.shelley // {})
            | .networkMagic |= $p.genesis.network_magic
            ' --slurpfile prof "$profile_json" \
            "$global_basedir"/profile/presets/mainnet/genesis/genesis-shelley.json
        ;;
    alonzo)
        jq '$prof[0] as $p | . * ($p.genesis.alonzo // {})' \
            --slurpfile prof "$profile_json" \
            "$global_basedir"/profile/presets/mainnet/genesis/genesis.alonzo.json
        ;;
    conway)
        jq '$prof[0] as $p | . * ($p.genesis.conway // {})' \
            --slurpfile prof "$profile_json" \
            "$global_basedir"/profile/presets/mainnet/genesis/genesis.conway.json
        ;;
    *)
        echo unknown era
        exit 1
        ;;
    esac
}

pool-relays-jq() {
    set -euo pipefail
    local profile_json=${1:?missing profile_json}
    # not needed in the jq version
    # local node_specs=${2:?missing node_specs}

    jq ' to_entries
       | map( select(.value.kind == "pool") )
       | map
         ({ key:   (.value.i | tostring)
          , value:
            [{ "single host name":
               { dnsName: .key
               , port:    .value.port
               }
             }
            ]
          })
       | from_entries
       ' "$node_specs"
}

# Entry point for genesis creation.
genesis-create-jq() {
    local profile_json=$1
    local dir=$2

    mkdir -p "$dir"

    genesis spec shelley "$profile_json" >"$dir/shelley-genesis.spec.json"
    genesis spec alonzo  "$profile_json" >"$dir/alonzo-genesis.spec.json"
    genesis spec conway  "$profile_json" >"$dir/conway-genesis.spec.json"
    genesis pool-relays  "$profile_json" >"$dir/pool-relays.json"

    local era
    era=$(jq --raw-output '.era' "$profile_json")

    # TODO if profile_json.composition.dense_pool_density != 1 -> create-testnet-data does not support dense pools
    read -r -a args <<<"$(jq --raw-output '.cli_args.createTestnetDataArgs | join(" ")' "$profile_json")"
    create_testnet_data_args=(
        --spec-shelley "$dir/shelley-genesis.spec.json"
        --spec-alonzo  "$dir/alonzo-genesis.spec.json"
        --spec-conway  "$dir/conway-genesis.spec.json"
        --relays       "$dir/pool-relays.json"
        --out-dir      "$dir"
        "${args[@]}"
    )
    progress genesis "$(colorise cardano-cli "$era" create-testnet-data "${create_testnet_data_args[@]}")"
    cardano-cli "$era" genesis create-testnet-data "${create_testnet_data_args[@]}"

    # for comparison/compatibility

    # genesis specs and finals
    jq_fmutate "$dir/byron.genesis.spec.json"   -S .
    jq_fmutate "$dir/byron-genesis.json"        -S .
    jq_fmutate "$dir/shelley-genesis.spec.json" -S .
    jq_fmutate "$dir/shelley-genesis.json"      -S .
    jq_fmutate "$dir/alonzo-genesis.spec.json"  -S .
    jq_fmutate "$dir/alonzo-genesis.json"       -S .
    jq_fmutate "$dir/conway-genesis.spec.json"  -S .
    jq_fmutate "$dir/conway-genesis.json"       -S .
    jq_fmutate "$dir/dijkstra-genesis.json"     -S .

    ln -sf byron.genesis.spec.json   "$dir/genesis-byron.spec.json"
    ln -sf byron-genesis.json        "$dir/genesis-byron.json"
    ln -sf shelley-genesis.spec.json "$dir/genesis-shelley.spec.json"
    ln -sf shelley-genesis.json      "$dir/genesis-shelley.json"
    ln -sf alonzo-genesis.spec.json  "$dir/genesis.alonzo.spec.json"
    ln -sf alonzo-genesis.json       "$dir/genesis.alonzo.json"
    ln -sf conway-genesis.spec.json  "$dir/genesis.conway.spec.json"
    ln -sf conway-genesis.json       "$dir/genesis.conway.json"
    ln -sf dijkstra-genesis.json     "$dir/genesis.dijkstra.json"

    shopt -s extglob

    function link_keys() {
        local from=${1}
        local to=${2}

        for k in "$dir/$from"/*/*; do
            local path=${k//"$dir/"/}
            local elems
            IFS=/ read -a elems -r <<<"$path"
            local no=${elems[1]##*([[:alpha:]])}
            local keyname=${elems[2]%.*}
            local keyext=${elems[2]##*.}
            ln -sf "../${path}" "$dir/${to}/${keyname}${no}.${keyext}"
        done
    }

    mkdir -p "$dir/pools"
    link_keys pools-keys pools

    mkdir -p "$dir/utxo-keys"
    link_keys utxo-keys utxo-keys

    local is_voting
    is_voting=$(jq --raw-output '.workloads | any( .name == "voting")' "$profile_json")
    if [[ "$is_voting" == "true" ]];
    then
        info genesis "voting workload specified - keeping one stake key per producer"
        mv "$dir/stake-delegators" "$dir/stake-delegators.bak"
        mkdir "$dir/stake-delegators"
        local pools
        pools="$(jq --raw-output '.composition.n_pools' "${profile_json}")"
        for i in $(seq 1 "$pools")
        do
          if test -d "$dir/stake-delegators.bak/delegator${i}"
          then
            local from_dir to_dir
            from_dir="$dir/stake-delegators.bak/delegator${i}"
            to_dir="$dir/stake-delegators/delegator$((i - 1))"
            mkdir "$to_dir"
            cp "$from_dir"/{payment,staking}.{skey,vkey} "$to_dir"/
          fi
        done
        rm "$dir/stake-delegators.bak" -rf
        info genesis "voting workload specified - skipping deletion of DRep keys"
    else
      info genesis "removing delegator keys."
      rm "$dir/stake-delegators" -rf
      info genesis "removing dreps keys."
      rm "$dir"/drep-keys -rf
    fi

    info genesis "moving keys"
    Massage_the_key_file_layout_to_match_AWS "$profile_json" "$node_specs" "$dir"

    info genesis "create-testnet-data genesis available in $dir"
}

__KEY_ROOT=
Massage_the_key_file_layout_to_match_AWS() {
    local profile_json=${1:?$usage}
    local node_specs=${2:?$usage}
    local dir=${3:?$usage}
    local ids
    local pool_density_map

    pool_density_map=$(topology density-map "$node_specs")
    if [[ -z "$pool_density_map" ]]; then
      fatal "failed: topology density-map '$node_specs'"
    fi
    info genesis "pool density map:  $pool_density_map"

    __KEY_ROOT=$dir

    read -r -a ids <<< "$(jq 'keys | join(" ")' -cr <<< "$pool_density_map")"

    local bid=1 pid=1 did=1 ## (B)FT, (P)ool, (D)ense pool
    for id in "${ids[@]}"; do
        mkdir -p "$dir"/node-keys/cold

        #### cold keys (do not copy to production system)
        if   jqtest ".composition.dense_pool_density > 1" "$profile_json" &&
             jqtest ".[\"$id\"]  > 1" <<<"$pool_density_map"
        then ## Dense/bulk pool
           info genesis "bulk pool $did -> node-$id"
           cp -f "$(key_genesis bulk      bulk "$did")" "$(key_depl bulk   bulk "$id")"
           did=$((did + 1))
        elif jqtest ".[\"$id\"] != 0" <<<"$pool_density_map"
        then ## Singular pool
           info genesis "pool $pid -> node-$id"
           cp -f "$(key_genesis cold       sig "$pid")" "$(key_depl cold    sig "$id")"
           cp -f "$(key_genesis cold       ver "$pid")" "$(key_depl cold    ver "$id")"
           cp -f "$(key_genesis opcert    cert "$pid")" "$(key_depl opcert none "$id")"
           cp -f "$(key_genesis opcert   count "$pid")" "$(key_depl cold  count "$id")"
           cp -f "$(key_genesis KES        sig "$pid")" "$(key_depl KES     sig "$id")"
           cp -f "$(key_genesis KES        ver "$pid")" "$(key_depl KES     ver "$id")"
           cp -f "$(key_genesis VRF        sig "$pid")" "$(key_depl VRF     sig "$id")"
           cp -f "$(key_genesis VRF        ver "$pid")" "$(key_depl VRF     ver "$id")"
           pid=$((pid + 1))
        else ## BFT node
           info genesis "BFT $bid -> node-$id"
           cp -f "$(key_genesis deleg      sig "$bid")" "$(key_depl cold    sig "$id")"
           cp -f "$(key_genesis deleg      ver "$bid")" "$(key_depl cold    ver "$id")"
           cp -f "$(key_genesis delegCert cert "$bid")" "$(key_depl opcert none "$id")"
           cp -f "$(key_genesis deleg    count "$bid")" "$(key_depl cold  count "$id")"
           cp -f "$(key_genesis delegKES   sig "$bid")" "$(key_depl KES     sig "$id")"
           cp -f "$(key_genesis delegKES   ver "$bid")" "$(key_depl KES     ver "$id")"
           cp -f "$(key_genesis delegVRF   sig "$bid")" "$(key_depl VRF     sig "$id")"
           cp -f "$(key_genesis delegVRF   ver "$bid")" "$(key_depl VRF     ver "$id")"
           bid=$((bid + 1))
        fi
    done
}

key_depl() {
    local type=$1 kind=$2 id=$3
    case "$kind" in
            bulk )     suffix='.creds';;
            cert )     suffix='.cert';;
            count )    suffix='.counter';;
            none )     suffix=;;
            sig )      suffix='.skey';;
            ver )      suffix='.vkey';;
            * )        fatal "key_depl: unknown key kind: '$kind'";; esac
    case "$type" in
            bulk )     stem=node-keys/bulk$id;;
            cold )     stem=node-keys/cold/operator$id;;
            opcert )   stem=node-keys/node$id.opcert;;
            KES )      stem=node-keys/node-kes$id;;
            VRF )      stem=node-keys/node-vrf$id;;
            * )        fatal "key_depl: unknown key type: '$type'";; esac
    echo "$__KEY_ROOT/$stem$suffix"
}

key_genesis() {
    local type=$1 kind=$2 id=$3
    case "$kind" in
            bulk )     suffix='.creds';;
            cert )     suffix='.cert';;
            count )    suffix='.counter';;
            none )     suffix=;;
            sig )      suffix='.skey';;
            ver )      suffix='.vkey';;
            * )        fatal "key_genesis: unknown key kind: '$kind'";; esac
    case "$type" in
            bulk )     stem=pools/bulk$id;;
            cold )     stem=pools/cold$id;;
            opcert )   stem=pools/opcert$id;;
            KES )      stem=pools/kes$id;;
            VRF )      stem=pools/vrf$id;;
            deleg )    stem=delegate-keys/delegate$id;;
            delegCert )stem=delegate-keys/opcert$id;;
            delegKES ) stem=delegate-keys/delegate$id.kes;;
            delegVRF ) stem=delegate-keys/delegate$id.vrf;;
            * )        fatal "key_genesis: unknown key type: '$type'";; esac
    echo "$__KEY_ROOT/$stem$suffix"
}

derive-from-cache-jq() {
    local usage="USAGE: wb genesis derive-from-cache PROFILE-OUT TIMING-JSON-EXPR CACHE-ENTRY-DIR OUTDIR"
    local profile_json=${1:?$usage}
    local timing=${2:?$usage}
    local cache_entry=${3:?$usage}
    local outdir=${4:?$usage}

    mkdir -p "$outdir"

    progress "genesis" "deriving from cache:  $cache_entry -> $outdir"

    ln -s "$profile_json"                     "$outdir"/profile.json
    ln -s "$cache_entry"                      "$outdir"/cache-entry
    ln -s "$cache_entry"/cache.key            "$outdir"
    ln -s "$cache_entry"/cache.key.input      "$outdir"
    ln -s "$cache_entry"/layout.version       "$outdir"

    # create-testnet-data does not create these directories if there are no keys in them
    [[ -d "$dir"/delegate-keys ]] && ln -s "$cache_entry"/delegate-keys "$outdir"
    [[ -d "$dir"/genesis-keys  ]] && ln -s "$cache_entry"/genesis-keys  "$outdir"

    cp -a "$cache_entry"/node-keys            "$outdir"
    chmod -R go-rwx                           "$outdir"/node-keys

    ln -s "$cache_entry"/pools                "$outdir"
    ln -s "$cache_entry"/stake-delegator-keys "$outdir"
    ln -s "$cache_entry"/utxo-keys            "$outdir"
    [[ -d "$cache_entry"/stake-delegators ]] && ln -s "$cache_entry"/stake-delegators "$outdir"
    [[ -d "$cache_entry"/drep-keys ]]        && ln -s "$cache_entry"/drep-keys        "$outdir"

    ## genesis
    cp    "$cache_entry"/genesis*.json        "$outdir"
    chmod u+w                                 "$outdir"/genesis*.json

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
      "$outdir"/genesis-byron.json |
      sponge "$outdir"/genesis-byron.json

    # Shelley: startTime, protocolVersion, maxBlockBodySize
    jq '$prof[0].genesis.shelley as $shey
         | $shey.protocolParams.protocolVersion   as $pver
         | $shey.protocolParams.maxBlockBodySize  as $bsize
         | . * { systemStart: $timing.systemStart }
         | if $pver  != null then . * { protocolParams: { protocolVersion:  $pver  } } else . end
         | if $bsize != null then . * { protocolParams: { maxBlockBodySize: $bsize } } else . end' \
      --argjson timing "$timing" \
      --slurpfile prof "$profile_json" \
      "$outdir"/genesis-shelley.json |
      sponge "$outdir"/genesis-shelley.json

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

