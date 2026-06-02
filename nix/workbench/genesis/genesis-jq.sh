# shellcheck shell=bash
# shellcheck disable=SC2154  # global_basedir is set externally by the sourcing script (wb)
#
# Legacy (jq-based) genesis backend.
# Used when WB_MODULAR_GENESIS=0 (the default).
#
# Implements the backend interface:
#   profile-cache-key-input-jq, profile-cache-key-jq,
#   spec-jq, pool-relays-jq,
#   genesis-byron-jq,
#   genesis-create-jq

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

genesis-byron-jq() {
    local system_start_epoch=$1
    local dir=$2
    local profile_json=$3

    jq '
      { heavyDelThd:       "300000"
      , maxBlockSize:      "641000"
      , maxHeaderSize:     "200000"
      , maxProposalSize:   "700"
      , maxTxSize:         "4096"
      , mpcThd:            "200000"
      , scriptVersion:     0
      , slotDuration:      "20000"
      , softforkRule:
        { initThd:         "900000"
        , minThd:          "600000"
        , thdDecrement:    "100000"
        }
      , txFeePolicy:
        { multiplier:      "439460"
        , summand:         "155381"
        }
      , unlockStakeEpoch:  "184467"
      , updateImplicit:    "10000"
      , updateProposalThd: "100000"
      , updateVoteThd:     "100000"
      }
    ' --null-input > "$dir"/byron-protocol-params.json

    cli_args=(
        ## Note that these parameters are irrelevant by now.
        --genesis-output-dir         "$dir"/byron
        --protocol-parameters-file   "$dir"/byron-protocol-params.json
        --start-time                 "$system_start_epoch"
        --k                          "$(jq '.genesis.parameter_k' "$profile_json")"
        --protocol-magic             "$(jq '.genesis.network_magic' "$profile_json")"
        --n-poor-addresses           1
        --n-delegate-addresses       1
        --total-balance              300000
        --delegate-share             0.9
        --avvm-entry-count           0
        --avvm-entry-balance         0
    )
    rm -rf "$dir"/byron

    verbose "genesis" "$(colorise cardano-cli byron genesis genesis "${cli_args[@]}")"
    cardano-cli byron genesis genesis "${cli_args[@]}"
}

# Entry point for genesis creation.
genesis-create-jq() {
    if [[ $WB_CREATE_TESTNET_DATA -eq 1 ]]; then
        genesis-create-testnet-data "$@"
    else
        genesis-create-staked "$@"
    fi
}

genesis-create-staked() {
    local profile_json=$1
    local dir=$2

    rm -rf   "$dir"/{*-keys,byron,pools,nodes,*.json,*.params,*.version}
    mkdir -p "$dir"

    genesis spec shelley "$profile_json" >"$dir/genesis.spec.json"
    genesis spec alonzo  "$profile_json" >"$dir/genesis.alonzo.spec.json"
    genesis spec conway  "$profile_json" >"$dir/genesis.conway.spec.json"
    genesis pool-relays  "$profile_json" >"$dir/pool-relays.json"

    read -r -a args <<<"$(jq --raw-output '.cli_args.createStakedArgs | join(" ")' "$profile_json")"
    create_staked_args=(
        --genesis-dir "$dir"
        --relay-specification-file "$dir/pool-relays.json"
        "${args[@]}"
    )
    progress "genesis" "$(colorise cardano-cli genesis create-staked "${create_staked_args[@]}")"
    cardano-cli genesis create-staked "${create_staked_args[@]}"

    mv "$dir"/genesis.json "$dir"/genesis-shelley.json
    mv "$dir"/genesis.spec.json "$dir"/genesis-shelley.spec.json

    info genesis "removing delegator keys."
    rm "$dir"/stake-delegator-keys -rf

    info genesis "moving keys"
    Massage_the_key_file_layout_to_match_AWS "$profile_json" "$node_specs" "$dir"
}

genesis-create-testnet-data() {
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
    jq_fmutate "$dir/shelley-genesis.spec.json" -S .
    jq_fmutate "$dir/shelley-genesis.json"      -S .
    jq_fmutate "$dir/alonzo-genesis.spec.json"  -S .
    jq_fmutate "$dir/alonzo-genesis.json"       -S .
    jq_fmutate "$dir/conway-genesis.spec.json"  -S .
    jq_fmutate "$dir/conway-genesis.json"       -S .
    jq_fmutate "$dir/dijkstra-genesis.json"     -S .

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

