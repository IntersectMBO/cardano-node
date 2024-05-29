# shellcheck shell=bash

global_genesis_format_version=March-14-2023

usage_genesis() {
  usage "genesis" "Genesis" <<EOF
    $(helpcmd prepare-cache-entry [--force] PROFILE-JSON CACHEDIR NODE-SPECS OUTDIR)
                     Prepare a genesis cache entry for the specified profile.
                       Cache entry regeneration can be $(yellow --force)'d

    $(helpcmd derive-from-cache PROFILE-DIR TIMING-JSON-EXPR CACHE-ENTRY-DIR OUTDIR)
                     Instantiate genesis from a cache entry

    $(helpcmd genesis-from-preset PRESET-NAME OUTDIR)
                     ($(red DEV)) Prepare genesis for an environment preset,
                       like $(yellow mainnet)

    $(helpcmd actually-genesis PROFILE-JSON NODE-SPECS OUTDIR CACHE-KEY-INPUT CACHE-KEY)
                     ($(red DEV)) Internal procedure to actually generate genesis

    $(helpcmd finalise-cache-entry PROFILE-JSON TIMING-JSON-EXPR OUTDIR)
                     ($(red DEV)) Update a genesis cache entry to the given profile
EOF
}

genesis() {
set -euo pipefail

local op=${1:-$(usage_genesis)}; shift

if [[ $WB_MODULAR_GENESIS -eq 1 ]]; then
    info genesis "$(red using modular configuration)"
fi

if [[ $WB_CREATE_TESTNET_DATA -eq 1 ]]; then
    info genesis "$(red using create-testnet-data)"
fi

case "$op" in
    prepare-cache-entry )
        local usage="USAGE:  wb genesis $op [--force] PROFILE-JSON NODE-SPECS OUTDIR CACHEDIR"

        local regenesis_causes=()
        while test $# -gt 0; do
          case "$1" in
            --force )
              regenesis_causes+=('has--force')
              ;;
            * )
              break
              ;;
          esac
          shift
        done

        local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}
        local node_specs=${2:-$WB_SHELL_PROFILE_DATA/node-specs.json}
        local cacheDir=${3:-$(envjqr 'cacheDir')}

        local cache_key_input cache_key cache_path

        mkdir -p "$cacheDir"
        if [[ ! -w "$cacheDir" ]]; then
          fatal "profile | allocate failed to create writable cache directory:  $cacheDir"
        fi

        cache_key_input=$(genesis profile-cache-key-input "$profile_json")
        if [[ -z "$cache_key_input" ]]; then
          fatal "no valid profile JSON in $profile_json"
        fi
        cache_key=$(genesis profile-cache-key "$profile_json")
        cache_path=$cacheDir/genesis/$cache_key
        if [[ "$(realpath "$cacheDir"/genesis)" == "$(realpath "$cache_path")" ]]; then
          fatal "no valid genesis cache key associated with profile in $profile_json"
        fi

        if genesis cache-test "$cache_path"; then
          cache_hit=t
          cache_hit_desc='hit'
        else 
          cache_hit=
          cache_hit_desc='miss'
        fi

        progress "genesis | cache"  "preparing entry $cache_key:  $(red $cache_hit_desc) ($cache_path)"

        if [[ -z "$cache_hit" ]]; then
          regenesis_causes+=('cache-miss')
        elif [[ -v __REWRITE_GENESIS_CACHE ]]; then
          regenesis_causes+=('__REWRITE_GENESIS_CACHE-env-var-defined')
        fi

        local preset

        if [[ -n "${regenesis_causes[*]}" ]]; then
          info genesis "generating due to ${regenesis_causes[*]}:  $cache_key @$cache_path"

          jqtest .genesis.single_shot "$profile_json" ||
              fatal "Incremental (non single-shot) genesis is not supported."

          if profile has-preset "$profile_json"; then
            preset=$(jq .preset "$profile_json" -r)
            genesis genesis-from-preset "$preset" "$cache_path"
          else
            genesis actually-genesis "$profile_json" "$node_specs" "$cache_path" "$cache_key_input" "$cache_key"
          fi
        fi

        echo >&2

        realpath "$cache_path";;

    cache-test )
        local usage="USAGE:  wb genesis $op GENESIS-CACHE-DIR"
        local cache_dir=${1:?$usage}
        local version

        version=$(cat "$cache_dir"/layout.version 2>/dev/null || echo "unknown")

        if [[ ! "$version" == "$global_genesis_format_version" ]]; then
          info genesis "cache entry at $cache_dir is incompatible:  layout version '$version' does not match current: $global_genesis_format_version"
          return 1;
        fi
        return 0
        ;;

    profile-cache-key)
        if [[ $WB_MODULAR_GENESIS -eq 1 ]]; then
          profile-cache-key-modular "$@"
        else
          profile-cache-key-jq "$@"
        fi
        ;;

    profile-cache-key-input )
        if [[ $WB_MODULAR_GENESIS -eq 1 ]]; then
          profile-cache-key-input-modular "$@"
        else
          profile-cache-key-input-jq "$@"
        fi
        ;;

    genesis-from-preset )
        local usage="USAGE: wb genesis $op PRESET GENESIS-DIR"
        local preset=${1:?$usage}
        local dir=${2:?$usage}

        info genesis "profile uses preset genesis: $preset"

        rm -rf   "$dir"/{*-keys,byron,pools,nodes,*.json,*.params,*.version}
        mkdir -p "$dir/byron"
        local fs=(
            byron/genesis.json
            genesis-shelley.json
            genesis.alonzo.json
        )
        for f in "${fs[@]}"; do
          cp -f "$(profile preset-get-file "$preset" 'genesis file' genesis/"$f")" "$dir/$f"
        done;;

    spec )
        if [[ $WB_MODULAR_GENESIS -eq 1 ]]; then
          spec-modular "$@"
        else
          spec-jq "$@"
        fi
        ;;

    pool-relays )
        if [[ $WB_MODULAR_GENESIS -eq 1 ]]; then
          pool-relays-modular "$@"
        else
          pool-relays-jq "$@"
        fi
        ;;

    actually-genesis )
        local usage="USAGE: wb genesis $op PROFILE-JSON NODE-SPECS DIR"
        local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}
        local node_specs=${2:-$WB_SHELL_PROFILE_DATA/node-specs.json}
        local dir=${3:-$(mktemp -d)}
        local cache_key_input=${4:-$(genesis profile-cache-key-input "$WB_SHELL_PROFILE_DATA"/profile.json)}
        local cache_key=${5:-$(genesis profile-cache-key "$WB_SHELL_PROFILE_DATA"/profile.json)}

        progress "genesis" "new one: $(yellow profile) $(blue "$profile_json") $(yellow node_specs) $(blue "$node_specs") $(yellow dir) $(blue "$dir") $(yellow cache_key) $(blue "$cache_key") $(yellow cache_key_input) $(blue "$cache_key_input")"

        if [[ $WB_CREATE_TESTNET_DATA -eq 1 ]]; then
            genesis-create-testnet-data "$profile_json" "$dir"
        else
            genesis-create-staked "$profile_json" "$dir"
        fi

        info genesis "sealing"
        cat <<<"$cache_key_input"               > "$dir"/cache.key.input
        cat <<<"$cache_key"                     > "$dir"/cache.key
        cat <<<"$global_genesis_format_version" > "$dir"/layout.version
        ;;

    derive-from-cache )
        local usage="USAGE: wb genesis $op PROFILE-OUT TIMING-JSON-EXPR CACHE-ENTRY-DIR OUTDIR"
        local profile=${1:?$usage}
        local timing=${2:?$usage}
        local cache_entry=${3:?$usage}
        local outdir=${4:?$usage}

        mkdir -p "$outdir"

        local preset
        preset=$(profile preset "$profile")
        if [[ -n "$preset" ]]; then
          progress "genesis" "instantiating from preset $(with_color white "$preset"):  $cache_entry"
          mkdir -p "$outdir"/byron
          cp -f "$cache_entry"/genesis*.json "$outdir"
          cp -f "$cache_entry"/byron/*.json  "$outdir"/byron
          return
        fi

        progress "genesis" "deriving from cache:  $cache_entry -> $outdir"

        ln -s "$profile"                          "$outdir"/profile.json
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

        ## genesis
        cp    "$cache_entry"/genesis*.json        "$outdir"
        chmod u+w                                 "$outdir"/genesis*.json

        genesis finalise-cache-entry "$profile" "$timing" "$outdir"
        ;;

    finalise-cache-entry )
        local usage="USAGE: wb genesis $op PROFILE-JSON TIMING-JSON-EXPR DIR"
        local profile_json=${1:?$usage}
        local timing=${2:?$usage}
        local dir=${3:?$usage}

        if profile has-preset "$profile_json"; then
          return
        fi

        progress "genesis" "deriving from cache:  $cache_entry -> $outdir"

        local system_start_epoch
        system_start_epoch="$(jq '.start' -r <<<"$timing")"

        genesis-byron "$system_start_epoch" "$dir" "$profile_json"

        jq '. * { systemStart: $timing.systemStart }' --argjson timing "$timing" \
          "$dir"/genesis-shelley.json |
          sponge "$dir"/genesis-shelley.json
        ;;

    * )
      usage_genesis
      ;;

    esac
}

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

profile-cache-key-input-modular() {
    set -euo pipefail
    local profile_json=${1:-$WB_SHELL_PROFILE_DATA/profile.json}

    # nix wants absolute paths
    profile_json=$(realpath "$profile_json")

    # NOTE: jq is only used for formatting
    evaluate --profile "${profile_json}" genesis.cache-key-input | jq
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
        if   jqtest ".genesis.dense_pool_density > 1" "$profile_json" &&
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

genesis-byron() {
    set -euo pipefail
    if [[ $WB_MODULAR_GENESIS -eq 1 ]]; then
      genesis-byron-modular "$@"
    else
      genesis-byron-jq "$@"
    fi
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

    ln -sf shelley-genesis.spec.json "$dir/genesis-shelley.spec.json"
    ln -sf shelley-genesis.json      "$dir/genesis-shelley.json"
    ln -sf alonzo-genesis.spec.json  "$dir/genesis.alonzo.spec.json"
    ln -sf alonzo-genesis.json       "$dir/genesis.alonzo.json"
    ln -sf conway-genesis.spec.json  "$dir/genesis.conway.spec.json"
    ln -sf conway-genesis.json       "$dir/genesis.conway.json"

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

    info genesis "removing delegator keys."
    rm "$dir/stake-delegators" -rf

    info genesis "removing dreps keys."
    rm "$dir"/drep-keys -rf

    info genesis "moving keys"
    Massage_the_key_file_layout_to_match_AWS "$profile_json" "$node_specs" "$dir"

    info genesis "create-testnet-data genesis available in $dir"
}
