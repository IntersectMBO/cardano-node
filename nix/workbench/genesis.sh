global_genesis_format_version=March-14-2022

usage_genesis() {
     usage "genesis" "Genesis" <<EOF
    prepare [--force] CACHEDIR PROFILE-JSON TOPO-DIR OUTDIR
                     Prepare a genesis cache entry for the specified profile.
                       Cache entry regeneration can be --force 'd

    profile-cache-key-input PROFILE-JSON
                     Output effective profile parameters factoring into the
                       genesis cache key

    profile-cache-key PROFILE-JSON
                     Output a genesis cache key for the specified profile

    actually-genesis PROFILE-JSON TOPO-DIR DIR
                     (DEV) Internal procedure to actually generate genesis

    finalise-cache-entry PROFILE-JSON DIR
                     (DEV) Update a genesis cache entry to the given profile
EOF
}

genesis() {
local op=${1:-$(usage_genesis)}; shift

case "$op" in
    prepare )
        local usage="USAGE:  wb genesis $op [--force] CACHEDIR PROFILE-JSON TOPO-DIR OUTDIR"

        local regenesis_causes=()
        while test $# -gt 0
        do case "$1" in
               --force ) regenesis_causes+=('has--force');;
               * ) break;; esac; shift; done

        local cachedir=${1:?$usage}
        local profile_json=${2:?$usage}
        local topo_dir=${3:?$usage}
        local outdir=${4:?$usage}

        local cache_key_input=$(genesis profile-cache-key-input "$profile_json")
        if test -z "$cache_key_input"
        then fatal "no valid profile JSON in $profile_json"
        fi
        local cache_key=$(genesis profile-cache-key "$profile_json")
        local cache_path=$cachedir/$cache_key
        if test "$(realpath $cachedir)" = "$(realpath $cache_path)"
        then fatal "no valid genesis cache key associated with profile in $profile_json"
        fi

        if genesis cache-test "$cache_path"
        then cache_hit=t; cache_hit_desc='hit'
        else cache_hit=;  cache_hit_desc='miss'; fi
        msg "genesis: preparing cache entry $cache_key:  $cache_hit_desc ($cache_path)"

        if   test -z "$cache_hit"
        then regenesis_causes+=('cache-miss')
        elif test -v __REWRITE_GENESIS_CACHE
        then regenesis_causes+=('__REWRITE_GENESIS_CACHE-env-var-defined')
        fi

        if test -n "${regenesis_causes[*]}"
        then msg "genesis: generating due to ${regenesis_causes[*]}:  $cache_key @$cache_path"
             jqtest .genesis.single_shot "$profile_json" ||
                 fatal "Incremental (non single-shot) genesis is not supported."

             if profile has-preset "$profile_json"
             then local preset=$(jq .preset "$profile_json" -r)
                  genesis genesis-from-preset "$preset" "$cache_path"
             else genesis actually-genesis "$profile_json" "$topo_dir" "$cache_path" "$cache_key_input" "$cache_key"
             fi
        fi

        genesis finalise-cache-entry "$profile_json" "$cache_path"

        ## Install the cache entry:
        rm -f $outdir ## Must be a symlink for this to succeed
        ln -s "$(realpath "$cache_path")" "$outdir";;

    cache-test )
        local usage="USAGE:  wb genesis $op GENESIS-CACHE-DIR"
        local cache_dir=${1:?$usage}
        local version=$(cat "$cache_dir"/layout.version 2>/dev/null || echo "unknown")

        test "$version" == $global_genesis_format_version
        ret=$?
        if test $ret != 0
        then msg "genesis:  cache entry at $cache_dir is incompatible:  layout version '$version' does not match current: $global_genesis_format_version"
        fi
        return $ret;;

    profile-cache-key-input )
        local usage="USAGE:  wb genesis $op PROFILE-JSON"
        local profile_json=${1:?$usage}

        local args=(
            --slurpfile profile      $profile_json
            --arg       profile_json $profile_json
            --sort-keys
            --null-input
            -L "$global_basedir/profiles"
        )
        jq 'include "genesis";

           profile_genesis_cache_key($profile[0]; $profile_json)
           ' ${args[*]};;

    profile-cache-key )
        local usage="USAGE:  wb genesis $op PROFILE-JSON"
        local profile_json=${1:?$usage}

        local args=(
            --arg params_hash $(genesis profile-cache-key-input $profile_json |
                                    sha1sum | cut -c-7)
            --slurpfile profile $profile_json
            --raw-output
            -L "$global_basedir/profiles"
        )
        jq 'include "genesis";

           profile_genesis_cache_entry_name($profile[0]; $params_hash)
           ' ${args[*]} $profile_json;;

    genesis-from-preset )
        local usage="USAGE:  wb genesis $op PRESET GENESIS-DIR"
        local preset=${1:?$usage}
        local dir=${2:?$usage}

        msg "genesis: profile uses preset genesis: $preset"

        rm -rf   "$dir"/{*-keys,byron,pools,nodes,*.json,*.params,*.version}
        mkdir -p "$dir/byron"
        local fs=(
            byron/genesis.json
            genesis-shelley.json
            genesis.alonzo.json
        )
        for f in ${fs[*]}
        do cp -f "$(profile preset-get-file $preset 'genesis file' genesis/$f)" "$dir/$f"
        done;;

    actually-genesis )
        local usage="USAGE:  wb genesis $op PROFILE-JSON TOPO-DIR DIR"
        local profile_json=${1:?$usage}
        local topo_dir=${2:?$usage}
        local dir=${3:?$usage}
        local cache_key_input=$4
        local cache_key=$5

        rm -rf   "$dir"/{*-keys,byron,pools,nodes,*.json,*.params,*.version}
        mkdir -p "$dir"

        jq ' $prof[0] as $p
           | . * ($p.genesis.shelley // {})
           | . * ($p.genesis.alonzo // {})
           ' --slurpfile prof       "$profile_json"  \
           "$global_basedir"/profiles/presets/mainnet/genesis/genesis-alonzo.json \
           >   "$dir"/genesis.alonzo.spec.json

        msg "genesis:  creating initial genesis"
        cardano-cli genesis create --genesis-dir "$dir"/ \
            $(jq '.cli_args.createSpec | join(" ")' "$profile_json" --raw-output)

        ## Overlay the verbatim genesis part into the profile spec:
        local params=(
            --slurpfile prof "$profile_json"
            --raw-output
        )
        jq ' $prof[0] as $p
           | . * ($p.genesis.shelley // {})
           '   "$dir"/genesis.spec.json "${params[@]}" |
        sponge "$dir"/genesis.spec.json

        msg "genesis:  mutating into staked genesis"
        params=(--genesis-dir "$dir"
                $(jq '.cli_args.createFinalBulk | join(" ")' "$profile_json" --raw-output)
               )
        cardano-cli genesis create-staked "${params[@]}"
        mv "$dir"/genesis.json "$dir"/genesis-shelley.json
        mv "$dir"/genesis.spec.json "$dir"/genesis-shelley.spec.json

        msg "genesis:  removing delegator keys.."
        rm "$dir"/stake-delegator-keys -rf

        cat <<<$cache_key_input               > "$dir"/cache.key.input
        cat <<<$cache_key                     > "$dir"/cache.key
        cat <<<$global_genesis_format_version > "$dir"/layout.version

        msg "genesis:  moving keys"
        ## TODO: try to get rid of this step:
        Massage_the_key_file_layout_to_match_AWS "$profile_json" "$topo_dir" "$dir";;

    derive-from-cache )
        local usage="USAGE:  wb genesis $op PROFILE-OUT CACHE-ENTRY-DIR OUTDIR"
        local profile=${1:?$usage}
        local cache_entry=${2:?$usage}
        local outdir=${3:?$usage}

        msg "genesis | derive-from-cache:  $cache_entry -> $outdir"
        ls -l $cache_entry

        mkdir -p "$outdir"
        ( cd $outdir
          ln -s $profile   ./profile
          ln -s $cache_entry cache-entry
          ln -s $cache_entry/cache.key
          ln -s $cache_entry/cache.key.input
          ln -s $cache_entry/layout.version
          ## keys
          ln -s $cache_entry/delegate-keys
          ln -s $cache_entry/genesis-keys
          cp    $cache_entry/node-keys . -a
          chmod -R    go-rwx node-keys
          ln -s $cache_entry/pools
          ln -s $cache_entry/stake-delegator-keys
          ln -s $cache_entry/utxo-keys
          ## JSON
          cp -v $cache_entry/genesis*.json .
          chmod u+w          genesis*.json
        )
        genesis finalise-cache-entry $profile/profile.json $outdir

        ls -l $outdir/node-keys
        ;;

    finalise-cache-entry )
        local usage="USAGE:  wb genesis $op PROFILE-JSON DIR"
        local profile_json=${1:?$usage}
        local dir=${2:?$usage}

        if profile has-preset "$profile_json"
        then return; fi

        ## Decide start time:
        local system_start_epoch=$(date '+%s' --date="now + $(jq .genesis.genesis_future_offset "$profile_json" --raw-output)")
        local system_start_human=$(date --date=@$system_start_epoch --utc +"%Y-%m-%dT%H:%M:%SZ")
        local start_time=$(date --date=@$system_start_epoch --utc --iso-8601=s |
                           cut -c-19)

        genesis_byron "$system_start_epoch" "$dir"

        jq ' $prof[0] as $p
           | . * ($p.genesis.shelley // {})
           | . * { systemStart: $start_time }
           ' --slurpfile prof       "$profile_json"  \
             --arg       start_time "${start_time}Z" \
               "$dir"/genesis-shelley.json |
        sponge "$dir"/genesis-shelley.json

        jq ' $prof[0] as $p
           | . * ($p.genesis.alonzo // {})
           ' --slurpfile prof       "$profile_json"  \
           "$global_basedir"/profiles/presets/mainnet/genesis/genesis-alonzo.json \
           >   "$dir"/genesis.alonzo.json;;

    * ) usage_genesis;; esac
}

__KEY_ROOT=
Massage_the_key_file_layout_to_match_AWS() {
    local profile_json=${1:?$usage}
    local topo_dir=${2:?$usage}
    local dir=${3:?$usage}
    local ids

    set -euo pipefail

    local pool_density_map=$(topology density-map "$profile_json" "$topo_dir")
    msg "genesis: pool density map:  $pool_density_map"

    __KEY_ROOT=$dir

    ids=($(jq 'keys
              | join(" ")
              ' -cr <<<$pool_density_map))
    local bid=1 pid=1 did=1 ## (B)FT, (P)ool, (D)ense pool
    for id in ${ids[*]}
    do
        mkdir -p "$dir"/node-keys/cold

        #### cold keys (do not copy to production system)
        if   jqtest ".genesis.dense_pool_density > 1" $profile_json &&
             jqtest ".[\"$id\"]  > 1" <<<$pool_density_map
        then ## Dense/bulk pool
           msg "genesis:  bulk pool $did -> node-$id"
           cp -f $(key_genesis bulk      bulk $did) $(key_depl bulk   bulk $id)
           did=$((did + 1))
        elif jqtest ".[\"$id\"] != 0" <<<$pool_density_map
        then ## Singular pool
           msg "genesis:  pool $pid -> node-$id"
           cp -f $(key_genesis cold       sig $pid) $(key_depl cold    sig $id)
           cp -f $(key_genesis cold       ver $pid) $(key_depl cold    ver $id)
           cp -f $(key_genesis opcert    cert $pid) $(key_depl opcert none $id)
           cp -f $(key_genesis opcert   count $pid) $(key_depl cold  count $id)
           cp -f $(key_genesis KES        sig $pid) $(key_depl KES     sig $id)
           cp -f $(key_genesis KES        ver $pid) $(key_depl KES     ver $id)
           cp -f $(key_genesis VRF        sig $pid) $(key_depl VRF     sig $id)
           cp -f $(key_genesis VRF        ver $pid) $(key_depl VRF     ver $id)
           pid=$((pid + 1))
        else ## BFT node
           msg "genesis:  BFT $bid -> node-$id"
           cp -f $(key_genesis deleg      sig $bid) $(key_depl cold    sig $id)
           cp -f $(key_genesis deleg      ver $bid) $(key_depl cold    ver $id)
           cp -f $(key_genesis delegCert cert $bid) $(key_depl opcert none $id)
           cp -f $(key_genesis deleg    count $bid) $(key_depl cold  count $id)
           cp -f $(key_genesis delegKES   sig $bid) $(key_depl KES     sig $id)
           cp -f $(key_genesis delegKES   ver $bid) $(key_depl KES     ver $id)
           cp -f $(key_genesis delegVRF   sig $bid) $(key_depl VRF     sig $id)
           cp -f $(key_genesis delegVRF   ver $bid) $(key_depl VRF     ver $id)
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
    echo "$__KEY_ROOT"/$stem$suffix
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
    echo "$__KEY_ROOT"/$stem$suffix
}

genesis_byron()
{
    local system_start_epoch=$1
    local dir=$2

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
        --start-time                 $system_start_epoch
        --k                          2160
        --protocol-magic             42
        --n-poor-addresses           1
        --n-delegate-addresses       1
        --total-balance              300000
        --delegate-share             0.9
        --avvm-entry-count           0
        --avvm-entry-balance         0
        )
    rm -rf "$dir"/byron
    cardano-cli byron genesis genesis "${cli_args[@]}"
}
