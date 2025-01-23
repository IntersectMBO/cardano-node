usage_profile() {
     usage "profile" "Cluster profile operations" <<EOF
    $(helpcmd profile-names)          List profile names (JSON)
     $(blk names ls)
    $(helpcmd profile-json NAME)      Get contents of either named profile, or profile JSON desc
     $(blk json show pj)
    $(helpcmd profile-describe NAME)
     $(blk describe pdesc pd)       Human-readable profile overview

    $(helpcmd profile-node-specs PROFILE-NAME/JSON)
     $(blk node-specs specs)        Print node specs JSON for the given profile and environment
    $(helpcmd allocate-time PROFILE-NAME/JSON)
                             Allocate time for a run of a profile
    $(helpcmd describe-timing TIMING-JSON)
                             Explain timing allocation
EOF
}

profile_default_op='profile-json'

profile() {
local op=${1:-$profile_default_op}; test $# -gt 0 && shift

case "$op" in
    profile-names | names | list | lsp )
        cardano-profile names
        ;;

    all-profiles | all )
        cardano-profile all
        ;;

    has-profile )
        local usage="USAGE: wb profile $op NAME"
        local name=${1:?$usage}

        profile profile-names | jq --exit-status --arg name "$name" 'map (. == $name) | any' >/dev/null
        ;;

    compose )
        local profile_names="$@"

        profile all-profiles |
        jq --argjson profile_names "$(to_jsonlist ${profile_names[*]})" '
          . as $profiles
          | $profile_names | debug
          | map($profiles[.])
          | add
          ';;

    profile-json | json | show | pj )
        local usage="USAGE: wb profile $op [NAME=<current-shell-profile>"
        local name=${1:-${WB_SHELL_PROFILE:?variable unset, no profile name to use as a default.}}

        local json=$(if test -f  "$name"
                     then jq '.' "$name"
                     else profile all-profiles |
                             jq '.["'$name'"]'
                     fi)

        local preset=$(jq -r '.preset // ""' <<<$json)
        local preset_overlay=$global_basedir/profile/presets/$preset/overlay.json
        if test -z "$preset" -o ! -f $preset_overlay
        then echo "$json"
        else jq '. * $overlay[0]' <<<$json --slurpfile overlay $preset_overlay
        fi;;

    profile-describe | describe | pdesc | pd )
        local usage="USAGE: wb profile $op NAME"
        local name=${1:?$usage}

          profile json $name \
        | jq -r              \
            '
              [ "  - era:                \(.era)"
              , "  - epoch slots:        \(.genesis.epoch_length)"
              , "  - slot duration:      \(.genesis.slot_duration)"
              , "  - k:                  \(.genesis.parameter_k)"
              , "  - active slots coeff: \(.genesis.active_slots_coeff)"
              , "  - hosts:              \(.composition.n_hosts)"
              , "  - pools:              \(.composition.n_pools)"
              , "    - normal:             \(.composition.n_singular_pools)"
              , "    - dense:              \(.composition.n_dense_pools)"
              , "  - UTxO:               \(.genesis.utxo), of which:"
              , "    - delegated:          \(.derived.utxo_delegated)"
              , "    - generated:          \(.derived.utxo_generated)"
              , "    - stuffed:            \(.derived.utxo_stuffed)"
              , "  - delegators:         \(.genesis.delegators)"
              , "  - generator duration: \(.derived.generator_duration              | tostring)s"
              , "    - requested epochs:   \(.generator.epochs                      | tostring)ep"
              , "    - effective epochs:   \(.derived.effective_epochs              | tostring)ep"
              , "    - transaction count:  \(.derived.generator_tx_count | . / 1000 | ceil | tostring)kTx"
              , "    - full blocks:        \(.derived.generator_blocks_lower_bound  | tostring)"
              , ""
              ]
              + if .node.shutdown_on_slot_synced == null then []
                else [
                     "  - terminate at slot:  \(.node.shutdown_on_slot_synced)"
                     ]
                end
              + if .node.shutdown_on_block_synced == null then []
                else [
                     "  - terminate at block: \(.node.shutdown_on_block_synced)"
                     ]
                end
              + [""]
              | join("\n")
            '
        ;;

    has-preset )
        local usage="USAGE: wb profile $op NAME"
        local profile=${1:?$usage}
        profile json "$profile" | jqtest ".preset != null";;

    preset )
        local usage="USAGE: wb profile $op NAME"
        local profile=${1:?$usage}
        profile json "$profile" | jq -r '.preset // ""';;

    preset-get-file )
        local usage="USAGE: wb profile $op PRESET-NAME DESC FILE"
        local preset=${1:?$usage}
        local   desc=${2:?$usage}
        local   file=${3:?$usage}
        local preset_dir=$global_basedir/profile/presets/$preset
        local       path=$preset_dir/$file

        if   test ! -d "$preset_dir"; then fail "unknown preset: $preset"
        elif test ! -f "$path";       then fail "preset $preset has no file: $file"
        else echo "$path"
        fi;;

    profile-node-specs | node-specs | specs )
        local usage="USAGE: wb profile $op PROFILE-JSON TOPOLOGY-JSON"
        local profile=${1:?$usage}
        local topology=${2:?$usage}

        cardano-profile node-specs "${profile}" "${topology}";;

    allocate-time )
        local usage="USAGE: wb profile $op PROFILE-JSON"
        local profile=${1:?$usage}

        if profile has-preset "$profile"
        then
            local preset=$(profile json "$profile" | jq '.preset' -r)
            local shelley=$(profile preset-get-file "$preset" 'genesis' 'genesis/genesis-shelley.json')
            local start=$(jq '.systemStart | fromdateiso8601' $shelley)
            local offset="$((start - $(date +%s)))"
            local start_tag=$(date --date=@$(date +%s) --utc +'%Y'-'%m'-'%d'-'%H.%M')
        else
            local offset=$(profile json "$profile" |
                           jq '.derived.genesis_future_offset' --raw-output)
            local start=$(date '+%s' --date="now + $offset seconds")
            local start_tag=$(date --date=@$start --utc +'%Y'-'%m'-'%d'-'%H.%M')
        fi
        local args=(
            --arg 'start'           "$start"
            --arg 'start_human'     "$(date --date=@$start --utc +"%Y-%m-%dT%H:%M:%SZ")"
            --arg 'start_tag'       "$start_tag"
            --arg 'systemStart'     "$(date --date=@$start --utc --iso-8601=s | cut -c-19)Z"
        )

        profile json "$profile" | jq '
            .derived                                                           as $derived
          | .genesis                                                           as $genesis
          | ($systemStart | fromdateiso8601 | . + $derived.generator_duration) as $workload_end
          | (   $systemStart | fromdateiso8601 | .
              + ($derived.shutdown_time // $derived.generator_duration)
            )                                                                  as $shutdown_end
          | ( [$shutdown_end, $workload_end]
            | map(select(. != null))
            | min
            )                                                                  as $earliest_end
          |
          { future_offset:         $derived.genesis_future_offset
          , extra_future_offset:   $genesis.extra_future_offset
          , start:                 $start
          , shutdown_end:          $shutdown_end
          , workload_end:          $workload_end
          , earliest_end:          $earliest_end
          , start_tag:             $start_tag
          , start_human:           $start_human
          , systemStart:           $systemStart
          , shutdownTime:          ($shutdown_end | todateiso8601)
          , workloadEndTime:       ($workload_end | todateiso8601)
          , earliestEndTime:       ($earliest_end | todateiso8601)
          }
        ' "${args[@]}";;

    describe-timing )
        local usage="USAGE: wb profile $op TIMING-JSONEXPR"
        local timing=${1:?$usage}

        jq <<<$timing --raw-output '
          [ "  - future offset:         \(.future_offset) (of which \(.extra_future_offset) is extra)"
          , "  - start time:            \(.systemStart)"
          , "  - shutdown time:         \(.shutdownTime[:-1])"
          , "  - workload end time:     \(.workloadEndTime[:-1])"
          , "  - earliest end:          \(.earliestEndTime[:-1])"
          , ""
          ] | join("\n")
        ';;

    * ) set +x; usage_profile;; esac
}
