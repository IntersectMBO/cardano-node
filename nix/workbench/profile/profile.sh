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
        cardano-profile names;;

    all-profiles | all )
        cardano-profile all;;

    has-profile )
        local usage="USAGE: wb profile $op NAME"
        local name=${1:?$usage}

        profile profile-names | jq --exit-status --arg name "$name" 'map (. == $name) | any' >/dev/null;;

    ## XXX:  does not respect overlays!!
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

        profile json $name |
        jq 'include "prof3-derived";

           profile_pretty_describe(.)
           ' --raw-output -L "$global_basedir/profile" -L "$global_basedir";;

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
            -L "$global_basedir"
            --arg 'start'           "$start"
            --arg 'start_human'     "$(date --date=@$start --utc +"%Y-%m-%dT%H:%M:%SZ")"
            --arg 'start_tag'       "$start_tag"
            --arg 'systemStart'     "$(date --date=@$start --utc --iso-8601=s | cut -c-19)Z"
        )

        profile json "$profile" | jq '
          include "profile/profile-run";

          . as $prof
          | profile_timing($prof;
                           $start;
                           $start_human;
                           $start_tag;
                           $systemStart)
        ' "${args[@]}";;

    describe-timing )
        local usage="USAGE: wb profile $op TIMING-JSONEXPR"
        local timing=${1:?$usage}

        jq <<<$timing -L "$global_basedir" --raw-output '
          include "profile/profile-run";

          timing_pretty_describe(.)
        ';;

    * ) set +x; usage_profile;; esac
}
