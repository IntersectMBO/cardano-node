global_rundir_def=$PWD/run
global_rundir_alt_def=$PWD/../cardano-ops/runs

usage_run() {
     usage "run" "Managing cluster runs" <<EOF
    list                  List cluster runs

    allocate BATCH-NAME PROFILE-NAME [ENV-CONFIG-OPTS..]
                          Allocate a cluster run with the specified:
                            - batch key (no semantics attached)
                            - profile name
                          A unique name would be allocated for this run,
                            and a run alias 'current' will be created for it.

    start [--scenario NAME] [--idle] TAG
                          Start the named run.
                            --scenario forces a scenario, different from what
                                is implied by the run profile;
                                See 'wb scenario --help' for scenario descriptions
                            --idle is a short-cut for the 'generic-idle' scenario

    stop TAG              Stop the named run

    restart [--no-generator] BACKEND-START-ARGS..
                          Stop and restart the current run (without a new allocation)

  Options:

    --rundir DIR          Set the runs directory.  Defaults to $global_rundir_def,
                            if it exists, otherwise to \$WORKBENCH_RUNDIR, if that
                            exists, and finally unconditionally to $global_rundir_def.
EOF
}

run() {
set -eu
if   test -d "$global_rundir_def"
then global_rundir=$global_rundir_def
## Allow compatibility with cardano-ops legacy runs directory layout:
elif test -v "WORKBENCH_RUNDIR" && test -d "$WORKBENCH_RUNDIR"
then global_rundir=$WORKBENCH_RUNDIR
else global_rundir=$global_rundir_def
     mkdir "$global_rundir"
fi

while test $# -gt 0
do case "$1" in
       --rundir ) global_rundir=$2; shift;;
       * ) break;; esac; shift; done

global_envjson=$global_rundir/env.json

local op=${1:-list}; test $# -gt 0 && shift

case "$op" in
    list | ls )
        test -d "$global_rundir" && cd "$global_rundir" &&
            ls | {
                ## Filter out aliases:
                grep -v 'current\|env\.json' || true; }
        ;;

    compute-path )
        echo -n "$global_rundir/$1";;

    check )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$(run compute-path "$tag")

        if test "$(tr -d / <<<$tag)" != "$tag"
        then fatal "run tag has slashes:  $tag"; fi

        jq_check_json "$dir"/meta.json ||
            fatal "run $tag (at $dir) missing a file:  meta.json"

        if test ! -f "$dir"/profile.json
        then # Legacy run structure, fix up:
            msg "fixing up legacy run in:  $dir"
            jq '.meta.profile_content' "$dir"/meta.json > "$dir"/profile.json

            local topdirs=$(ls -d "$dir"/logs-*/ 2>/dev/null || true)
            local anadirs=$(ls -d "$dir"/analysis/logs-*/ 2>/dev/null || true)
            if test -n "$topdirs"
            then for logdir in $topdirs
                 do local fixed=$(basename "$logdir" | cut -c6-)
                    mv "$logdir" "$dir"/$fixed; done
            elif test -n "$anadirs"
            then for logdir in $anadirs
                 do local fixed=$(basename "$logdir" | cut -c6-)
                    mv "$logdir" "$dir"/analysis/$fixed; done; fi

            jq_fmutate "$dir"/env.json '. *
              { type:         "legacy"
              , staggerPorts: false
              }
            '
        fi;;

    get-path | get )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        run check        "$tag"
        run compute-path "$tag";;

    show-meta | show | meta | s )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}

        jq '.' "$(run get "$tag")"/meta.json;;

    set-current | set )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$(run get "$tag")

        rm -f      "$global_rundir"/{current,-current}
        ln -s $tag "$global_rundir"/-current
        ln -s $tag "$global_rundir"/current;;

    current-run-path | current-path | path )
        realpath "$global_rundir"/current;;

    current-run-tag | current-tag | tag | current )
        basename "$(run current-path)";;

    current-run-meta | current-meta | meta )
        jq '.' "$(run current-path)"/meta.json;;

    current-run-profile | current-profile | profile | p )
        jq '.' "$(run current-path)"/profile.json;;

    allocate )
        local usage="USAGE: wb run $op BATCH-NAME PROFILE-NAME [ENV-CONFIG-OPTS..] [-- BACKEND-ARGS-AND-ENV-CONFIG-OPTS..]"
        local batch=${1:?$usage}; shift
        local  prof=${1:?$usage}; shift

        local cacheDir=$default_cacheDir basePort=$default_basePort staggerPorts='false'
        while test $# -gt 0
        do case "$1" in
               --profile-out )   profileOut=$2; shift;;
               --cache-dir )     cacheDir=$2; shift;;
               --base-port )     basePort=$2; shift;;
               --stagger-ports ) staggerPorts=true;;
               -- ) shift; break;;
               --* ) msg "FATAL:  unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done

        local timestamp=$(date +'%s' --utc)
        local date=$(date +'%Y'-'%m'-'%d'-'%H.%M' --date=@$timestamp --utc)
        local tag=$date.$batch.$prof
        local dir=$global_rundir/$tag
        local realdir=$(realpath --canonicalize-missing "$dir")

        if test "$(dirname "$realdir")" != "$(realpath "$global_rundir")"
        then fatal "bad tag/run dir:  $tag @ $dir"; fi

        if test -e "$dir"
        then fatal "tag busy:  $tag @ $dir"; fi

        if ! profile has-profile          "$prof"
        then fatal      "no such profile:  $prof"; fi

        mkdir -p "$cacheDir" && test -w "$cacheDir" ||
            fatal "failed to create writable cache directory:  $cacheDir"

        mkdir -p "$dir" && test -w "$dir" ||
            fatal "failed to create writable run directory:  $dir"

        local args=(
            --null-input
            --arg     cacheDir    "$cacheDir"
            --argjson basePort     $basePort
            --argjson staggerPorts $staggerPorts
        )
        jq_fmutate "$global_envjson" '
          { cacheDir:     $cacheDir
          , basePort:     $basePort
          , staggerPorts: $staggerPorts
          }
        ' "${args[@]}"
        backend record-extended-env-config "$global_envjson" "$@"
        cp "$global_envjson" "$dir"/env.json

        profile get "$prof" > "$dir"/profile.json
        profile node-specs    "$dir"/profile.json "$global_envjson" > "$dir"/node-specs.json

        ## TODO:  AWS
        local node_commit_desc=$(git_repo_commit_description '.')

        local args=(
            --arg       tag              "$tag"
            --arg       batch            "$batch"
            --arg       profile          "$prof"
            --argjson   timestamp        "$timestamp"
            --arg       date             "$date"
            --arg       node_commit_desc "$node_commit_desc"
            --slurpfile profile_content  "$dir"/profile.json
        )
        jq_fmutate "$dir"/meta.json '. *
           { meta:
             { tag:              $tag
             , batch:            $batch
             , profile:          $profile
             , timestamp:        $timestamp
             , date:             $date
             , node_commit_desc: $node_commit_desc
             , profile_content:  $profile_content[0]
             }
           }
           ' "${args[@]}"

        topology make    "$dir"/profile.json "$dir"/topology

        local svcs=$profileOut/node-services.json
        for node in $(jq_tolist 'keys' "$dir"/node-specs.json)
        do local node_dir="$dir"/$node
           mkdir -p                                          "$node_dir"
           jq      '."'"$node"'"' "$dir"/node-specs.json   > "$node_dir"/node-spec.json
           cp $(jq '."'"$node"'"."config"'         -r $svcs) "$node_dir"/config.json
           cp $(jq '."'"$node"'"."service-config"' -r $svcs) "$node_dir"/service-config.json
           cp $(jq '."'"$node"'"."start"'          -r $svcs) "$node_dir"/start.sh
           cp $(jq '."'"$node"'"."topology"'       -r $svcs) "$node_dir"/topology.json
        done

        local gtor=$profileOut/generator-service.json
        gen_dir="$dir"/generator
        mkdir -p                              "$gen_dir"
        cp $(jq '."run-script"'     -r $gtor) "$gen_dir"/run-script.json
        cp $(jq '."service-config"' -r $gtor) "$gen_dir"/service-config.json
        cp $(jq '."start"'          -r $gtor) "$gen_dir"/start.sh

        backend allocate-run "$dir"

        run     describe "$tag"
        profile describe "$dir"/profile.json

        run  set-current "$tag"

        msg "current run is:  $tag / $dir"
        ;;

    list-hosts | hosts )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$global_rundir/$tag

        if test -f "$dir"/node-specs.json
        then jq             'keys | .[]' -r "$dir"/node-specs.json
        else jq '.hostname | keys | .[]' -r "$dir"/meta.json; fi;;

    describe )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$global_rundir/$tag

        if ! run check "$tag"
        then fatal "run fails sanity checks:  $tag at $dir"; fi

        cat <<EOF
workbench:  run $(with_color yellow $tag) params:
  - run dir:         $dir
  - profile JSON:    $dir/profile.json
  - node specs:      $dir/node-specs.json
  - topology:        $dir/topology/topology-nixops.json $dir/topology/topology.pdf
  - node base port:  $(jq .basePort "$global_envjson")
EOF
        backend describe-run "$dir"
        ;;

    compat-meta-fixups | compat-f )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$(run get "$tag")

        local compat_args=(
            --rawfile genesis_cache_key "$dir/genesis/cache.key"
        )
        jq_fmutate "$dir"/meta.json '
           def compat_fixups:
             { genesis:
               { dense_pool_density: .composition.dense_pool_density
               , n_pools:            .composition.n_pools
               }
             , generator:
               { era:                .era
               }
             };
           . * { meta:
                 { profile_content:   (.meta.profile_content | compat_fixups)
                 , genesis_cache_id:  $genesis_cache_key
                 }
               }' "${compat_args[@]}";;

    start )
        local usage="USAGE: wb run $op [--no-generator] [--scenario NAME] TAG"

        local scenario_override=
        while test $# -gt 0
        do case "$1" in
               --idle )     scenario_override='generic-idle';;
               --scenario ) scenario_override=$2; shift;;
               --* ) msg "FATAL:  unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done

        local tag=${1:-?$usage}; shift
        local dir=$(run get "$tag")

        run set-current "$tag"
        local cacheDir=$(jq -r .cacheDir "$global_envjson")
        if test "$cacheDir" = 'null'
        then fatal "invalid meta.json in current run:  $dir/meta.json"; fi

        local genesis_args=(
            ## Positionals:
            "$cacheDir"/genesis "$dir"/profile.json
            "$dir"/topology
            "$dir"/genesis
        )
        genesis prepare "${genesis_args[@]}"

        ## Record geneses
        cp "$dir"/genesis/genesis-shelley.json "$dir"/genesis-shelley.json
        cp "$dir"/genesis/genesis-alonzo.json  "$dir"/genesis-alonzo.json

        ## Execute the scenario
        local scenario=${scenario_override:-$(jq -r .scenario "$dir"/profile.json)}
        scenario "$scenario" "$dir"

        run compat-meta-fixups "$tag"
        ;;

    stop )
        local tag=${1:-current}
        local dir=$(run get "$tag")

        if backend is-running "$dir"
        then backend stop-cluster "$dir"
             msg "cluster stopped"
        fi
        ;;

    restart )
        local tag=$(run current-tag)
        local dir=$(run get "$tag")

        test -d "$dir" ||
            fail "no valid current run to restart:  please set run/current appropriately"

        msg "restarting cluster in the same run directory: $dir"

        run stop                "$tag"
        jq_fmutate "$dir"/meta.json '
          { modifiers: { wiped_and_restarted: true }
          } * .
        '
        backend cleanup-cluster "$dir"
        run start          "$@" "$tag"

        msg "cluster re-started in the same run directory: $dir"
        ;;

    * ) usage_run;; esac
}
