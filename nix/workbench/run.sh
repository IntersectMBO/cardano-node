global_runsdir_def=$PWD/run
global_runsdir=$global_runsdir_def
global_envjson=$global_runsdir/env.json

usage_run() {
     usage "run" "Managing cluster runs" <<EOF
    list                  List cluster runs

    allocate BATCH-NAME PROFILE-NAME [ENV-CONFIG-OPTS..]
                          Allocate a cluster run with the specified:
                            - batch key (no semantics attached)
                            - profile name
                          A unique name would be allocated for this run,
                            and a run alias 'current' will be created for it.

    start-run TAG BACKEND-ARGS..
                          Start the named run, passing thru any extra backend args

  Options:

    --runsdir DIR         Set the runs directory.  Defaults to $global_runsdir_def
EOF
}

run() {
while test $# -gt 0
do case "$1" in
       --runsdir )
           global_runsdir=$2; global_envjson=$global_runsdir/env.json; shift;;
       * ) break;; esac; shift; done

local op=${1:-list}; test $# -gt 0 && shift

case "$op" in
    list | ls )
        test -d "$global_runsdir" && cd "$global_runsdir" &&
            ls | {
                ## Filter out aliases:
                grep -v 'current\|env\.json' || true; }
        ;;

    compute-path )
        echo -n "$global_runsdir/$1";;

    check )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$(run compute-path "$tag")

        if test "$(tr -d / <<<$tag)" != "$tag"
        then fatal "run tag has slashes:  $tag"; fi

        for f in "$dir"/{profile,env,meta}.json
        do if ! jq_check_json "$f"
           then fatal "run $tag (at $dir) missing a file:  $(basename "$f")"
           fi
        done;;

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

        rm -f      "$global_runsdir"/current
        ln -s $tag "$global_runsdir"/current;;

    current-run-path | current-path | path )
        realpath "$global_runsdir"/current;;

    current-run-tag | current-tag | tag | current )
        basename "$(run current-path)";;

    current-run-meta | current-meta | meta )
        jq '.' "$(run current-path)"/meta.json;;

    current-run-profile | current-profile | profile | p )
        jq '.' "$(run current-path)"/profile.json;;

    allocate )
        local usage="USAGE: wb run $op BATCH-NAME PROFILE-NAME [ENV-CONFIG-OPTS..] [-- BACKEND-ENV-CONFIG-OPTS..]"
        local batch=${1:?$usage}; shift
        local  prof=${1:?$usage}; shift

        local cacheDir=$default_cacheDir basePort=$default_basePort staggerPorts='false'
        while test $# -gt 0
        do case "$1" in
               --cache-dir )     cacheDir=$2; shift;;
               --base-port )     basePort=$2; shift;;
               --stagger-ports ) staggerPorts=true; shift;;
               --* ) msg "FATAL:  unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done

        local timestamp=$(date +'%s' --utc)
        local date=$(date +'%Y'-'%m'-'%d'-'%H.%M' --date=@$timestamp --utc)
        local tag=$date.$batch.$prof
        local dir=$global_runsdir/$tag
        local realdir=$(realpath --canonicalize-missing "$dir")

        if test "$(dirname "$realdir")" != "$(realpath "$global_runsdir")"
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
            --arg       tag              $tag
            --arg       batch            $batch
            --arg       profile          $prof
            --argjson   timestamp        $timestamp
            --arg       date             $date
            --arg       node_commit_desc $node_commit_desc
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

        for node in $(jq_tolist 'keys' "$dir"/node-specs.json)
        do local node_dir="$dir"/$node
           mkdir -p                           "$node_dir"
           jq '.["'"$node"'"]' "$dir"/node-specs.json > "$node_dir"/node-spec.json
        done

        run     describe "$tag"
        profile describe "$dir"/profile.json

        run  set-current "$tag"

        msg "current run is:  $tag / $dir"
        ;;

    describe )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$global_runsdir/$tag

        if ! run check "$tag"
        then fatal "run fails sanity checks:  $tag at $dir"; fi

        cat <<EOF
workbench:  run $tag params:
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
        local usage="USAGE: wb run $op TAG BACKEND-ARGS.."
        local tag=${1:-?$usage}; shift
        local dir=$(run get "$tag")

        run set-current "$tag"
        local cacheDir=$(jq -r .cacheDir "$global_envjson")
        if test "$cacheDir" = 'null'
        then fatal "invalid meta.json in current run:  $dir/meta.json"; fi

        local genesis_args=(
            ## Positionals:
            "$cacheDir"/genesis
            "$dir"/profile.json
            "$dir"/topology
            "$dir"/genesis
        )
        genesis prepare "${genesis_args[@]}"

        backend start-run "$dir" "$@"

        run compat-meta-fixups "$tag"
        ;;

    * ) usage_run;; esac
}
