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
        test -d "$global_rundir" &&
            (cd "$global_rundir"
             find . -mindepth 1 -maxdepth 1 -type d |
                 cut -c3- |
                 grep -v 'current$' || true);;

    compute-path )
        echo -n "$global_rundir/$1";;

    fix-legacy-run-structure | fix-legacy )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$(run compute-path "$tag")

        if test ! -f "$dir"/genesis-shelley.json
        then msg "fixing up genesis naming in:  $dir"
             mv "$dir"/genesis.json "$dir"/genesis-shelley.json
             ln -s genesis-shelley.json "$dir"/genesis.json; fi

        if test -z "$(ls -d "$dir"/node-* 2>/dev/null)"
        then msg "fixing up a legacy cardano-ops run in:  $dir"
             if   local dirs=$(ls -d "$dir"/logs/logs-*/ 2>/dev/null || true)
                  test -n "$dirs"
             then for logdir in $dirs
                  do local logs_less=$(basename "$logdir" | cut -c6-)
                     mv "$logdir" "$dir"/$logs_less; done
             elif local dirs=$(ls -d "$dir"/analysis/logs-*/ 2>/dev/null || true)
                  test -n "$dirs"
             then for logdir in $dirs
                  do local logs_less=$(basename "$logdir" | cut -c6-)
                     mv "$logdir" "$dir"/$logs_less; done; fi
        else msg "fixing up a cardano-ops run in:  $dir"; fi

        jq '.meta.profile_content' "$dir"/meta.json > "$dir"/profile.json

        jq_fmutate "$dir"/env.json '. *
          { type:         "legacy"
          , staggerPorts: false
          }
        ';;

    check )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$(run compute-path "$tag")

        test "$(tr -d / <<<$tag)" = "$tag" ||
            fatal "run tag has slashes:  $tag"

        jq_check_json "$dir"/meta.json ||
            fatal "run $tag (at $dir) missing a file:  meta.json"

        test -f "$dir"/profile.json -a -f "$dir"/genesis-shelley.json ||
            run fix-legacy-run-structure "$tag";;

    fix-systemstart )
        local usage="USAGE: wb run $op TAG [MACH=node-1]"
        local tag=${1:?$usage}
        local mach=${2:-node-1}
        local dir=$(run compute-path "$tag")
        local nodelog=$(ls $dir/logs/$mach/node-*.json | head -n1)
        local genesis=$dir/genesis-shelley.json

        msg "cross-checking systemStart of $tag:  $nodelog"
        local apparent_systemStart=$(grep -F 'TraceStartLeadershipCheck' $nodelog |
                                     head -n2 |
                                     tail -n1 |
                                     jq '[ (.at | "\(.[:19])Z" | fromdateiso8601)
                                         , .data.slot
                                         ] | .[0] - .[1]
                                           | todateiso8601' -r)
        local genesis_systemStart=$(jq .systemStart $genesis -r)

        if test "$genesis_systemStart" != "$apparent_systemStart"
        then msg "Fixing genesis systemStart in $tag:  $apparent_systemStart (log), $genesis_systemStart (genesis)"
             jq_fmutate "$dir"/genesis-shelley.json '. *
               { systemStart: $systemStart
               }
               ' --arg systemStart $apparent_systemStart
        else msg "Good: both genesis and log-implied systemStart are at:  $genesis_systemStart"
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

    allocate-from-machine-run-slice | alloc-from-mrs )
        local usage="USAGE: wb run $op MACH RUN-SLICE-ID PRESET"
        local mach=${1:?$usage}; shift
        local run_slice_id=${1:?$usage}; shift
        local preset=${1:?$usage}; shift

        local args=(
            --arg id     $run_slice_id
            --arg mach   $mach
            --arg preset $preset
        )
        local meta=$(jq '
            { Y:  $id[0:4], M:   $id[4:6], D: $id[6:8]   }     as $d
          | { h: $id[8:10], m: $id[10:12], s: $id[12:14] }     as $t
          | { tag:     "\($d.Y)-\($d.M)-\($d.D)-\($t.h).\($t.m).\($mach)"
            , profile: $preset
            , date:    "\($d.Y)-\($d.M)-\($d.D)T\($t.h):\($t.m):\($t.s)Z"
            , batch:   $mach
            }
            | . +
              { timestamp: (.date | fromdateiso8601)
              }
          | { meta: . }
          ' "${args[@]}" --null-input)
        local tag=$(jq '.meta.tag' -r <<<$meta)
        local dir="$global_rundir"/$tag

        mkdir -p "$dir"/$mach
        local genesis=$(profile preset-get-file "$preset" 'genesis' 'genesis/genesis-shelley.json')
        cp -f  "$genesis" "$dir"/genesis-shelley.json
        ln -sf genesis-shelley.json "$dir"/genesis.json
        jq <<<$meta '
              $gsisf[0] as $gsis
            | . *
            { meta:
              { profile_content:
                { genesis:
                  { active_slots_coeff: $gsis.activeSlotsCoeff
                  , delegators:         1000000
                  , dense_pool_density: 1
                  , epoch_length:       $gsis.epochLength
                  , parameter_k:        $gsis.securityParam
                  , max_block_size:     $gsis.protocolParams.maxBlockBodySize
                  , max_tx_size:        $gsis.protocolParams.maxTxSize
                  , n_pools:            1
                  , slot_duration:      $gsis.slotLength
                  , utxo:               4000000
                  }
                , generator:
                  { add_tx_size:        0
                  , inputs_per_tx:      1
                  , outputs_per_tx:     1
                  , tps:                8
                  , tx_count:           0
                  , era:                "alonzo"
                  }
                }
              }
            }
           ' --slurpfile gsisf "$dir"/genesis-shelley.json > "$dir"/meta.json

        echo $dir;;

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

    fetch-analysis | fa )
        local usage="USAGE: wb run $op ENV DEPL BATCH-OR-TAG.."
        local   env=${1:?$usage}; shift
        local  depl=${1:?$usage}; shift

        for x in $*
        do
            ssh $env -- \
                sh -c "'cd $depl/runs && tar c {*.$x.*,$x}/analysis/{block-propagation,logs-node-1.timeline}.txt --zstd --ignore-failed-read'" 2>/dev/null |
                (cd run; tar x --zstd); done
        ;;

    remote-machine-run-slice-list | rmrsl )
        local usage="USAGE: wb run $op ENV DEPL [HOST=DEPL]"
        local env=${1:?$usage}
        local depl=${2:?$usage}
        local host=${3:-$2}

        local nixops_ssh_cmd="ls /var/lib/cardano-node/logs/node-*.json"
        local nixops_cmd="nixops ssh -d $depl $host -- $nixops_ssh_cmd"
        ssh $env -- \
            sh -c "cd $depl && nix-shell -p nixops --run ${nixops_cmd@Q}" |
            sed 's_/var/lib/cardano-node/logs/node-\(.*\).json_\1_'
        ;;

    remote-machine-run-slice-fetch | rmrsf )
        local usage="USAGE: wb run $op ENV DEPL MACHINE SLICE"
        local env=${1:?$usage}
        local depl=${2:?$usage}
        local mach=${3:?$usage}
        local slice=${4:?$usage}

        local dir=$(run allocate-from-machine-run-slice \
                        $mach $slice 'mainnet')
        local nixops_cmd="nixops scp -d $depl --from $mach /var/lib/cardano-node/logs/node-$slice.json $depl/node-$slice.json"
        ssh $env -- \
            sh -c "cd $depl && nix-shell -p nixops --run ${nixops_cmd@Q}"
        ssh $env -- \
            sh -c "cd $depl && tar c $depl/node-$slice.json --zstd" |
            (cd $dir; tar x --zstd)
        ;;

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
