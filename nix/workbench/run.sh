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

    start [--scenario NAME] [--idle] [--analyse] TAG
                          Start the named run.
                            --scenario forces a scenario, different from what
                                is implied by the run profile;
                                See 'wb scenario --help' for scenario descriptions
                            --idle is a short-cut for '--scenario idle'
                            --analyse triggers analysis after 'start' returns

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

local op=${1:-list}; test $# -gt 0 && shift

case "$op" in
    list | ls )
        test -d "$global_rundir" &&
            (cd "$global_rundir"
             find . -mindepth 2 -maxdepth 2 -type f -name 'meta.json' -exec dirname \{\} \; |
                 grep -v 'current$\|deploy-logs$' |
                 cut -c3- |
                 sort || true);;

    compute-path )
        if test -f "$1/meta.json"
        then echo -n "$1"
        else echo -n "$global_rundir/$1"
        fi;;

    fix-legacy-run-structure | fix-legacy )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$(run compute-path "$tag")

        if test ! -f "$dir"/genesis-shelley.json
        then msg "fixing up genesis naming in:  $dir"
             mv "$dir"/genesis.json "$dir"/genesis-shelley.json; fi

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

        jq '.meta.profile_content' "$dir"/meta.json > "$dir"/profile.json;;

    check )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$(run compute-path "$tag")

        if ! jq_check_json "$dir"/meta.json
        then if test $tag = 'current'
             then local alt=$(run list | tail -n1)
                  progress 'run | check' "$(with_color white current) missing, resetting to $(with_color white $alt)"
                  run set-current $alt
             else fatal "run $tag (at $dir) missing a file:  meta.json"; fi; fi

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

    list-aws | lsaws )
        local usage="USAGE: wb run $op [DEPLOYMENT=bench-1] [ENV=bench]"
        local depl=${1:-bench-1}
        local env=${2:-bench}

        ssh $env -- \
            sh -c "'cd $depl/runs &&
                    find . -mindepth 2 -maxdepth 2 -type f -name 'meta.json' -exec dirname \{\} \; |
                    grep -v current\$\\|deploy-logs\$ |
                    cut -c3- |
                    sort ||
                    true'" 2>/dev/null;;

    allocate-from-aws | steal-from-aws | aws-get )
        local usage="USAGE: wb run $op RUN [MACHINE] [DEPLOYMENT=bench-1] [ENV=bench]"
        local run=${1:?$usage}
        local mach=${2:-}
        local depl=${3:-bench-1}
        local env=${4:-bench}

        local meta=$(ssh $env -- sh -c "'jq . $depl/runs/$run/meta.json'")
        if ! jq . <<<$meta >/dev/null
        then fail "allocate-from-aws:  malformed $(yellow meta.json) in $(white $run) on $(white $depl)@$(white env)"; fi

        ## Minor validation passed, create & populate run with remote data:
        local dir=$global_rundir/$run
        mkdir -p "$dir"
        jq . <<<$meta > $dir/meta.json

        local hosts=($(if test -n "$mach"; then echo $mach
                       else jq -r '.hostname | keys | .[]' <<<$meta; fi))
        local objects=(
            ${hosts[*]}
            genesis-shelley.json
            genesis-alonzo.json
            network-latency-matrix.json
            machines.json
        )

        local count=${#objects[*]}
        progress "run | aws-get $(white $run)" "objects to fetch:  $(white $count) total:  $(yellow ${objects[*]})"

        local max_batch=9
        progress "run | aws-get $(white $run)" "fetching in batches"

        local base=0 batch
        while test $base -lt $count
        do local batch=(${objects[*]:$base:$max_batch})
           progress_ne "run | aws-get $(white $run)" "fetching batch: "
           local obj=
           for obj in ${batch[*]}
           do { ssh $env -- \
                    sh -c "'cd $depl/runs/$run && if test -f compressed/logs-$obj.tar.zst; then cat compressed/logs-$obj.tar.zst; else tar c $obj --zstd --ignore-failed-read; fi'" 2>/dev/null |
                    (cd $dir; tar x --zstd)
                echo -ne " $(yellow $obj)" >&2
              } &
           done
           wait
           echo >&2
           base=$((base + max_batch))
        done;;

    allocate )
        local usage="USAGE: wb run $op BATCH-NAME PROFILE-NAME [ENV-CONFIG-OPTS..] [-- BACKEND-ARGS-AND-ENV-CONFIG-OPTS..]"
        local batch=${1:?$usage}; shift
        local profile_name=${1:?$usage}; shift

        local profile= topology= genesis_cache_entry= manifest= preset= cabal_mode=
        while test $# -gt 0
        do case "$1" in
               --manifest )            manifest=$2; shift;;
               --profile )             profile=$2; shift;;
               --topology )            topology=$2; shift;;
               --genesis-cache-entry ) genesis_cache_entry=$2; shift;;
               --cabal-mode | --cabal ) cabal_mode=t;;
               -- ) shift; break;;
               --* ) msg "FATAL:  unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done

        if profile has-preset "$profile"/profile.json
        then preset=$(profile json "$profile"/profile.json | jq '.preset' -r)
             progress "run" "allocating from preset '$preset'"
        else progress "run" "allocating a new one"
        fi

        ## 0. report software manifest
        progress "run | manifest" "component versions:"
        manifest report "$manifest"
        local hash=$(jq '."cardano-node" | .[:5]' -r <<<$manifest)

        ## 1. compute cluster composition
        local node_specs=$(profile node-specs "$profile"/profile.json)

        ## 2. genesis cache entry population
        progress "run | genesis" "cache entry:  $(if test -n "$genesis_cache_entry"; then echo pre-supplied; else echo preparing a new one..; fi)"
        if test  -z "$genesis_cache_entry"
        then local genesis_tmpdir=$(mktemp --directory)
             local cacheDir=$(envjqr 'cacheDir')
             mkdir -p "$cacheDir" && test -w "$cacheDir" ||
                     fatal "profile | allocate failed to create writable cache directory:  $cacheDir"
             local genesis_args=(
                 ## Positionals:
                 "$profile"/profile.json
                 "$cacheDir"/genesis
                 "$node_specs"
                 "$genesis_tmpdir"/genesis
             )
             genesis prepare-cache-entry "${genesis_args[@]}"
             genesis_cache_entry=$(realpath "$genesis_tmpdir"/genesis)
             rm -f "$genesis_tmpdir"/genesis
             rmdir "$genesis_tmpdir"
        fi

        ## 2. allocate time
        progress "run | time" "allocating time:"
        local timing=$(profile allocate-time "$profile"/profile.json)
        profile describe-timing "$timing"

        ## 3. decide the tag:
        local tag=$(jq '.start_tag' -r <<<$timing)$(if test "$batch" != 'plain'; then echo -n .$batch; fi).$hash.$profile_name$(test -z "$cabal_mode" && echo '.nix')
        progress "run | tag" "allocated run identifier (tag):  $(with_color white $tag)"

        ## 4. allocate directory:
        local dir=$global_rundir/$tag
        local realdir=$(realpath --canonicalize-missing "$dir")

        test "$(dirname "$realdir")" = "$(realpath "$global_rundir")" ||
            fatal "profile | allocate bad tag/run dir:  $tag @ $dir"
        test ! -e "$dir" ||
            fatal "profile | allocate tag busy:  $tag @ $dir"
        mkdir -p "$dir"/flag && test -w "$dir" ||
            fatal "profile | allocate failed to create writable run directory:  $dir"

        ## 5. populate the directory:
        progress "run | profile" "$(if test -n "$profile"; then echo "pre-supplied ($profile_name):  $profile"; else echo "computed:  $profile_name"; fi)"
        if test -n "$profile"
        then
            test "$(jq -r .name $profile/profile.json)" = "$profile_name" ||
                fatal "profile | allocate incoherence:  --profile $profile/profile.json mismatches '$profile_name'"
            ln -s "$profile"              "$dir"/profile
            cp    "$profile"/profile.json "$dir"/profile.json
        else
            profile has-profile          "$profile_name" ||
                fatal  "no such profile:  $profile_name"
            profile json                 "$profile_name" > "$dir"/profile.json
        fi
        jq '.' <<<$node_specs > "$dir"/node-specs.json

        local args=(
            --arg       tag              "$tag"
            --arg       batch            "$batch"
            --arg       profile_name     "$profile_name"
            --argjson   timing           "$timing"
            --slurpfile profile_content  "$dir"/profile.json
            --argjson   manifest         "$manifest"
        )
        jq_fmutate "$dir"/meta.json '. *
           { meta:
             { tag:              $tag
             , batch:            $batch
             , profile:          $profile_name
             , timing:           $timing
             , manifest:         $manifest
             , profile_content:  $profile_content[0]
             }
           }
           ' "${args[@]}"

        progress "run | topology"  "$(if test -n "$topology"; then echo pre-supplied; else echo computed; fi)"
        if test -n "$topology"
        then ln -s "$topology"                    "$dir"/topology
        else topology make    "$dir"/profile.json "$dir"/topology
        fi

        if      test -z "$genesis_cache_entry"
        then fail "internal error:  no genesis cache entry"

        else genesis derive-from-cache      \
                     "$profile"             \
                     "$timing"              \
                     "$genesis_cache_entry" \
                     "$dir"/genesis
        fi
        ## Record geneses
        cp "$dir"/genesis/genesis-shelley.json "$dir"/genesis-shelley.json
        cp "$dir"/genesis/genesis.alonzo.json  "$dir"/genesis.alonzo.json
        echo >&2

        local svcs=$profile/node-services.json
        for node in $(jq_tolist 'keys' "$dir"/node-specs.json)
        do local node_dir="$dir"/$node
           mkdir -p                                          "$node_dir"
           jq      '."'"$node"'"' "$dir"/node-specs.json   > "$node_dir"/node-spec.json
           cp $(jq '."'"$node"'"."config"'         -r $svcs) "$node_dir"/config.json
           cp $(jq '."'"$node"'"."service-config"' -r $svcs) "$node_dir"/service-config.json
           cp $(jq '."'"$node"'"."start"'          -r $svcs) "$node_dir"/start.sh
           cp $(jq '."'"$node"'"."topology"'       -r $svcs) "$node_dir"/topology.json
        done

        local gtor=$profile/generator-service.json
        gen_dir="$dir"/generator
        mkdir -p                              "$gen_dir"
        cp $(jq '."run-script"'     -r $gtor) "$gen_dir"/run-script.json
        cp $(jq '."service-config"' -r $gtor) "$gen_dir"/service-config.json
        cp $(jq '."start"'          -r $gtor) "$gen_dir"/start.sh
        ln -s          ../node-0/config.json  "$gen_dir"

        local trac=$profile/tracer-service.json
        trac_dir="$dir"/tracer
        mkdir -p                              "$trac_dir"
        cp $(jq '."tracer-config"'        -r $trac) "$trac_dir"/tracer-config.json
        cp $(jq '."nixos-service-config"' -r $trac) "$trac_dir"/nixos-service-config.json
        cp $(jq '."config"'               -r $trac) "$trac_dir"/config.json
        cp $(jq '."start"'                -r $trac) "$trac_dir"/start.sh

        backend allocate-run "$dir"

        progress "run" "allocated $(with_color white $tag) @ $dir"
        run     describe "$tag"
        profile describe "$dir"/profile.json
        run  set-current "$tag"
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

    trim )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$global_rundir/$tag

        if ! run check "$tag"
        then fatal "run fails sanity checks:  $tag at $dir"; fi

        jq_fmutate "$dir"/genesis-shelley.json '
           del(.initialFunds)
         | del(.staking)
         | .initialFunds = {}
         | .staking      = {}
        ';;

    describe )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$global_rundir/$tag

        if ! run check "$tag"
        then fatal "run fails sanity checks:  $tag at $dir"; fi

        cat <<EOF
  - run dir:         $dir
  - profile JSON:    $dir/profile.json
  - node specs:      $dir/node-specs.json
  - topology:        $dir/topology/topology.pdf
  - node base port:  $(envjq 'basePort')
EOF
        backend describe-run "$dir"
        ;;

    compat-meta-fixups | compat-f )
        local usage="USAGE: wb run $op TAG"
        local tag=${1:?$usage}
        local dir=$(run get "$tag")

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
                 }
               }';;

    start )
        local usage="USAGE: wb run $op [--idle] [--scenario NAME] [--analyse] TAG"

        local scenario_override= analyse=yes analysis_can_fail=
        while test $# -gt 0
        do case "$1" in
               --idle )              scenario_override='generic-idle';;
               --scenario | -s )     scenario_override=$2; shift;;
               --no-analysis )       analyse=;;
               --analysis-can-fail | -f )
                                     analysis_can_fail=t;;
               --* ) msg "FATAL:  unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done

        local tag=${1:-?$usage}; shift
        local dir=$(run get "$tag")
        test -d "$dir" ||
            fatal "invalid run tag: $tag"

        progress "run" "starting $(with_color white $tag)"

        run set-current "$tag"

        ## Execute the scenario
        local scenario=${scenario_override:-$(jq -r .scenario "$dir"/profile.json)}
        scenario "$scenario" "$dir"

        run compat-meta-fixups "$tag"
        ;;

    stop )
        local tag=${1:-current}
        local dir=$(run get "$tag")

        if backend is-running "$dir"
        then progress "run" "terminating.."
             backend stop-cluster "$dir"
             progress "run" "cluster stopped"
        fi
        ;;

    restart )
        local tag=$(run current-tag)
        local dir=$(run get "$tag")

        test -d "$dir" ||
            fatal "no valid current run to restart:  please set run/current appropriately"

        progress "run" "restarting cluster in the same run directory: $dir"

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
