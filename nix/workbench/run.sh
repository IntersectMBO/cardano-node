global_rundir_def=$(realpath ${WB_RUNDIR:-$PWD/run})

usage_run() {
    set +x
    usage "run" "Managing cluster runs" <<EOF
    $(helpcmd list-runs)              List local runs
     $(blk runs lsr)
    $(helpcmd list-remote)            List remote runs
     $(blk remote lsrr)
    $(helpcmd list-verbose)           List local runs, verbosely
     $(blk verb lsrv)
    $(helpcmd list-verbose-remote)    List remote runs, verbosely
     $(blk rverb lsrvr)
    $(helpcmd list-sets)              List local run sets
     $(blk sets lss)
    $(helpcmd list-sets-remote)       List remote cluster run sets
     $(blk rsets lssr)
    $(helpcmd set-add SETNAME RUN...)
     $(blk add sa)                  Add runs to the named run set
    $(helpcmd run-or-set)             Resolve a run-or-set name to a list of runs
     $(blk ros)
    $(helpcmd list-pattern)           List local runs
     $(blk lspat lsrp)
    $(helpcmd as-remote)            Helper to modify remote environment in $(blue \$WB_REMOTE)
     $(blk asr)

    $(helpcmd describe RUN)

    $(helpcmd fix-legacy-run-structure RUN)
     $(blk flrs fix-legacy)      Update legacy (cardano-ops) meta.json to mostly match
                            the workbench

    $(helpcmd allocate BATCH-NAME PROFILE-NAME [ENV-CONFIG-OPTS..])
                          Allocate a cluster run with the specified:
                            - batch key (no semantics attached)
                            - profile name
                          A unique name would be allocated for this run,
                            and a run alias $(green current) will be created for it.

    $(helpcmd fetch-run RUN)        Fetch a remote run
     $(blk fr fetch)
    $(helpcmd fetch-analysis RUN..)
     $(blk fa)                   Fetch analyses of remote runs
    $(helpcmd analyse-remote RUN..)  Run analyses of runs, remotely

    $(helpcmd start [--scenario NAME] [--idle] [--analyse] RUN)
                          Start the named run.
                            --scenario forces a scenario, different from what
                                is specified by the run profile;
                                See 'wb scenario --help' for scenario descriptions
                            --idle is a short-cut for '--scenario idle'
                            --analyse triggers analysis after 'start' returns

    $(helpcmd stop RUN)              Stop the named run

    $(helpcmd restart [--no-generator] BACKEND-START-ARGS..)
                          Stop and restart the current run (without a new allocation)

  $(red run) $(blue options):

    $(helpopt --rundir DIR)          Set the runs directory.  Defaults to $(green $global_rundir_def),
                            if it exists, otherwise to $(blue \$WB_RUNDIR), if that
                            exists, and finally unconditionally to $(green $global_rundir_def)
EOF
}

run_default_op='list-runs'

run() {
set -eu
if   test -v "WB_RUNDIR" && test -d "$WB_RUNDIR"
then global_rundir=$WB_RUNDIR
## Allow compatibility with cardano-ops legacy runs directory layout:
elif test -d "$global_rundir_def"
then global_rundir=$global_rundir_def
else global_rundir=$global_rundir_def
     mkdir "$global_rundir"
fi

if test -v "WB_REMOTE"
then remote=$WB_REMOTE
else remote='{"env":"deployer","depl":"nomad-1"}'            # nomad/nomad-1 as fallback default
fi

local sargs=()

run_remote_get_args=()
while test $# -gt 0
do case "$1" in
       --remote )      sargs+=($1 $2); remote=$2; shift;;
       --rundir )      sargs+=($1 $2); global_rundir=$2; shift;;
       --clean | -c )  sargs+=($1);    run_remote_get_args+=($1);;
       * ) break;; esac; shift; done

local op=${1:-$run_default_op}; test $# -gt 0 && shift

case "$op" in
    get-global-rundir )
        realpath --relative-to "$(pwd)" "$global_rundir";;

    as-remote | asr )
        local usage="USAGE: wb run $op <environment> <deployment> [rundir]"
        if [[ -z ${1+x} || -z ${2+x} ]]
        then
            echo $usage
            echo "where: <environment> is a Host from your '~/.ssh/config' or a literal like 'user@1.2.3.4'"
            echo "        <deployment> is a cardano-node or cardano-ops checkout in the environment user's home dir"
            echo "            [rundir] subdirectory containing runs; necessary only when differing from default 'run'"
        else
            remote=$(jq -cn --arg env $1 --arg depl $2 --arg dir ${3:-''} '{env: $env, depl: $depl} | if $dir=="" then . else . * {dir: $dir} end')
            echo "to modify remote in current shell environment, please execute:"
            echo $(yellow "export WB_REMOTE='$remote'")
        fi;;

    list-runs | runs | lsr )
        local usage="USAGE: wb run $op [--remote | -r]"
        local on_remote=
        while test $# -gt 0
        do case "$1" in
               --remote | -r ) on_remote='true';;
               * ) msg "FATAL:  list, unknown flag '$1'"; usage_run;;
           esac; shift; done

        if test -z "$on_remote"
        then (eval "$(run_ls_cmd "$global_rundir")")
        else
            local r=${1:-$remote}
            local env=$( jq <<<$r '.env' -r)
            local depl=$(jq <<<$r '.depl' -r)
            local dir=$( jq <<<$r '.dir // "run"' -r)
            ssh $env \
                sh -c "'$(run_ls_cmd $depl/$dir)'"
        fi;;

    list-remote | remote | lsrr ) ## Convenience alias for 'list'
        run list-runs --remote;;

    list-verbose | verb | lsrv )
        local usage="USAGE: wb run $op [--remote | -r] [--limit [N=10] | -n N]"
        local on_remote= limit=20
        while test $# -gt 0
        do case "$1" in
               --remote | -r ) on_remote='true';;
               --limit | -n )           limit=$2; shift;;
               * ) msg "FATAL:  list-verbose, unknown flag '$1'"; usage_run;;
           esac; shift; done

        if test -z "$on_remote"
        then (eval "$(run_ls_tabulated_cmd "$global_rundir" $limit)")
        else
            local r=${1:-$remote}
            local env=$( jq <<<$r '.env' -r)
            local depl=$(jq <<<$r '.depl' -r)
            local dir=$( jq <<<$r '.dir // "run"' -r)
            ssh $env \
                sh -c "'$(run_ls_tabulated_cmd $depl/$dir $limit)'"
        fi;;

    list-verbose-remote | rverb | lsrvr ) ## Convenience alias for 'list-verbose'
        run list-verbose --remote;;

    list-sets | sets | lss )
        local usage="USAGE: wb run $op [--remote | -r]"
        local on_remote=
        while test $# -gt 0
        do case "$1" in
               --remote | -r ) on_remote='true';;
               * ) msg "FATAL:  list-sets, unknown flag '$1'"; usage_run;;
           esac; shift; done

        if test -z "$on_remote"
        then (eval "$(run_ls_sets_cmd "$global_rundir")")
        else
            local r=${1:-$remote}
            local env=$( jq <<<$r '.env' -r)
            local depl=$(jq <<<$r '.depl' -r)
            local dir=$( jq <<<$r '.dir // "run"' -r)
            ssh $env \
                sh -c "'$(run_ls_sets_cmd $depl/$dir)'"
        fi;;

    list-sets-remote | rsets | lssr ) ## Convenience alias for 'list-sets'
        run list-sets --remote;;

    set-add | add | sa )
        local usage="USAGE: wb run $op NAME [RUN..]"
        local name=${1:?$usage}; shift
        mkdir -p "$global_rundir/.sets/$name" &&
            (cd  "$global_rundir/.sets/$name"
             for x in $*
             do if ! run get $x >/dev/null
                then fail "set-add $name:  constituent run missing: $(white $x)"
                fi
                ln -s ../../$x
             done);;

    run-or-set | ros )
        local usage="USAGE: wb run $op [--query] [--remote | -r] NAME"
        local query= get_args=() on_remote=
        while test $# -gt 0
        do case "$1" in
               --try | --query ) get_args+=($1); query='true';;
               --remote | -r ) on_remote='true';;
               --* ) msg "FATAL:  run-or-set, unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done

        local name=${1:?$usage}
        local env=$( jq <<<$remote '.env' -r)
        local depl=$(jq <<<$remote '.depl' -r)
        local dir=$( jq <<<$remote '.dir // "run"' -r)
        if   test -n "$on_remote"
        then if test -n "$(ssh $env -- sh -c "'$(run_ls_sets_cmd $depl/$dir)'" |
                                       grep $name || true)"
             then rsync -Wa --delete-after \
                      $env:$depl/$dir/.sets/$name ../cardano-node/run/.sets
                  ssh $env -- \
                      sh -c "'cd $depl/$dir/.sets/$name && find . -type l | cut -d/ -f2'"
             else ssh $env -- \
                      sh -c "'if test -f $depl/$dir/$name/meta.json;
                              then echo $name;
                              else echo \"$(red run-or-set on $env/$depl:)  missing run or set $(white $name)\"
                              exit 1;
                              fi'"
             fi
        elif test -n "$(run list-sets | grep $name || true)"
        then (cd     "$global_rundir/.sets/$name"
              find . -type l | cut -d/ -f2)
        elif run get "${get_args[@]}" $name >/dev/null
        then echo $name
        elif test -n "$query"
        then return 1
        else fail "run-or-set:  missing run or set $(white $name)"
        fi;;

    list-pattern | lspat | lsrp )
        test -d "$global_rundir" &&
            (cd "$global_rundir"
             ls $1/meta.json |
                 grep -v 'current$\|deploy-logs$' |
                 cut -d/ -f1 |
                 sort || true);;

    describe )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$(run get $run)

        if ! run check "$run"
        then fatal "run fails sanity checks:  $run at $dir"; fi

        cat <<EOF
  - run dir:         $dir
  - profile JSON:    $dir/profile.json
  - node specs:      $dir/node-specs.json
  - topology:        $dir/topology/topology.pdf
  - node base port:  $(envjq 'basePort')
EOF
        backend describe-run "$dir"
        ;;

    fix-legacy-run-structure | fix-legacy | flrs )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$(run compute-path "$run")

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

        progress "run | fix-legacy-run-structure" "adding manifest"
        jq_fmutate "$dir"/meta.json '
           .meta.manifest = $manifest
           ' --argjson manifest "$(legacy_run_manifest $dir)"

        progress "run | fix-legacy-run-structure" "adding timing"
        jq_fmutate "$dir"/meta.json '
           .meta.timing = $timing
           ' --argjson timing "$(legacy_run_timing $dir)"

        jq ' .meta.profile_content
           | .analysis.filters += ["model"]
           | .node.tracing_backend =
               (if .node.withNewTracing
                then "trace-dispatcher"
                else "iohk-monitoring"
                end)
           ' "$dir"/meta.json > "$dir"/profile.json;;

    compute-path )
        if test -f "$1/meta.json"
        then echo -n "$1"
        else realpath --relative-to "$(pwd)" "$global_rundir/$1"
        fi;;

    check )
        local usage="USAGE: wb run $op [--query] RUN"
        local query=
        while test $# -gt 0
        do case "$1" in
               --try | --query ) query='true';;
               --* ) msg "FATAL:  run-or-set, unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done

        local run=${1:?$usage}
        local dir=$(run compute-path "$run")

        if ! jq_check_json "$dir"/meta.json 2>/dev/null
        then if test $run = 'current'
             then local alt=$(run list | tail -n1)
                  progress 'run | check' "$(with_color white current) missing, resetting to $(with_color white $alt)"
                  run set-current $alt
             elif test -n "$query"
             then return 1
             else fatal "run $run (at $dir) missing a file:  meta.json"; fi; fi

        test -f "$dir"/profile.json -a -f "$dir"/genesis-shelley.json ||
            run fix-legacy-run-structure "$run";;

    fix-systemstart )
        local usage="USAGE: wb run $op RUN [MACH=node-1]"
        local run=${1:?$usage}
        local mach=${2:-node-0}
        local dir=$(run compute-path "$run")
        local nodelog=$(ls $dir/logs/$mach/node-*.json | head -n1)
        local genesis=$dir/genesis-shelley.json

        msg "cross-checking systemStart of $run:  $nodelog"
        local apparent_systemStart=$(grep -F 'TraceStartLeadershipCheck' $nodelog |
                                     head -n2 |
                                     tail -n1 |
                                     jq '[ (.at | "\(.[:19])Z" | fromdateiso8601)
                                         , .data.slot
                                         ] | .[0] - .[1]
                                           | todateiso8601' -r)
        local genesis_systemStart=$(jq .systemStart $genesis -r)

        if test "$genesis_systemStart" != "$apparent_systemStart"
        then msg "Fixing genesis systemStart in $run:  $apparent_systemStart (log), $genesis_systemStart (genesis)"
             jq_fmutate "$dir"/genesis-shelley.json '. *
               { systemStart: $systemStart
               }
               ' --arg systemStart $apparent_systemStart
        else msg "Good: both genesis and log-implied systemStart are at:  $genesis_systemStart"
        fi;;

    get-path | get )
        local usage="USAGE: wb run $op [--query] RUN"
        local check_args=()
        while test $# -gt 0
        do case "$1" in
               --try | --query ) check_args+=($1);;
               --* ) msg "FATAL:  get-path, unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done

        local runspec=${1:?$usage}
        local nrun=$(runspec_normalise $runspec)
        local run=$(runspec_run $nrun)

        if   run check "${check_args[@]}" "$run"
        then run compute-path             "$run"
        else return 1
        fi;;

    set-identifier | setid )
        local usage="USAGE: wb run $op [RUN IDENT]*"

        while test $# -gt 0
        do local run=${1:?$usage}
           local ident=${2:?$usage}

           local dir=$(run compute-path "$run")
           test -n "$dir" || fail "malformed run: $run"

           progress "analyse" "setting run identifier to:  $(white $ident), was $(blue $(jq -r .meta.ident "$dir"/meta.json))"

           ## XXX: ugly duplication.
           jq_fmutate "$dir"/meta.json \
             '.meta.ident = $ident' --arg ident "$ident"
           jq_fmutate "$dir"/analysis/summary.json \
             '.sumMeta.ident = $ident' --arg ident "$ident"

           shift 2
        done;;

    ## It's quite messy, semantically: set(when-specified)-and-get,
    decide-identifier | decid )
        local usage="USAGE: wb run $op (RUN | ID:RUN)"
        local runspec=${1:?$usage}
        local nrun=$(runspec_normalise $runspec)
        local run=$(runspec_run $nrun)  ident=$(runspec_id $nrun)
        local dir=$(run compute-path "$run")
        if   test          -n "$ident"
        then run setid "$run" "$ident"
             echo              $ident
        else jq -r .meta.ident "$dir"/meta.json
        fi;;

    show-meta | show | meta | s )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}

        jq '.' "$(run get "$run")"/meta.json;;

    set-current | set )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$(run get "$run")

        rm -f      "$global_rundir"/{current,-current}
        ln -s $run "$global_rundir"/-current
        ln -s $run "$global_rundir"/current;;

    current-run-path | current-path | path )
        realpath --relative-to "$(pwd)" "$global_rundir"/current;;

    current-run-tag | current-tag | tag | current )
        basename "$(run current-path)";;

    current-run-meta | current-meta | meta )
        jq '.' "$(run current-path)"/meta.json;;

    current-run-profile | current-profile | profile | p )
        jq '.' "$(run current-path)"/profile.json;;

    allocate )
        local usage="USAGE: wb run $op BATCH-NAME PROFILE-NAME [ENV-CONFIG-OPTS..] [-- BACKEND-ARGS-AND-ENV-CONFIG-OPTS..]"
        local batch=${1:?$usage}; shift
        local profile_name=${1:?$usage}; shift
        local backend_name=${1:?$usage}; shift

        local profile_data= genesis_cache_entry= manifest=
        while test $# -gt 0
        do case "$1" in
               --manifest )            manifest=$2; shift;;
               --profile-data )        profile_data=$2; shift;;
               --genesis-cache-entry ) genesis_cache_entry=$2; shift;;
               -- ) shift; break;;
               --* ) msg "FATAL:  unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done
        local backend_args=("$@")

        ## 1. genesis cache entry:
        progress "run | genesis" "cache entry:  $(if test -n "$genesis_cache_entry"; then echo pre-supplied; else echo preparing a new one..; fi)"
        if test -z "$genesis_cache_entry"
        then genesis_cache_entry=$(
                 genesis prepare-cache-entry \
                  "$profile_data"/profile.json \
                  "$profile_data"/node-specs.json)
        fi

        ## 2. decide the tag:
        ##    Must match `^[a-zA-Z0-9-]{1,128}$)` or it won't be possible to use
        ##    it as a Nomad Namespace or Nomad Job name.
        ##    NOTE: The tag time is different from the genesis time
        local hash=$(jq '."cardano-node".commit | .[:5]' -r <<<$manifest)
        local date_pref=$(date --utc +'%Y-%m-%d'-'%H-%M')
        if [[ "$batch" == "undefined" ]]
        then batch=$(get_most_significant_git_tag)
        fi
        local batch_inf=$(echo -n ${batch} | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z^0-9]//g')
        local prof_suf=$(test -v "WB_PROFILING" && test -n "$WB_PROFILING" -a "$WB_PROFILING" != 'none' && echo '-prof')
        local run="${date_pref}-${hash}-${batch_inf::12}-${profile_name}-${backend_name::3}${prof_suf}"
        progress "run | tag" "allocated run identifier (tag):  $(with_color white $run)"

        ## 3. create directory:
        local dir=$(realpath --relative-to "$(pwd)" "$global_rundir/$run")
        local realdir=$(realpath --canonicalize-missing "$dir")

        test "$(dirname "$realdir")" = "$(realpath "$global_rundir")" ||
            fatal "profile | allocate bad tag/run dir:  $run @ $dir"
        test ! -e "$dir" ||
            fatal "profile | allocate tag busy:  $run @ $dir"
        mkdir -p "$dir"/flag && test -w "$dir" ||
            fatal "profile | allocate failed to create writable run directory:  $dir"

        ## 4. populate the directory with files shared by all backends:
        progress "run | profile" "$(if test -n "$profile_data"; then echo "pre-supplied ($profile_name):  $profile_data"; else echo "computed:  $profile_name"; fi)"
        if test -n "$profile_data"
        then
            test "$(jq -r .name $profile_data/profile.json)" = "$profile_name" ||
                fatal "profile | allocate incoherence:  --profile-data $profile_data/profile.json mismatches '$profile_name'"
            ln -s "$profile_data"                 "$dir"/profile
            if test -n "${WB_PROFILE_OVERLAY:-}"
            ## Allow 'wb' pick up the profile overlay in 'profiles.jq':
            then wb profile
            else jq . "$profile_data"/profile.json
            fi > "$dir"/profile.json
            progress "profile | overlay" "$(white $(jq .overlay "$dir"/profile.json))"
            cp    "$profile_data"/node-specs.json "$dir"/node-specs.json
        else
            fail "Mode no longer supported:  operation without profile/ directory."
        fi

        local ghc_version=$(get_node_ghc_version)

        progress "run | topology" \
                 "$(white $(jq -r .composition.topology "$dir"/profile.json))"
        ln -s "$profile_data"/topology.json        "$dir"
        ln -s "$profile_data"/topology.dot         "$dir"

        ## 5. backend specifics allocations:
        backend allocate-run "$dir" "${backend_args[@]}"

        ## 6. to be able to deploy the genesis the cluster must be started
        #     because Nomad uses `nomad exec wget`
        # FIXME: the problem is that supervisor uses "run/current" in its
        #        configuration and a "meta.json" is needed to pass `run check`
        #        that is called by `run set-current`. This error is only showing
        #        a "FATAL" message and not exiting by pure chance (bash error
        #        supression because of a command called inside `local`). So
        #        we ignore it as a temporary solution because all the bash
        #        cleaup needed to fix this will take too long.
        run set-current "$run" 2>/dev/null || true
        backend start-cluster "$dir"

        ## 7. allocate genesis time
        ##    NOTE: The genesis time is different from the tag time.
        progress "run | time" "allocating time:"
        local timing=$(profile allocate-time "$dir"/profile.json)
        profile describe-timing "$timing"

        local args=(
            --arg       run              "$run"
            --arg       batch            "$batch"
            --arg       profile_name     "$profile_name"
            --arg       ghc_version      "$ghc_version"
            --argjson   timing           "$timing"
            --slurpfile profile_content  "$dir"/profile.json
            --argjson   manifest         "$manifest"
        )
        jq_fmutate "$dir"/meta.json '. *
           { meta:
             { tag:              $run
             , batch:            $batch
             , profile:          $profile_name
             , node_ghc_version: $ghc_version
             , timing:           $timing
             , manifest:         $manifest
             , profile_overlay:  $profile_content[0].overlay
             , profile_content:  $profile_content[0]
             }
           }
           ' "${args[@]}"

        progress "run | genesis" "deriving from cache"
        if test -z "$genesis_cache_entry"
        then fail "internal error:  no genesis cache entry"
        else genesis derive-from-cache      \
                     "$dir"/profile.json    \
                     "$timing"              \
                     "$genesis_cache_entry" \
                     "$dir"/genesis
        fi
        ## Record geneses
        cp "$dir"/genesis/genesis-shelley.json "$dir"/genesis-shelley.json
        cp "$dir"/genesis/genesis.alonzo.json  "$dir"/genesis.alonzo.json
        echo >&2

        ## 8. deploy genesis
        progress "run | genesis" "deploying.."
        backend deploy-genesis "$dir"

        ## 9. everything needed to start-[tracers|nodes|generator] should be
        ##    ready
        progress "run" "allocated $(with_color white $run) @ $dir"
        run     describe "$run"
        profile describe "$dir"/profile.json
        ;;

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
        local run=$(jq '.meta.tag' -r <<<$meta)
        local dir="$global_rundir"/$run

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

    fetch-run | fetch | fr )
        local usage="USAGE: wb run $op RUN [MACHINE]"
        local run=${1:?$usage}
        local mach=${2:-all-hosts}

        local env=$( jq <<<$remote '.env' -r)
        local depl=$(jq <<<$remote '.depl' -r)
        local dir=$( jq <<<$remote '.dir // "run"' -r)

        local args=(
            "$env"
            "$depl"
            "$dir"
            "$run"
            'for f in ${files[*]};
             do fp=${f/%/.tar.zst};
                fpp=${fp/#/compressed/logs-};
                if test -f $fpp;
                then cat $fpp;
                else tar c $f --zstd --ignore-failed-read;
                fi;
             done;
             '

            common-run-files
            $mach
        )
        run_remote_get "${args[@]}";;

    fetch-analysis | fa )
        local usage="USAGE: wb run $op RUN.."
        local runs=() run
        for rs in $*
        do runs+=($(run "${sargs[@]}" run-or-set --query --remote $rs || echo $rs))
        done
        if test $# = 0; then runs=(current); fi

        local env=$( jq <<<$remote '.env' -r)
        local depl=$(jq <<<$remote '.depl' -r)
        local dir=$( jq <<<$remote '.dir // "run"' -r)

        progress "run | remote" "trying to fetch analyses:  $(white ${runs[*]})"
        for run in ${runs[*]}
        do if   test "$(ssh $env -- sh -c "'ls -ld $depl/$dir/$run          | wc -l'")" = 0
           then fail "fetch-analysis:  run does not exist on remote: $(white $run)"
           elif test "$(ssh $env -- sh -c "'ls -ld $depl/$dir/$run/analysis | wc -l'")" = 0
           then fail "fetch-analysis:  run has not been analysed on remote: $(white $run)"
           else local analysis_files=(
                   $(ssh $env -- \
                     sh -c "'cd $depl/$dir/$run && ls analysis/{cdf/*.cdf,*.{json,org,txt}} | fgrep -v -e flt.json -e flt.logobjs.json -e flt.perf-stats.json'" \
                     2>/dev/null)
                )
                local args=(
                   "${run_remote_get_args[@]}"
                   "$env"
                   "$depl"
                   "$dir"
                   "$run"
                   'tar c ${files[*]} --zstd;'

                   common-run-files
                   ${analysis_files[*]}
                )
                run_remote_get "${args[@]}"
           fi
        done
        ;;

    analyse-remote )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}

        local env=$( jq <<<$remote '.env' -r)
        local depl=$(jq <<<$remote '.depl' -r)
        local dir=$( jq <<<$remote '.dir // "run"' -r)

        if   test "$(ssh $env -- sh -c "'ls -ld $depl/$dir/$run          | wc -l'")" = 0
        then fail "analyse-remote:  run does not exist on remote: $(white $run)"
        else ssh $env -- sh -c "'export WB_RUNDIR=../$depl/$dir && cd cardano-node && echo env: $(yellow $env), rundir: $(color blue)\$WB_RUNDIR$(color reset), workbench: $(color yellow)\$(git log -n1)$(color reset) && make analyse RUN=$run'"
        fi
        ;;

    list-hosts | hosts )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$global_rundir/$run

        if test -f "$dir"/node-specs.json
        then jq             'keys | .[]' -r "$dir"/node-specs.json
        else jq '.hostname | keys | .[]' -r "$dir"/meta.json; fi;;

    trim )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$global_rundir/$run
        local genesis="$dir"/genesis-shelley.json
        local geneses_orig_dir=$global_rundir/.geneses.orig
        local genesis_orig="$geneses_orig_dir"/$run.orig.json

        if ! run get $run >/dev/null
        then fail "trim" "missing run: $(white $run)"
        fi

        mkdir -p "$geneses_orig_dir" "$dir"/genesis/

        local size=$(ls -s "$genesis" | cut -d' ' -f1)
        if test "$size" -gt 1000
        then progress "run" "genesis size: ${size}k, trimming.."
             mv    "$genesis" "$genesis_orig"
             ln -s "$(realpath $genesis_orig)" "$genesis".orig
             jq > "$genesis" '
               .initialFunds = {}
             | .staking      = {}
             ' "$genesis_orig"; fi
        cp -f "$dir"/genesis-shelley.json "$dir"/genesis/genesis-shelley.json;;

    package | pack )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}

        run trim "$run"
        ( cd $global_rundir
          tar c --zstd --exclude '*/*/*.socket' "$run" > $run.tar.zst
        );;

    compat-meta-fixups | compat-f )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$(run get "$run")

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
        local usage="USAGE: wb run $op [--idle] [--scenario NAME] [--analyse] RUN"

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

        local run=${1:-?$usage}; shift
        local dir=$(run get "$run")
        test -d "$dir" ||
            fatal "invalid run identifier: $run"

        progress "run" "starting $(with_color white $run)"

        run set-current "$run"

        ## Execute the scenario
        local scenario=${scenario_override:-$(jq -r .scenario "$dir"/profile.json)}
        scenario "$scenario" "$dir"

        backend fetch-logs     "$dir"
        backend stop-cluster   "$dir"

        run compat-meta-fixups "$run"
        ;;

    stop )
        local run=${1:-current}
        local dir=$(run get "$run")

        local running_components=($(backend is-running "$dir"))
        if test ${#running_components[*]} -gt 0
        then progress "run" "terminating backend components ($(red ${running_components[*]})).."
             backend stop-cluster "$dir"
             progress "run" "cluster stopped"
        fi
        ;;

    restart )
        local run=$(run current-tag)
        local dir=$(run get "$run")

        test -d "$dir" ||
            fatal "no valid current run to restart:  please set run/current appropriately"

        progress "run" "restarting cluster in the same run directory: $dir"

        run stop                "$run"
        jq_fmutate "$dir"/meta.json '
          { modifiers: { wiped_and_restarted: true }
          } * .
        '
        backend cleanup-cluster "$dir"
        run start-cluster  "$@" "$run"

        msg "cluster re-started in the same run directory: $dir"
        ;;

    * ) usage_run;; esac
}

run_remote_get() {
    local usage='USAGE: run_remote_get ENV DEPLOYMENT RUN REMOTE-TAR-CMD OBJ..'
    local clean=
    while test $# -gt 0
    do case "$1" in
           --clean | -c ) clean='true';;
           -- ) shift; break;;
           --* ) msg "FATAL:  unknown flag '$1'"; fail "$usage";;
           * ) break;; esac; shift; done

    local env=${1:?$usage}; shift
    local depl=${1:?$usage}; shift
    local rdir=${1:?$usage}; shift
    local run=${1:?$usage}; shift
    local remote_tar_cmd=${1:?$usage}; shift
    local objects=($*)

    progress "run_remote_get" "env $(yellow $env) depl $(yellow $depl) run $(white $run)"
    progress "run_remote_get" "tar $(green $remote_tar_cmd)"

    local meta=$(ssh $env -- sh -c "'jq . $depl/$rdir/$run/meta.json'")
    if ! jq . <<<$meta >/dev/null
    then fail "run_remote_get:  malformed $(yellow meta.json) in $(white $run) on $(white $depl)@$(white env)"; fi

    ## Minor validation passed, create & populate run with remote data:
    local dir=$global_rundir/$run
    if test -z "$run" -o -z "$global_rundir"
    then fail "run_remote_get: run=$run global_rundir=$global_rundir"
    elif test -n "$clean"
    then rm -rf "$dir"
    fi
    mkdir -p "$dir"
    jq . <<<$meta > $dir/meta.json

    local common_run_files=(
        genesis-alonzo.json
        genesis-shelley.json
        machines.json
        network-latency-matrix.json
        profile.json
    )
    local xs0=(${objects[*]})
    local xs1=(${xs0[*]/#all-hosts/        $(jq -r '.hostname | keys | .[]' <<<$meta)})
    local xs2=(${xs1[*]/#common-run-files/ ${common_run_files[*]}})
    local xs=(${xs2[*]})

    local count=${#xs[*]}
    progress "run | fetch $(white $run)" "objects to fetch:  $(white $count) total:  ${objects[*]}"

    local max_batch=9 base=0 batch
    while test $base -lt $count
    do local batch=(${xs[*]:$base:$max_batch})
       {
           local lbatch=(${batch[*]})
           ssh $env -- \
               sh -c "'files=(${lbatch[*]}); cd $depl/$rdir/$run && { ${remote_tar_cmd} }'" |
               (cd $dir
                tar x --zstd --ignore-zeros ||
                    progress "fetch error" "'files=(${lbatch[*]}); cd $depl/$rdir/$run && ${remote_tar_cmd}'"
               )
           progress "run | fetch $(white $run)" "batch done:  $(yellow ${batch[*]})"
       } &
       sleep 1
       base=$((base + max_batch))
    done
    progress "run | fetch $(white $run)" "batches started, waiting.."
    wait

    progress "run | fetch" "adding manifest"
    jq_fmutate "$dir"/meta.json '.meta.manifest = $manifest
    ' --argjson manifest "$(legacy_run_manifest $dir)"
}

node_cabal_source_at() {
    # FIXME
    echo 0123456789ABCDEF0123456789ABCDEF01234567
}

legacy_run_manifest() {
    local dir=$1
    local node=$(       jq -r '.meta.pins."cardano-node"' $dir/meta.json)
    local node_branch=$(jq -r '.meta.node_commit_spec'    $dir/meta.json)
    local node_ver=$(   jq -r '.meta.node_commit_desc'    $dir/meta.json)

    local args=(
      --arg Node          $node
      --arg NodeBranch    $node_branch
      --arg NodeApproxVer $node_ver
      --arg Network  $(node_cabal_source_at $node ouroboros-network)
      --arg Ledger   $(node_cabal_source_at $node cardano-ledger)
      --arg Plutus   $(node_cabal_source_at $node plutus)
      --arg Crypto   $(node_cabal_source_at $node cardano-crypto)
      --arg Base     $(node_cabal_source_at $node cardano-base)
      --arg Prelude  $(node_cabal_source_at $node cardano-prelude)
    )
    jq '
  { "cardano-node"         : $Node
  , "cardano-node-branch"  : $NodeBranch
  , "cardano-node-version" : $NodeApproxVer
  , "cardano-node-status"  : "unknown"
  , "ouroboros-network"    : $Network
  , "cardano-ledger"       : $Ledger
  , "plutus"               : $Plutus
  , "cardano-crypto"       : $Crypto
  , "cardano-base"         : $Base
  , "cardano-prelude"      : $Prelude
  }
  ' --null-input "${args[@]}"
}

legacy_run_timing() {
    local dir=$1
    local stamp=$(jq -r '.meta.timestamp' $dir/meta.json)

    local args=(
        --argjson stamp $stamp
    )
    jq '
    .meta.profile_content                                       as $prof
  | ($stamp + ($prof.generator.tx_count / $prof.generator.tps)) as $stamp_end
  |
  { future_offset:   "0 seconds"
  , start:           $stamp
  , shutdown_end:    $stamp_end
  , workload_end:    $stamp_end
  , earliest_end:    $stamp_end

  , start_tag:       .meta.tag[:16]
  , start_human:     ($stamp | todateiso8601)
  , systemStart:     ($stamp | todateiso8601)
  , shutdownTime:    ($stamp_end | todateiso8601)
  , workloadEndTime: ($stamp_end | todateiso8601)
  , earliestEndTime: ($stamp_end | todateiso8601)
  }' $dir/meta.json "${args[@]}"
}

expand_runsets() {
    local runs=() rs

    if test $# = 0
    then runs=(current)
    else for rs in $*
         do runs+=($(run run-or-set $rs))
         done
    fi
    echo ${runs[*]}
}

runspec_normalise() {
    local runspec=${1:?$usage}
    local precomma=$(cut -d: -f1 <<<$runspec) run= ident=

    if   test "${runspec::1}" = "/" -o \
              "${runspec::1}" = "." -o \
              "$precomma" = "$runspec"
    then ident="";        run=$runspec
    else ident=$precomma; run=$(cut -d: -f2 <<<$runspec)
    fi

    echo "$ident:$run"
}

runspec_id() {
    cut -d: -f1 <<<$1
}

runspec_run() {
    cut -d: -f2 <<<$1
}

run_ls_cmd() {
    local rundir=$1

    echo 'cd '$rundir' && \
          { find . -mindepth 2 -maxdepth 2 -type f -name meta.json -exec dirname \{\} \; |
            grep -v "current\$\|deploy-logs\$" &&
            find . -mindepth 3 -maxdepth 3 -type f -name *.ede -exec dirname \{\} \; |
            xargs -r dirname
          } |
          cut -c3- |
          sort || true'
}

# Will need to support new manifest format in a backwards compatible manner
run_ls_tabulated_cmd() {
    local rundir=$1 limit=$2

    echo 'cd '$rundir' && \
          find . -mindepth 2 -maxdepth 2 -type f -name meta.json -exec dirname \{\} \; |
          grep -v "current\$\|deploy-logs\$" |
          cut -c3- |
          sort |
          tail -n'$limit' |
          {
            printf "%-60s %-12s %7s %-15s %10s\n" \
                 "run tag" "identifier" "gitrev" "node version" "node branch"
            while read lst_tag; test -n "$lst_tag";
            do printf_args=(
               $(jq ".meta | .manifest as \$manif |
                      \"\\(.ident) \\(\$manif.\"cardano-node\" | .[:7]) \\(\$manif.\"cardano-node-version\") \\(\$manif.\"cardano-node-branch\")\"
                    " -r <$lst_tag/meta.json))
              printf "%-60s %-12s %7s %-15s %10s\n" \
                    $lst_tag ${printf_args[*]}
            done || true
          }'
}

run_ls_sets_cmd() {
    local rundir=$1

    echo 'cd '$rundir'/.sets 2>/dev/null && \
          find -L . -mindepth 3 -maxdepth 3 -type f -name meta.json -exec dirname \{\} \; |
          cut -d/ -f2 |
          sort -u || true'
}

get_node_ghc_version(){
    local node_executable

    if [[ $WB_BACKEND == nomad* ]]
    then
       node_executable=$(jq --raw-output '.containerPkgs."cardano-node"."nix-store-path"' $WB_BACKEND_DATA/container-specs.json)
       node_executable="$node_executable/bin/cardano-node"
    else
       node_executable=cardano-node
    fi

    if [[ -x $node_executable ]]
    then
        $node_executable +RTS --info | grep 'GHC version' | grep -E -o "([0-9]{1,2}[\.]){2}[0-9]{1,2}"
    else
        echo "unknown"
    fi
}

get_most_significant_git_tag(){
    local lasttag=$(git describe --tags --abbrev=0)
    local commhash=$(git rev-list -n 1 $lasttag)
    local oldesttag=$(git tag --sort=creatordate --points-at $commhash | head -n1)
    echo $oldesttag
}
