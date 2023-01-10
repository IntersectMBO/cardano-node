global_rundir_def=$(realpath ${WB_RUNDIR:-$PWD/run})

usage_run() {
     usage "run" "Managing cluster runs" <<EOF
    $(helpcmd list-runs)              List local runs
     $(blk runs lsr)
    $(helpcmd list-remote)            List AWS runs
     $(blk remote lsrr)
    $(helpcmd list-verbose)           List local runs, verbosely
     $(blk verb lsrv)
    $(helpcmd list-verbose-remote)    List AWS runs, verbosely
     $(blk rverb lsrvr)
    $(helpcmd list-sets)              List local run sets
     $(blk sets lss)
    $(helpcmd list-sets-remote)       List AWS cluster run sets
     $(blk rsets lssr)
    $(helpcmd set-add SETNAME RUN...)
     $(blk add sa)                  Add runs to the named run set
    $(helpcmd run-or-set)             Resolve a run-or-set name to a list of runs
     $(blk ros)
    $(helpcmd list-pattern)           List local runs
     $(blk lspat lsrp)

    $(helpcmd describe RUN)

    $(helpcmd fix-legacy-run-structure RUN)
     $(blk flrs fix-legacy)      Update legacy (AWS) meta.json to mostly match
                            the workbench

    $(helpcmd allocate BATCH-NAME PROFILE-NAME [ENV-CONFIG-OPTS..])
                          Allocate a cluster run with the specified:
                            - batch key (no semantics attached)
                            - profile name
                          A unique name would be allocated for this run,
                            and a run alias $(green current) will be created for it.

    $(helpcmd fetch-run RUN)        Fetch an AWS run
     $(blk fr fetch)
    $(helpcmd fetch-analysis RUN..)
     $(blk fa)                   Fetch analyses of AWS runs
    $(helpcmd analyse-aws RUN..)     Run analyses of AWS runs, remotely

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

local sargs=()
local remote='{ "env":  "bench"
              , "depl": "bench-1"
              }'
run_aws_get_args=()
while test $# -gt 0
do case "$1" in
       --remote )      sargs+=($1 $2); remote=$2; shift;;
       --rundir )      sargs+=($1 $2); global_rundir=$2; shift;;
       --clean | -c )  sargs+=($1);    run_aws_get_args+=($1);;
       * ) break;; esac; shift; done

local op=${1:-$run_default_op}; test $# -gt 0 && shift

case "$op" in
    get-rundir )
        echo $global_rundir;;

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
        else local r=${1:-$remote}
             ssh $(jq <<<$r .env -r) -- \
                 sh -c "'$(run_ls_cmd $(jq <<<$r .depl)/runs)'"
        fi;;

    list-remote | remote | lsrr ) ## Convenience alias for 'list'
        run list-runs --remote;;

    list-verbose | verb | lsrv )
        local usage="USAGE: wb run $op [--remote | -r] [--limit [N=10] | -n N]"
        local on_remote= limit=10
        while test $# -gt 0
        do case "$1" in
               --remote | -r ) on_remote='true';;
               --limit | -n )           limit=$2; shift;;
               * ) msg "FATAL:  list-verbose, unknown flag '$1'"; usage_run;;
           esac; shift; done

        if test -z "$on_remote"
        then (eval "$(run_ls_tabulated_cmd "$global_rundir" $limit)")
        else local r=${1:-$remote}
             ssh $(jq <<<$r .env -r) -- \
                 sh -c "'$(run_ls_tabulated_cmd $(jq <<<$r .depl)/runs $limit)'"
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
        else local r=${1:-$remote}
             ssh $(jq <<<$r .env -r) -- \
                 sh -c "'$(run_ls_sets_cmd $(jq <<<$r .depl)/runs)'"
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
        local env=$( jq <<<$remote  .env -r)
        local depl=$(jq <<<$remote .depl -r)
        if   test -n "$on_remote"
        then if test -n "$(ssh $env -- sh -c "'$(run_ls_sets_cmd $depl/runs)'" |
                                       grep $name || true)"
             then rsync -Wa --delete-after \
                      $env:$depl/runs/.sets/$name ../cardano-node/run/.sets
                  ssh $env -- \
                      sh -c "'cd $depl/runs/.sets/$name && find . -type l | cut -d/ -f2'"
             else ssh $env -- \
                      sh -c "'if test -f $depl/runs/$name/meta.json;
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
        local dir=$global_rundir/$run

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
        else echo -n "$global_rundir/$1"
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
        local mach=${2:-node-1}
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
               --* ) msg "FATAL:  run-or-set, unknown flag '$1'"; usage_run;;
               * ) break;; esac; shift; done

        local run=${1:?$usage}
        if   run check "${check_args[@]}" "$run"
        then run compute-path             "$run"
        else return 1
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
        local profile_name=${1:?$usage}; shift
        local backend_name=${1:?$usage}; shift

        local profile= topology= genesis_cache_entry= manifest= cabal_mode=
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

        ## 1. genesis cache entry
        progress "run | genesis" "cache entry:  $(if test -n "$genesis_cache_entry"; then echo pre-supplied; else echo preparing a new one..; fi)"
        if test -z "$genesis_cache_entry"
        then genesis_cache_entry=$(
                 genesis prepare-cache-entry \
                     "$profile"/profile.json \
                  "$profile"/node-specs.json)
        fi

        ## 2. allocate time
        progress "run | time" "allocating time:"
        local timing=$(profile allocate-time "$profile"/profile.json)
        profile describe-timing "$timing"

        ## 3. decide the tag:
        local hash=$(jq '."cardano-node" | .[:5]' -r <<<$manifest)
        local run=$(jq '.start_tag' -r <<<$timing)$(if test "$batch" != 'plain'; then echo -n .$batch; fi).$hash.$profile_name.${backend_name::3}
        progress "run | tag" "allocated run identifier (tag):  $(with_color white $run)"

        ## 4. allocate directory:
        local dir=$global_rundir/$run
        local realdir=$(realpath --canonicalize-missing "$dir")

        test "$(dirname "$realdir")" = "$(realpath "$global_rundir")" ||
            fatal "profile | allocate bad tag/run dir:  $run @ $dir"
        test ! -e "$dir" ||
            fatal "profile | allocate tag busy:  $run @ $dir"
        mkdir -p "$dir"/flag && test -w "$dir" ||
            fatal "profile | allocate failed to create writable run directory:  $dir"

        ## 5. populate the directory:
        progress "run | profile" "$(if test -n "$profile"; then echo "pre-supplied ($profile_name):  $profile"; else echo "computed:  $profile_name"; fi)"
        if test -n "$profile"
        then
            test "$(jq -r .name $profile/profile.json)" = "$profile_name" ||
                fatal "profile | allocate incoherence:  --profile $profile/profile.json mismatches '$profile_name'"
            ln -s "$profile"                 "$dir"/profile
            cp    "$profile"/profile.json    "$dir"/profile.json
            cp    "$profile"/node-specs.json "$dir"/node-specs.json
        else
            fail "Mode no longer supported:  operation without profile/ directory."
        fi

        local args=(
            --arg       run              "$run"
            --arg       batch            "$batch"
            --arg       profile_name     "$profile_name"
            --argjson   timing           "$timing"
            --slurpfile profile_content  "$dir"/profile.json
            --argjson   manifest         "$manifest"
        )
        jq_fmutate "$dir"/meta.json '. *
           { meta:
             { tag:              $run
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

        backend allocate-run "$dir"

        progress "run" "allocated $(with_color white $run) @ $dir"
        run     describe "$run"
        profile describe "$dir"/profile.json
        run  set-current "$run"
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
        local usage="USAGE: wb run $op RUN [MACHINE] [DEPLOYMENT=bench-1] [ENV=bench]"
        local run=${1:?$usage}
        local mach=${2:-all-hosts}
        local depl=${3:-bench-1}
        local env=${4:-bench}

        local args=(
            "$env"
            "$depl"
            "$run"
            'if test -f compressed/logs-$obj.tar.zst; then cat compressed/logs-$obj.tar.zst; else tar c $obj --zstd --ignore-failed-read; fi'

            common-run-files
            $mach
        )
        run_aws_get "${args[@]}";;

    fetch-analysis | fa )
        local usage="USAGE: wb run $op RUN.."
        local runs=() run
        for rs in $*
        do runs+=($(run "${sargs[@]}" run-or-set --query --remote $rs || echo $rs))
        done
        if test $# = 0; then runs=(current); fi

        local env='bench'
        local depl='bench-1'

        progress "aws" "trying to fetch analyses:  $(white ${runs[*]})"
        for run in ${runs[*]}
        do if   test "$(ssh $env -- sh -c "'ls -ld $depl/runs/$run          | wc -l'")" = 0
           then fail "fetch-analysis:  run does not exist on AWS: $(white $run)"
           elif test "$(ssh $env -- sh -c "'ls -ld $depl/runs/$run/analysis | wc -l'")" = 0
           then fail "fetch-analysis:  run has not been analysed on AWS: $(white $run)"
           else local analysis_files=(
                   $(ssh $env -- \
                     sh -c "'cd $depl/runs/$run && ls analysis/{cdf/*.cdf,*.{json,org,txt}} | fgrep -v -e flt.json -e flt.logobjs.json -e flt.perf-stats.json'" \
                     2>/dev/null)
                )
                local args=(
                   "${run_aws_get_args[@]}"
                   "$env"
                   "$depl"
                   "$run"
                   'tar c ${files[*]} --zstd'

                   common-run-files
                   ${analysis_files[*]}
                )
                run_aws_get "${args[@]}"
           fi
        done
        ;;

    analyse-aws )
        local usage="USAGE: wb run $op RUN [MACHINE] [DEPLOYMENT=bench-1] [ENV=bench]"
        local run=${1:?$usage}
        local mach=${2:-}
        local depl=${3:-bench-1}
        local env=${4:-bench}

        if   test "$(ssh $env -- sh -c "'ls -ld $depl/runs/$run          | wc -l'")" = 0
        then fail "analyse-aws:  run does not exist on AWS: $(white $run)"
        else ssh $env -- sh -c "'export WB_RUNDIR=../$depl/runs && cd cardano-node && echo env: $(yellow $env), rundir: $(color blue)\$WB_RUNDIR$(color reset), workbench: $(color yellow)\$(git log -n1)$(color reset) && make analyse RUN=$run'"
        fi
        ;;

    list-hosts | hosts )
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$global_rundir/$run

        if test -f "$dir"/node-specs.json
        then jq             'keys | .[]' -r "$dir"/node-specs.json
        else jq '.hostname | keys | .[]' -r "$dir"/meta.json; fi;;

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
        local usage="USAGE: wb run $op RUN"
        local run=${1:?$usage}
        local dir=$global_rundir/$run
        local genesis="$dir"/genesis-shelley.json
        local genesis_orig="$genesis".orig

        local size=$(ls -s "$genesis" | cut -d' ' -f1)
        if test "$size" -gt 1000
        then progress "run" "genesis size: ${size}k, trimming.."
             mv   "$genesis" "$genesis_orig"
             jq > "$genesis" '
               .initialFunds = {}
             | .staking      = {}
             ' "$genesis_orig"; fi;;

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

        run compat-meta-fixups "$run"
        ;;

    stop )
        local run=${1:-current}
        local dir=$(run get "$run")

        if backend is-running "$dir"
        then progress "run" "terminating.."
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
        run start          "$@" "$run"

        msg "cluster re-started in the same run directory: $dir"
        ;;

    * ) usage_run;; esac
}

run_aws_get() {
    local usage='USAGE: run_aws_get ENV DEPLOYMENT RUN REMOTE-TAR-CMD OBJ..'
    local clean=
    while test $# -gt 0
    do case "$1" in
           --clean | -c ) clean='true';;
           -- ) shift; break;;
           --* ) msg "FATAL:  unknown flag '$1'"; fail "$usage";;
           * ) break;; esac; shift; done

    local env=${1:?$usage}; shift
    local depl=${1:?$usage}; shift
    local run=${1:?$usage}; shift
    local remote_tar_cmd=${1:?$usage}; shift
    local objects=($*)

    progress "run_aws_get" "env $(yellow $env) depl $(yellow $depl) run $(white $run)"
    progress "run_aws_get" "tar $(green $remote_tar_cmd)"

    local meta=$(ssh $env -- sh -c "'jq . $depl/runs/$run/meta.json'")
    if ! jq . <<<$meta >/dev/null
    then fail "run_aws_get:  malformed $(yellow meta.json) in $(white $run) on $(white $depl)@$(white env)"; fi

    ## Minor validation passed, create & populate run with remote data:
    local dir=$global_rundir/$run
    if test -z "$run" -o -z "$global_rundir"
    then fail "run_aws_get: run=$run global_rundir=$global_rundir"
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
    progress "run | fetch $(white $run)" "objects to fetch:  $(white $count) total"

    local max_batch=9 base=0 batch
    while test $base -lt $count
    do local batch=(${xs[*]:$base:$max_batch})
       {
           local lbatch=(${batch[*]})
           ssh $env -- \
               sh -c "'files=(${lbatch[*]}); cd $depl/runs/$run && ${remote_tar_cmd}'" |
               (cd $dir
                tar x --zstd ||
                    progress "fetch error" "'files=(${lbatch[*]}); cd $depl/runs/$run && ${remote_tar_cmd}'"
               )
           progress "run | fetch $(white $run)" "batch done:  $(yellow ${batch[*]})"
       } &
       sleep 1
       base=$((base + max_batch))
    done
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

expand_runspecs() {
    local runs=() rs

    if test $# = 0
    then runs=(current)
    else for rs in $*
         do runs+=($(run run-or-set $rs))
         done
    fi
    echo ${runs[*]}
}

run_ls_cmd() {
    local rundir=$1

    echo 'cd '$rundir' && \
          find . -mindepth 2 -maxdepth 2 -type f -name meta.json -exec dirname \{\} \; |
          grep -v "current\$\|deploy-logs\$" |
          cut -c3- |
          sort || true'
}

run_ls_tabulated_cmd() {
    local rundir=$1 limit=$2

    echo 'cd '$rundir' && \
          find . -mindepth 2 -maxdepth 2 -type f -name meta.json -exec dirname \{\} \; |
          grep -v "current\$\|deploy-logs\$" |
          cut -c3- |
          sort |
          tail -n'$limit' |
          while read lst_tag; test -n "$lst_tag";
          do printf_args=(
               $(jq ".meta | .manifest as \$manif |
                      \"\\(.profile) \\(\$manif.\"cardano-node\" | .[:7]) \\(.batch) \\(\$manif.\"cardano-node-version\") \\(\$manif.\"cardano-node-branch\")\"
                    " -r <$lst_tag/meta.json))
              printf "%16s  %-75s %7s %-20s %-15s %10s\n" \
                    $(echo $lst_tag |cut -c -16) ${printf_args[*]}
          done || true'
}

run_ls_sets_cmd() {
    local rundir=$1

    echo 'cd '$rundir'/.sets 2>/dev/null && \
          find -L . -mindepth 3 -maxdepth 3 -type f -name meta.json -exec dirname \{\} \; |
          cut -d/ -f2 |
          sort -u || true'
}
