usage_analyse() {
     usage "analyse" "Analyse cluster runs" <<EOF
    multi-run RUN-NAME..  Standard analyses on a batch of runs, followed by a multi-run summary.

    full-run-analysis RUN-NAME..
                          Standard batch of analyses: block-propagation, and machine-timeline
    multi-run-summary-only RUN-NAME..
                          Summarise results of multiple runs: results in \$global_rundir

    call RUN-NAME OPS..   Execute 'locli' "uops" on the specified run

    Options of 'analyse' command:

       --filters F,F,F..  Comma-separated list of named chain filters:  see bench/chain-filters
                            Note: filter names have no .json suffix
       --dump-logobjects  Dump the intermediate data: lifted log objects
EOF
}

analyse() {
local filters=() aws= sargs=() unfiltered=
local dump_logobjects= dump_machviews= dump_chain_raw= dump_chain= dump_slots_raw= dump_slots=
while test $# -gt 0
do case "$1" in
       --dump-logobjects | -lo )  sargs+=($1);    dump_logobjects='true';;
       --dump-machviews  | -mw )  sargs+=($1);    dump_machviews='true';;
       --dump-chain-raw  | -cr )  sargs+=($1);    dump_chain_raw='true';;
       --dump-chain      | -c )   sargs+=($1);    dump_chain='true';;
       --filters )                sargs+=($1 $2); analysis_set_filters "base,$2"; shift;;
       --no-filters | --unfiltered | -u )
                                  sargs+=($1);    analysis_set_filters ""; unfiltered='true';;
       * ) break;; esac; shift; done

if curl --connect-timeout 0.5 http://169.254.169.254/latest/meta-data >/dev/null 2>&1
then aws='true'; fi

## Work around the odd parallelism bug killing performance on AWS:
if test -n "$aws"
then locli_rts_args=(+RTS -N1 -A128M -RTS)
     echo "{ \"aws\": true }"
else locli_rts_args=()
     echo "{ \"aws\": false }"
fi

local op=${1:-standard}; if test $# != 0; then shift; fi

case "$op" in
    # 'read-mach-views' "${logs[@]/#/--log }"
    multi-run-pattern | multi-pattern | multipat | mp )
        analyse ${sargs[*]} multi-run $(run list-pattern $1)
        ;;

    multi-run-full | multi-run | multi )
        progress "analysis" "$(white multi-summary) on: $(yellow $*)"

        analyse full-run-analysis      "$*"
        analyse multi-run-summary-only "$*"
        ;;

    multi-run-summary-only | summary )
        local script=(
            read-clusterperfs
            summarise-clusterperfs
            dump-multi-clusterperf

            read-propagations
            summarise-propagations
            dump-multi-propagation

            report-multi-clusterperf-{full,brief}

            report-multi-prop-{forger,peers,endtoend,full}
        )
        progress "analysis" "$(white multi-summary), calling script: $(colorise ${script[*]})"
        analyse ${sargs[*]} multi-call "$*" ${script[*]}
        ;;

    full-run-analysis | standard | std )
        local script=(
            logs               $(test -n "$dump_logobjects" && echo 'dump-logobjects')
            context

            build-mach-views   $(test -n "$dump_machviews" && echo 'dump-mach-views')
            build-chain        $(test -n "$dump_chain_raw" && echo 'dump-chain-raw')
            chain-timeline-raw
            filter-chain       $(test -n "$dump_chain" && echo 'dump-chain')
            chain-timeline

            collect-slots      $(test -n "$dump_slots_raw" && echo 'dump-slots-raw')
            filter-slots       $(test -n "$dump_slots" && echo 'dump-slots')
            timeline-slots

            propagation
            dump-propagation
            report-prop-{forger,peers,endtoend,full}

            machperf
            dump-machperf
            clusterperf
            dump-clusterperf
            report-machperf-{full,brief}
            report-clusterperf-{full,brief}
         )
        progress "analysis" "$(white full), calling script:  $(colorise ${script[*]})"
        analyse ${sargs[*]} map "call ${script[*]}" "$@"
        ;;

    performance | perf )
        local script=(
            logs               $(test -n "$dump_logobjects" && echo 'dump-logobjects')
            context

            collect-slots      $(test -n "$dump_slots_raw" && echo 'dump-slots-raw')
            filter-slots       $(test -n "$dump_slots" && echo 'dump-slots')
            timeline-slots

            machperf
            dump-machperf
            report-perf-{full,brief}
        )
        progress "analysis" "$(white performance), calling script:  $(colorise ${script[*]})"
        analyse ${sargs[*]} map "call ${script[*]}" "$@"
        ;;

    performance-single-host | perf-single )
        local usage="USAGE: wb analyse $op HOST"
        local host=${1:?usage}; shift

        local script=(
            logs               'dump-logobjects'
            context

            collect-slots      $(test -n "$dump_slots_raw" && echo 'dump-slots-raw')
            filter-slots       $(test -n "$dump_slots" && echo 'dump-slots')
            timeline-slots

            machperf
            dump-machperf
            report-perf-{full,brief}
        )
        progress "analysis" "$(with_color white performance), calling script:  $(colorise ${script[*]})"
        analyse ${sargs[*]} map "call --host $host ${script[*]}" "$@"
        ;;

    map )
        local usage="USAGE: wb analyse $op OP [-opt-flag] [--long-option OPTVAL] RUNS.."
        ## Meaning: map OP over RUNS, optionally giving flags/options to OP

        local preop=${1:?usage}; shift
        local runs=($*); if test $# = 0; then runs=(current); fi

        local op_split=($preop)
        local op=${op_split[0]}
        local op_args=()
        ## This is magical and stupid, but oh well, it's the cost of abstraction:
        ##  1. We are passing all '--long-option VAL' pairs to the mapped preop
        ##  2. We are passing all '-opt-flag' flags to the mapped preop
        for ((i=1; i<=${#op_split[*]}; i++))
        do local arg=${op_split[$i]} argnext=${op_split[$((i+1))]}
           case "$arg" in
               --* ) op_args+=($arg $argnext); i=$((i+1));;
               -*  ) op_args+=($arg);;
               * ) break;; esac; done
        local args=${op_split[*]:$i}
        progress "analyse" "mapping op $(with_color yellow $op ${op_args[*]}) $(with_color cyan $args) over runs:  $(with_color white ${runs[*]})"
        for r in ${runs[*]}
        do analyse ${sargs[*]} prepare $r
           analyse ${sargs[*]} $op ${op_args[*]} $r ${args[*]}
        done
        ;;

    call )
        local usage="USAGE: wb analyse $op [--host HOST] RUN-NAME OPS.."

        local host=
        while test $# -gt 0
        do case "$1" in
               --host ) host=$2; shift;;
               * ) break;; esac; shift; done

        local name=${1:?$usage}; shift
        local dir=$(run get "$name")
        local adir=$dir/analysis
        test -n "$dir" -a -d "$adir" || fail "malformed run: $name"

        local logfiles=($(if test -z "$host"
                          then ls "$adir"/logs-*.flt.json
                          else ls "$adir"/logs-$host.flt.json; fi))

        if test -z "${filters[*]}" -a -z "$unfiltered"
        then local filter_names=$(jq '.analysis.filters
                                      | join(",")
                                     ' "$dir"/profile.json --raw-output)
             analysis_set_filters "$filter_names"
        fi

        local v0=("$@")
        local v1=(${v0[*]/#logs/               'unlog' --host-from-log-filename ${logfiles[*]/#/--log }})
        local v2=(${v1[*]/#context/            'meta-genesis'   --run-metafile "$dir"/meta.json
                                                             --shelley-genesis "$dir"/genesis-shelley.json})
        local v3=(${v2[*]/#dump-chain-raw/     'dump-chain-raw'        --chain "$adir"/chain-raw.json})
        local v4=(${v3[*]/#chain-timeline-raw/ 'timeline-chain-raw' --timeline "$adir"/chain-raw.txt})
        local v5=(${v4[*]/#filter-chain/       'filter-chain'                  ${filters[*]}})
        local v6=(${v5[*]/#dump-chain/         'dump-chain'            --chain "$adir"/chain.json})
        local v7=(${v6[*]/#chain-timeline/     'timeline-chain'     --timeline "$adir"/chain.txt})
        local v8=(${v7[*]/#filter-slots/       'filter-slots'                  ${filters[*]}})
        local v9=(${v8[*]/#dump-propagation/   'dump-propagation'       --prop "$adir"/blockprop.json})

        local va=(${v9[*]/#report-prop-forger/  'report-prop-forger'  --report "$adir"/blockprop-forger.txt  })
        local vb=(${va[*]/#report-prop-peers/   'report-prop-peers'   --report "$adir"/blockprop-peers.txt   })
        local vc=(${vb[*]/#report-prop-endtoend/'report-prop-endtoend' --report "$adir"/blockprop-endtoend.txt})
        local vd=(${vc[*]/#report-prop-full/    'report-prop-full'    --report "$adir"/blockprop-full.txt    })
        local ve=(${vd[*]/#dump-clusterperf/   'dump-clusterperf' --clusterperf "$adir"/cluster-perf.json })
        local vf=(${ve[*]/#report-clusterperf-full/ 'report-clusterperf-full' --report "$adir"/clusterperf-full.txt    })
        local vg=(${vf[*]/#report-clusterperf-brief/'report-clusterperf-brief' --report "$adir"/clusterperf-brief.txt    })
        local ops_final=(${vg[*]})

        progress "analysis | locli" "$(with_color reset ${locli_rts_args[@]}) $(colorise "${ops_final[@]}")"
        time locli "${locli_rts_args[@]}" "${ops_final[@]}"
        ;;

    multi-call )
        local usage="USAGE: wb analyse $op \"RUN-NAMES..\" OPS.."

        local runs=${1:?$usage}; shift
        local dirs=(  $(for run  in $runs;       do run get "$run"; echo; done))
        local adirs=( $(for dir  in ${dirs[*]};  do echo $dir/analysis; done))
        local props=( $(for adir in ${adirs[*]}; do echo --prop        ${adir}/blockprop.json;    done))
        local cperfs=($(for adir in ${adirs[*]}; do echo --clusterperf ${adir}/cluster-perf.json; done))

        local v0=("$@")
        local v1=(${v0[*]/#read-clusterperfs/ 'read-clusterperfs' ${cperfs[*]} })
        local v2=(${v1[*]/#read-propagations/ 'read-propagations' ${props[*]}  })
        local v3=(${v2[*]/#dump-multi-clusterperf/ 'dump-multi-clusterperf' --multi-clusterperf $global_rundir'/multi-cluster-perf.json' })
        local v4=(${v3[*]/#dump-multi-propagation/ 'dump-multi-propagation' --multi-prop $global_rundir'/multi-blockprop.json' })
        local v5=(${v4[*]/#report-multi-prop-forger/ 'report-multi-prop-forger' --report $global_rundir'/multi-prop-forger.json' })
        local v6=(${v5[*]/#report-multi-prop-peers/ 'report-multi-prop-peers' --report $global_rundir'/multi-prop-peers.txt' })
        local v7=(${v6[*]/#report-multi-prop-endtoend/ 'report-multi-prop-endtoend' --report $global_rundir'/multi-prop-endtoend.txt' })
        local v8=(${v7[*]/#report-multi-prop-full/ 'report-multi-prop-full' --report $global_rundir'/multi-prop-full.txt' })
        local v9=(${v8[*]/#report-multi-clusterperf-full/ 'report-multi-clusterperf-full' --report $global_rundir'/multi-clusterperf-full.txt' })
        local va=(${v9[*]/#report-multi-clusterperf-brief/ 'report-multi-clusterperf-brief' --report $global_rundir'/multi-clusterperf-brief.txt' })
        local ops_final=(${va[*]})

        progress "analysis | locli" "$(with_color reset ${locli_rts_args[@]}) $(colorise "${ops_final[@]}")"
        time locli "${locli_rts_args[@]}" "${ops_final[@]}"
        ;;

    prepare | prep )
        local usage="USAGE: wb analyse $op [RUN-NAME=current].."

        local name=${1:-current}; if test $# != 0; then shift; fi
        local dir=$(run get "$name")
        test -n "$dir" || fail "malformed run: $name"

        run trim "$name"

        progress "analyse" "preparing run for analysis:  $(with_color white $name)"
        local adir=$dir/analysis
        mkdir -p "$adir"

        ## 0. ask locli what it cares about
        local keyfile="$adir"/substring-keys
        case $(jq '.node.tracing_backend // "iohk-monitoring"' --raw-output $dir/profile.json) in
             trace-dispatcher ) locli 'list-logobject-keys'        --keys        "$keyfile";;
             iohk-monitoring  ) locli 'list-logobject-keys-legacy' --keys-legacy "$keyfile";;
        esac

        ## 1. unless already done, filter logs according to locli's requirements
        local logdirs=($(ls -d "$dir"/node-*/ 2>/dev/null))
        local logfiles=($(ls "$adir"/logs-node-*.flt.json 2>/dev/null))
        local prefilter=$(test -z "${logfiles[*]}" && echo 'true' || echo 'false')
        echo "{ \"prefilter\": $prefilter }"
        if test x$prefilter != xtrue
        then return; fi

        local jq_args=(
            --sort-keys
            --compact-output
            'delpaths([["app"],["env"],["loc"],["msg"],["ns"],["sev"]])'
        )
        progress "analyse" "filtering logs:  $(with_color black ${logdirs[@]})"
        for d in "${logdirs[@]}"
        do throttle_shell_job_spawns
           local logfiles="$(ls "$d"/stdout* 2>/dev/null | tac) $(ls "$d"/node-*.json 2>/dev/null)"
           if test -z "$logfiles"
           then msg "no logs in $d, skipping.."; fi
           local output="$adir"/logs-$(basename "$d").flt.json
           grep -hFf "$keyfile" $logfiles > "$output" &
        done

        wait;;

    * ) usage_analyse;; esac
}

num_jobs="\j"
num_threads=$({ grep processor /proc/cpuinfo 2>/dev/null || echo -e '\n\n\n';
              } | wc -l)

throttle_shell_job_spawns() {
    sleep 0.5s
    while ((${num_jobs@P} >= num_threads - 4))
    do wait -n; sleep 0.$(((RANDOM % 5) + 1))s; done
}

analysis_set_filters() {
    local filter_names=($(echo $1 | sed 's_,_ _g'))
    local filter_paths=(${filter_names[*]/#/"$global_basedir/chain-filters/"})
    local filter_files=(${filter_paths[*]/%/.json})

    for f in ${filter_files[*]}
    do test -f "$f" ||
            fail "no such filter: $f"; done

    filters+=(${filter_files[*]/#/--filter })
}

analysis_classify_traces() {
    local name=${1:-current}; if test $# != 0; then shift; fi
    local node=${1:-node-0}; if test $# != 0; then shift; fi
    local dir=$(run get "$name")

    progress "analysis" "enumerating namespace from logs of $(with_color yellow $node)"
    grep -h '^{' $dir/$node/stdout* | jq --raw-output '(try .ns[0] // .ns) + ":" + (.data.kind // "")' 2>/dev/null | sort -u
    # grep -h '^{' $dir/$node/stdout* | jq --raw-output '.ns' 2>/dev/null | tr -d ']["' | sort -u
}

analysis_trace_frequencies() {
    local same_types=
    while test $# -gt 0
    do case "$1" in
       --same-types | --same | -s )  same_types='true';;
       * ) break;; esac; shift; done

    local name=${1:-current}; if test $# != 0; then shift; fi
    local dir=$(run get "$name")
    local types=()

    if test -n "$same_types"
    then types=($(analysis_classify_traces $name 'node-0'))
         progress_ne "analysis" "message frequencies: "; fi

    for nodedir in $dir/node-*/
    do local node=$(basename $nodedir)

       if test -z "$same_types"
       then types=($(analysis_classify_traces $name $node))
            progress "analysis" "message frequencies: $(with_color yellow $node)"; fi

       for type in ${types[*]}
       do local ns=$(cut -d: -f1 <<<$type)
          local kind=$(cut -d: -f2 <<<$type)
          echo $(grep -h "\"$ns\".*\"$kind\"\|\"$kind\".*\"$ns\"" $nodedir/stdout* | wc -l) $type
       done |
           sort -nr > $nodedir/log-namespace-occurence-stats.txt
       test -n "$same_types" && echo -n ' '$node >&2
    done
    echo >&2
}

analysis_config_extract_legacy_tracing() {
    local file=$(realpath $1)
    local nix_eval_args=(
        --raw
        --impure
        --expr '
          let f = __fromJSON (__readFile "'$file'");
          in with f;
             __toJSON
             { inherit rotation;
             }'
    )
    nix eval "${nix_eval_args[@]}" | jq --sort-keys
}
