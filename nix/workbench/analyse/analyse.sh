usage_analyse() {
     usage "analyse" "Analyse cluster runs" <<EOF
    $(helpcmd compare RUN_NAME..)     Produce a comparative analysis between specified runs, in a new
     $(blk cmp)                     run directory.  A builtin $(blue .ede) template will be used and dumped,
                             so a later $(red analyse) $(yellow recompare) can use it as basis.

    $(helpcmd recompare RUN_NAME..)   Update an existing comparative analysis between specified runs,
     $(blk recmp)                   using its $(blue .ede) template as basis.

    $(helpcmd variance RUN-NAME..)
     $(blk var)                   Variance analyses on a set of runs

    $(helpcmd standard RUN-NAME..)
     $(blk full std)              Standard batch of analyses: block-propagation, and machine-timeline

    $(helpcmd block-propagation RUN-NAME..)
     $(blk blockprop bp)          Full block propagation analysis

    $(helpcmd re-block-propagation RUN-NAME..)
     $(blk reblockprop rebp)      Rerun block propagation analysis on dumped chain

    $(helpcmd performance RUN-NAME..)
     $(blk perf)                  Cluster performance analysis

    $(helpcmd performance-host HOST RUN-NAME..)
     $(blk perf-host)             Single-host performance analysis

    $(helpcmd call RUN-NAME OPS..)    Execute 'locli' "uops" on the specified run

    $(helpcmd trace-frequencies LOGFILENAME)
     $(blk trace-freq freq)       Classify trace messages by namespace frequency.
                             Output will be stored in a filename derived from argument

    $(helpcmd chain-rejecta-reasons LOGFILENAME)
     $(blk chain-rejecta rejecta) Dump chain rejecta block timeline.
                             Note that only chain filter tags are printed

  $(red analyse) $(blue options):

    $(helpopt --filters F,F,F..)  Comma-separated list of named chain filters:  see bench/analyse/chain-filters
                         Note: filter names have no .json suffix
                         Defaults are specified by the run's profile.
    $(helpopt --filter-expr JSON)
                       A directly specified filter JSON expression.
                         Please see workbench filters for an example of lists of
                         the intended filter expressions.
    $(helpopt --no-filters)       Disable filters implied by the profile
    $(helpopt --filter-reasons)   Chain timeline: explain per-block filter-out reasons
    $(helpopt --chain-errors)     Chain timeline: show per-block anomalies
    $(helpopt --ok-loany)         [MULTI] Allow a particular LOAnyType.
                         Default:  ${analysis_allowed_loanys[*]}
    $(helpopt --lodecodeerror-ok) Allow non-EOF LODecodeError logobjects
    $(helpopt --dump-logobjects)  Dump the intermediate data: lifted log objects
    $(helpopt --dump-machviews)   Blockprop: dump machine views (JSON)
    $(helpopt --dump-slots-raw)   Machperf:  dump unfiltered slots (JSON)
    $(helpopt --dump-slots)       Machperf:  dump filtered slots (JSON)
    $(helpopt --multi-overall)    Multirun:  Overall dataset statistical summary
    $(helpopt --multi-inter-cdf)  Multirun:  Inter-sample (i.e. inter-CDF) stats
    $(helpopt --force-prepare)    Logs:  force re-prefiltering & manifest collection
EOF
}
analysis_allowed_loanys=(
    'LAFallingEdge'
    'LANonBlocking'
    'LARollback'
)

analyse_default_op='standard'

analyse() {
local filters=() filter_exprs=() filter_reasons= chain_errors= aws= sargs=() unfiltered= perf_omit_hosts=()
local dump_logobjects= dump_machviews= dump_chain= dump_slots_raw= dump_slots=
local multi_aspect='--inter-cdf' rtsmode= force_prepare=
locli_args=()

progress "analyse" "args:  $(yellow $*)"
while test $# -gt 0
do case "$1" in
       --filters | -f )           sargs+=($1 "$2"); analysis_set_filters "unitary,$2"; shift;;
       --filter-expr | -fex )     sargs+=($1 "$2"); filter_exprs+=($2); shift;;
       --filter-block-expr | -fbex ) sargs+=($1 "$2"); filter_exprs+=('{ "tag":"CBlock" , "contents": '"$2"'}'); shift;;
       --filter-slot-expr | -fsex )  sargs+=($1 "$2"); filter_exprs+=('{ "tag":"CSlot" , "contents": '"$2"'}'); shift;;
       --no-filters | --unfiltered | -u )
                                  sargs+=($1);    analysis_set_filters ""; unfiltered='true';;
       --filter-reasons  | -fr )   sargs+=($1);    filter_reasons='true';;
       --chain-errors    | -cer )  sargs+=($1);    chain_errors='true';;
       --loany-ok         | -lok ) sargs+=($1);    locli_args+=(--loany-ok);;
       --lodecodeerror-ok | -dok ) sargs+=($1);    locli_args+=(--lodecodeerror-ok);;
       --dump-logobjects | -lo )   sargs+=($1);    dump_logobjects='true';;
       --dump-machviews  | -mw )   sargs+=($1);    dump_machviews='true';;
       --dump-slots-raw  | -sr )   sargs+=($1);    dump_slots_raw='true';;
       --dump-slots      | -s )    sargs+=($1);    dump_slots='true';;
       --multi-overall )           sargs+=($1);    multi_aspect='--overall';;
       --multi-inter-cdf )         sargs+=($1);    multi_aspect='--inter-cdf';;
       --rtsmode-aws | --aws )     sargs+=($1);    rtsmode='aws';;
       --rtsmode-lomem | --lomem ) sargs+=($1);    rtsmode='lomem';;
       --rtsmode-hipar )           sargs+=($1);    rtsmode='hipar';;
       --perf-omit-host )          sargs+=($1 "$2"); perf_omit_hosts+=($2); shift;;
       --force-prepare | -fp )     sargs+=($1);    force_prepare='true';;
       --trace )                   sargs+=($1);    set -x;;
       * ) break;; esac; shift; done

local op=${1:-$analyse_default_op}; if test $# != 0; then shift; fi

case "$op" in
    # 'read-mach-views' "${logs[@]/#/--log }"

    compare | cmp )
        local runs=($(expand_runspecs $*))
        local baseline=${runs[0]}
        progress "analysis" "$(white comparing) $(colorise ${runs[*]:1}) $(plain against baseline) $(white $baseline)"
        analyse "${sargs[@]}" multi-call 'compare' "${runs[*]}" 'compare'
        ;;

    recompare | recmp )
        local runs=($(expand_runspecs $*))
        local baseline=${runs[0]}
        progress "analysis" "$(white regenerating comparison) of $(colorise ${runs[*]:1}) $(plain against baseline) $(white $baseline)"
        analyse "${sargs[@]}" multi-call 'compare' "${runs[*]}" 'update'
        ;;

    variance | var )
        local script=(
            read-clusterperfs
            compute-multi-clusterperf
            multi-clusterperf-json
            multi-clusterperf-gnuplot
            multi-clusterperf-org
            multi-clusterperf-report
            multi-clusterperf-full

            read-propagations
            compute-multi-propagation
            multi-propagation-json
            multi-propagation-org
            multi-propagation-{control,forger,peers,endtoend}
            multi-propagation-gnuplot
            multi-propagation-full
        )
        verbose "analysis" "$(white variance), calling script: $(colorise ${script[*]})"
        analyse "${sargs[@]}" multi-call 'variance' "$*" ${script[*]}
        ;;

    rerender | render )
        local script=(
            context

            read-propagations
            propagation-json
            propagation-org
            propagation-{control,forger,peers,endtoend}
            propagation-gnuplot
            propagation-full

            read-clusterperfs
            clusterperf-json
            clusterperf-gnuplot
            clusterperf-org
            clusterperf-report
            clusterperf-full
         )
        verbose "analysis" "$(white full), calling script:  $(colorise ${script[*]})"
        analyse "${sargs[@]}" map "call ${script[*]}" "$@"
        ;;

    standard | full | std )
        local script=(
            logs               $(test -n "$dump_logobjects" && echo 'dump-logobjects')
            context

            build-mach-views   $(test -n "$dump_machviews"  && echo 'dump-mach-views')
            rebuild-chain
            dump-chain
            chain-timeline

            collect-slots      $(test -n "$dump_slots_raw"  && echo 'dump-slots-raw')
            filter-slots       $(test -n "$dump_slots"      && echo 'dump-slots')
            timeline-slots

            compute-propagation
            propagation-json
            propagation-org
            propagation-{control,forger,peers,endtoend}
            propagation-gnuplot
            propagation-full

            compute-machperf
            render-machperf

            compute-clusterperf
            clusterperf-json
            clusterperf-gnuplot
            clusterperf-org
            clusterperf-report
            clusterperf-full

            compute-summary
            summary-json
            summary-report
         )
        verbose "analysis" "$(white full), calling script:  $(colorise ${script[*]})"
        analyse "${sargs[@]}" map "call ${script[*]}" "$@"
        ;;

    block-propagation | blockprop | bp )
        local script=(
            logs               $(test -n "$dump_logobjects" && echo 'dump-logobjects')
            context

            build-mach-views   $(test -n "$dump_machviews"  && echo 'dump-mach-views')
            rebuild-chain
            dump-chain
            chain-timeline

            compute-propagation
            propagation-json
            propagation-org
            propagation-{control,forger,peers,endtoend}
            propagation-gnuplot
            propagation-full
         )
        verbose "analysis" "$(white full), calling script:  $(colorise ${script[*]})"
        analyse "${sargs[@]}" map "call ${script[*]}" "$@"
        ;;

    re-block-propagation | reblockprop | rebp )
        fail "re-block-propagation is broken:  read-chain not implemented"
        local script=(
            read-chain
            chain-timeline

            compute-propagation
            propagation-json
            propagation-org
            propagation-{control,forger,peers,endtoend}
            propagation-gnuplot
            propagation-full
         )
        verbose "analysis" "$(white full), calling script:  $(colorise ${script[*]})"
        analyse "${sargs[@]}" map "call ${script[*]}" "$@"
        ;;

    performance | perf )
        local script=(
            logs               $(test -n "$dump_logobjects" && echo 'dump-logobjects')
            context

            collect-slots      $(test -n "$dump_slots_raw"  && echo 'dump-slots-raw')
            filter-slots       $(test -n "$dump_slots"      && echo 'dump-slots')
            timeline-slots

            compute-machperf
            render-machperf

            compute-clusterperf
            clusterperf-json
            clusterperf-gnuplot
            clusterperf-org
            clusterperf-report
            clusterperf-full
        )
        verbose "analysis" "$(white performance), calling script:  $(colorise ${script[*]})"
        analyse "${sargs[@]}" map "call ${script[*]}" "$@"
        ;;

    performance-host | perf-host )
        local usage="USAGE: wb analyse $op HOST"
        local host=${1:?$usage}; shift

        local script=(
            logs               'dump-logobjects'
            context

            collect-slots      $(test -n "$dump_slots_raw"  && echo 'dump-slots-raw')
            filter-slots       $(test -n "$dump_slots"      && echo 'dump-slots')
            timeline-slots

            compute-machperf
            render-machperf
        )
        verbose "analysis" "$(with_color white performance), calling script:  $(colorise ${script[*]})"
        analyse "${sargs[@]}" map "call --host $host ${script[*]}" "$@"
        ;;

    map )
        local usage="USAGE: wb analyse $op OP [-opt-flag] [--long-option OPTVAL] RUNS.."
        ## Meaning: map OP over RUNS, optionally giving flags/options to OP

        local preop=${1:?$usage}; shift
        local runs=($(expand_runspecs $*))

        local op_split=($preop)
        local op=${op_split[0]}
        local op_args=()
        ## This is magical and stupid, but oh well, it's the cost of abstraction:
        ##  1. We are passing all '--long-option VAL' pairs to the mapped preop
        ##  2. We are passing all '-opt-flag' flags to the mapped preop
        local nops=${#op_split[*]}
        for ((i=1; i<=$nops; i++))
        do local arg=${op_split[$i]}
           if test $i -lt $((nops - 1))
           then argnext=${op_split[$((i+1))]}
           else argnext=
           fi
           case "$arg" in
               --* ) test $i -lt $nops || \
                           fail "No value passed for option:  $arg"
                     op_args+=($arg "$argnext"); i=$((i+1));;
               -*  ) op_args+=($arg);;
               * ) break;; esac; done
        local args=${op_split[@]:$i}
        progress "analyse" "mapping op $(with_color yellow $op "${op_args[@]}") $(with_color cyan $args) over runs:  $(with_color white ${runs[*]})"
        for r in ${runs[*]}
        do analyse "${sargs[@]}" prepare $r
           analyse "${sargs[@]}" $op "${op_args[@]}" $r "${args[@]}"
        done
        ;;

    call )
        local usage="USAGE: wb analyse $op [--host HOST] RUN-NAME OPS.."

        local host=
        while test $# -gt 0
        do case "$1" in
               --host ) host=$2; shift;;
               * ) break;; esac; shift; done

        local run=${1:?$usage}; shift
        local dir=$(run get "$run")
        local adir=$dir/analysis
        test -n "$dir" -a -d "$adir" || fail "malformed run: $run"

        local logfiles=(
            $(if test -z "$host"
              then ls "$adir"/logs-*.flt.json
              else ls "$adir"/logs-$host.flt.json; fi))
        test ${#logfiles[*]} -gt 0 ||
            fail "no files match $adir"'/logs-*.flt.json'

        local minus_logfiles=(
            $(for host in ${perf_omit_hosts[*]}
              do ls "$adir"/logs-$host.flt.json; done))

        if test -z "$unfiltered"
        then local filter_names=$(jq '(.analysis.filters // [])
                                      | join(",")
                                     ' "$dir"/profile.json --raw-output)
             analysis_set_filters "$filter_names"
             filter_exprs+=($(jq '(.analysis.filter_exprs // [])
                                   | map(tojson)
                                   | join(",")
                                   ' "$dir"/profile.json --raw-output))
        fi
        local filter_exprs_q=("${filter_exprs[@]@Q}")
        filters+=(${filter_exprs_q[@]/#/--filter-expr })
        progress "analysis" "filters exprs: $(yellow "${filter_exprs[@]}")"

        local v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 va vb vc vd ve vf vg vh vi vj vk vl vm vn vo
        v0=( $* )
        v1=("${v0[@]/#logs/                 'unlog' --run-logs \"$adir\"/log-manifest.json ${analysis_allowed_loanys[*]/#/--ok-loany } }")
        v2=("${v1[@]/#context/              'meta-genesis' --run-metafile    \"$dir\"/meta.json
                                                         --shelley-genesis \"$dir\"/genesis-shelley.json }")
        v3=("${v2[@]/#read-chain/           'read-chain'            --chain \"$adir\"/chain.json}")
        v4=("${v3[@]/#rebuild-chain/        'rebuild-chain'                  ${filters[@]}}")
        v5=("${v4[@]/#dump-chain/           'dump-chain'            --chain \"$adir\"/chain.json --chain-rejecta \"$adir\"/chain-rejecta.json}")
        v6=("${v5[@]/#chain-timeline/       'timeline-chain'     --timeline \"$adir\"/chain.txt ${filter_reasons:+--filter-reasons} ${chain_errors:+--chain-errors}}")
        v7=("${v6[@]/#collect-slots/        'collect-slots'           ${minus_logfiles[*]/#/--ignore-log }}")
        v8=("${v7[@]/#filter-slots/         'filter-slots'                   ${filters[@]}}")
        v9=("${v8[@]/#propagation-json/     'render-propagation'   --json \"$adir\"/blockprop.json     --full}")
        va=("${v9[@]/#propagation-org/      'render-propagation'    --org \"$adir\"/blockprop.org      --full}")
        vb=("${va[@]/#propagation-control/  'render-propagation' --org-report \"$adir\"/blockprop.control.org --control}")
        vc=("${vb[@]/#propagation-forger/   'render-propagation' --org-report \"$adir\"/blockprop.forger.org --forger}")
        vd=("${vc[@]/#propagation-peers/    'render-propagation' --org-report \"$adir\"/blockprop.peers.org --peers }")
        ve=("${vd[@]/#propagation-endtoend/ 'render-propagation' --org-report \"$adir\"/blockprop.endtoend.org --end-to-end}")
        vf=("${ve[@]/#propagation-gnuplot/  'render-propagation' --gnuplot \"$adir\"/cdf/%s.cdf            --full}")
        vg=("${vf[@]/#propagation-full/     'render-propagation' --pretty \"$adir\"/blockprop-full.txt --full}")
        vh=("${vg[@]/#clusterperf-json/     'render-clusterperf'   --json \"$adir\"/clusterperf.json --full }")
        vi=("${vh[@]/#clusterperf-org/      'render-clusterperf'    --org \"$adir\"/clusterperf.org --full }")
        vj=("${vi[@]/#clusterperf-report/   'render-clusterperf' --org-report \"$adir\"/clusterperf.report.org --report }")
        vk=("${vj[@]/#clusterperf-gnuplot/  'render-clusterperf' --gnuplot \"$adir\"/cdf/%s.cdf --full }")
        vl=("${vk[@]/#clusterperf-full/     'render-clusterperf' --pretty \"$adir\"/clusterperf-full.txt --full }")
        vm=("${vl[@]/#read-clusterperfs/    'read-clusterperfs' --clusterperf \"$adir\"/clusterperf.json }")
        vn=("${vm[@]/#read-propagations/    'read-propagations'        --prop \"$adir\"/blockprop.json }")
        vo=("${vn[@]/#summary-json/         'render-summary'       --json \"$adir\"/summary.json}")
        vp=("${vo[@]/#summary-report/       'render-summary'     --org-report \"$adir\"/summary.org}")
        local ops_final=()
        for v in "${vp[@]}"
        do eval ops_final+=($v); done

        call_locli "$rtsmode" "${ops_final[@]}"

        local analysis_jsons=($(ls $adir/*.json |
                                    fgrep -v -e '.flt.json'             \
                                             -e '.logobjs.json'         \
                                             -e 'chain-rejecta.json'    \
                                             -e 'chain.json'
              ))
        progress "analyse" "prettifying JSON data:  ${#analysis_jsons[*]} files"
        time json_compact_prettify "${analysis_jsons[@]}"
        progress "output" "run:  $(white $run)  subdir:  $(yellow analysis)"
        ;;

    multi-call )
        local usage="USAGE: wb analyse $op SUFFIX \"RUN-NAMES..\" OPS.."

        local suffix=${1:?$usage}; shift
        local runs=($(expand_runspecs ${1:?$usage})); shift

        local dirs=(  $(for run  in ${runs[*]};  do run get "$run"; echo; done))
        local adirs=( $(for dir  in ${dirs[*]};  do echo $dir/analysis; done))
        local props=( $(for adir in ${adirs[*]}; do echo --prop        ${adir}/blockprop.json;   done))
        local cperfs=($(for adir in ${adirs[*]}; do echo --clusterperf ${adir}/clusterperf.json; done))
        local compares=($(for adir in ${adirs[*]}
                          do echo --summary         ${adir}/summary.json     \
                                  --perf            ${adir}/clusterperf.json \
                                  --prop            ${adir}/blockprop.json
                          done))
        local run=$(for dir in ${dirs[*]}; do basename $dir; done | sort -r | head -n1 | cut -d. -f1-2)_$suffix
        local adir=$(run get-rundir)/$run

        mkdir -p "$adir/cdf"
        progress "analysis | multi-call" "output $(yellow $run), inputs: $(white ${runs[*]})"

        local v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 va vb vc vd ve vf vg vh vi vj vk vl vm vn vo
        v0=("$@")
        v1=(${v0[*]/#read-clusterperfs/ 'read-clusterperfs' ${cperfs[*]} })
        v2=(${v1[*]/#read-propagations/ 'read-propagations' ${props[*]}  })
        v3=(${v2[*]/#multi-clusterperf-json/ 'render-multi-clusterperf' --json $adir/'clusterperf.json' --full $multi_aspect })
        v4=(${v3[*]/#multi-clusterperf-org/     'render-multi-clusterperf' --org $adir/'clusterperf.org' --full $multi_aspect })
        v5=(${v4[*]/#multi-clusterperf-report/  'render-multi-clusterperf' --org-report $adir/'clusterperf.report.org' --report $multi_aspect })
        v6=(${v5[*]/#multi-clusterperf-gnuplot/ 'render-multi-clusterperf' --gnuplot $adir/cdf/'%s.cdf' --full $multi_aspect })
        v7=(${v6[*]/#multi-clusterperf-full/    'render-multi-clusterperf' --pretty $adir/'clusterperf-full.txt' --full $multi_aspect })
        v8=(${v7[*]/#multi-propagation-json/     'render-multi-propagation' --json $adir/'blockprop.json' --full $multi_aspect })
        v9=(${v8[*]/#multi-propagation-org/      'render-multi-propagation' --org $adir/'blockprop.org' --full $multi_aspect })
        va=(${v9[*]/#multi-propagation-control/  'render-multi-propagation' --org-report $adir/'blockprop.control.org' --control $multi_aspect })
        vb=(${va[*]/#multi-propagation-forger/   'render-multi-propagation' --org-report $adir/'blockprop.forger.org' --forger $multi_aspect })
        vc=(${vb[*]/#multi-propagation-peers/    'render-multi-propagation' --org-report $adir/'blockprop.peers.org' --peers $multi_aspect })
        vd=(${vc[*]/#multi-propagation-endtoend/ 'render-multi-propagation' --org-report $adir/'blockprop.endtoend.org' --end-to-end $multi_aspect })
        ve=(${vd[*]/#multi-propagation-gnuplot/  'render-multi-propagation' --gnuplot $adir/cdf/'%s.cdf' --full $multi_aspect })
        vf=(${ve[*]/#multi-propagation-full/     'render-multi-propagation' --pretty $adir/'blockprop-full.txt' --full $multi_aspect })
        vg=(${vf[*]/#compare/ 'compare' --ede nix/workbench/ede --report $adir/report-$run.org ${compares[*]} })
        vh=(${vg[*]/#update/  'compare' --ede nix/workbench/ede --report $adir/report-$run.org ${compares[*]} --template $adir/report-$run.ede })
        local ops_final=(${vh[*]})

        call_locli "$rtsmode" "${ops_final[@]}"

        progress "output" "run:  $(white $run)"
        ;;

    prepare | prep )
        local usage="USAGE: wb analyse $op [RUN-NAME=current].."

        local name=${1:-current}; if test $# != 0; then shift; fi
        local dir=$(run get "$name")
        test -n "$dir" || fail "malformed run: $name"

        run trim "$name"

        progress "analyse" "preparing run for analysis:  $(with_color white $name)"
        local adir=$dir/analysis
        mkdir -p "$adir/cdf"

        ## 0. ask locli what it cares about
        local keyfile="$adir"/substring-keys
        local key_old=$(sha256sum "$keyfile" | cut -d' ' -f1)
        local tracing_backend=$(jq '.node.tracing_backend // "iohk-monitoring"' --raw-output $dir/profile.json)
        case "$tracing_backend" in
             trace-dispatcher ) locli 'list-logobject-keys'        --keys        "$keyfile";;
             iohk-monitoring  ) locli 'list-logobject-keys-legacy' --keys-legacy "$keyfile";;
             * ) fail "Unknown tracing backend:  $tracing_backend"
        esac
        local key_new=$(sha256sum "$keyfile" | cut -d' ' -f1)

        ## 1. unless already done, filter logs according to locli's requirements
        local logdirs=($(ls -d "$dir"/node-*/ 2>/dev/null))
        local logfiles=($(ls "$adir"/logs-node-*.flt.json 2>/dev/null))
        local run_logs=$adir/log-manifest.json
        local prefilter=$(if   test -z "${logfiles[*]}"
                          then echo 'prefiltered-logs-not-yet-created'
                          elif test "$key_new" != "$key_old"
                          then echo 'prefiltering-keyset-changed'
                          elif test ! -f "$run_logs"
                          then echo 'missing '$run_logs
                          elif test -n "$force_prepare"
                          then echo '--force-prepare passed on CLI'
                          else echo 'false'
                          fi)
        echo "{ \"prefilter\": \"$prefilter\" }"
        if test "$prefilter" = "false"
        then return; fi

        verbose "analyse" "filtering logs:  $(with_color black ${logdirs[@]})"
        local grep_params=(
            --binary-files=text
            --file="$keyfile"
            --fixed-strings
            --no-filename
        )

        echo '{}' > $run_logs
        for d in "${logdirs[@]}"
        do throttle_shell_job_spawns
           local logfiles=($(ls --reverse -t "$d"stdout* "$d"node-[0-9]*.json \
                                2>/dev/null))
           if test -z "${logfiles[*]}"
           then msg "no logs in $d, skipping.."; fi
           local mach=$(basename "$d")
           local  out="$adir"/logs-$mach
           grep ${grep_params[*]} ${logfiles[*]} | grep '^{'  > "$out".flt.json       &
           trace_frequencies_json ${logfiles[*]}              > "$out".tracefreq.json &
           { cat ${logfiles[*]} | sha256sum | cut -d' ' -f1 | xargs echo -n;} > "$out".sha256 &
           jq_fmutate "$run_logs" '
             .rlHostLogs["'"$mach"'"] =
               { hlRawLogfiles:    ["'"$(echo ${logfiles[*]} | sed 's/ /", "/')"'"]
               , hlRawLines:       '"$(cat ${logfiles[*]} | wc -l)"'
               , hlRawSha256:      ""
               , hlRawTraceFreqs:  {}
               , hlLogs:           ["'"$adir/logs-$mach.flt.json"'", null]
               , hlFilteredSha256: ""
               }
           | .rlFilterDate = (now | todate)
           | .rlFilterKeys = ($keys | split("\n"))
           ' --rawfile            keys $keyfile
        done
        wait

        for mach in $(jq_tolist '.rlHostLogs | keys' $run_logs)
        do jq_fmutate "$run_logs" '
             .rlHostLogs[$mach].hlRawSha256      = $raw_sha256
           | .rlHostLogs[$mach].hlRawTraceFreqs  = $freqs[0]
           | .rlHostLogs[$mach].hlFilteredSha256 = $filtered_sha256
           ' --sort-keys                                                         \
             --arg                mach         $mach                             \
             --rawfile      raw_sha256 "$adir"/logs-$mach.sha256                 \
             --arg     filtered_sha256 $(sha256sum < $adir/logs-$mach.flt.json | \
                                          cut -d' ' -f1 | xargs echo -n)         \
             --slurpfile         freqs "$adir"/logs-$mach.tracefreq.json
        done;;

    trace-frequencies | trace-freq | freq | tf )
        local new_only= sargs=()
        while test $# -gt 0
        do case "$1" in
               --new-only ) sargs+=(--new-only);;
               * ) break;; esac; shift; done
        local usage="USAGE: wb analyse $op LOGFILE"

        local logfile=${1:?$usage}; shift


        trace_frequencies_json "$logfile" > "${logfile}.tracefreqs.json"

        local src=$(wc -l <"$logfile")
        local res=$(cut -d' ' -f1 "${logfile}.trace-freqs" |
                        xargs echo |
                        sed 's/ /, /g; s/^/\[/; s/$/\]/' |
                        jq add)
        if test $src != $res
        then local col=red; else local col=green; fi
        progress    "trace-freq" "total in source: $(white           $src)"
        progress_ne "trace-freq" "total counted:   $(with_color $col $res)";;

    chain-rejecta-reasons | chain-rejecta | rejecta )
        local usage="USAGE: wb analyse $op [RUN-NAME=current]"

        local name=${1:-current}; if test $# != 0; then shift; fi
        local dir=$(run get "$name")
        test -n "$dir" || fail "malformed run: $name"
        local rejecta=$dir/analysis/chain-rejecta.json

        jq '.beBlockNo as $no
          | .beAcceptance
          | { block: $no
            , nacks: map( select(.[1] == false)
                        | .[0].contents.tag)
            }
          '   $rejecta --compact-output
        wc -l $rejecta
        ;;

    * ) progress "analyse" "unexpected 'analyse' subop:  $(red $op)"
        usage_analyse;; esac
}

call_locli() {
    local rtsmode="$1"; shift
    local args=("$@")

    if test -z "$rtsmode"
    then if curl --connect-timeout 0.5 http://169.254.169.254/latest/meta-data >/dev/null 2>&1
         then rtsmode='aws'
         else rtsmode='hipar'; fi; fi

    echo "{ \"rtsmode\": \"$rtsmode\" }"
    case "$rtsmode" in
        aws )   ## Work around the odd parallelism bug killing performance on AWS:
                locli_args+=(+RTS -N1 -A128M -RTS);;
        lomem ) locli_args+=(+RTS -N3 -A8M -RTS);;
        hipar ) locli_args+=();;
        * )     fail "unknown rtsmode: $rtsmode";;
    esac

    verbose "analysis | locli" "$(with_color reset ${locli_args[@]}) $(colorise "${ops_final[@]}")"
    time locli "${locli_args[@]}" "${args[@]}"
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
    local filter_paths=(${filter_names[*]/#/"$global_basedir/analyse/chain-filters/"})
    local filter_files=(${filter_paths[*]/%/.json})

    for f in ${filter_files[*]}
    do test -f "$f" ||
            fail "no such filter: $f"; done

    progress "analysis" "filter files: $(yellow ${filter_files[*]})"
    filters+=(${filter_files[*]/#/--filter })
}

trace_frequencies_json() {
    grep --no-filename '^{.*}$' "$@" |
    jq 'reduce inputs as $line
          ( {};
            ( $line
            | (try .ns[0] // .ns)
              + ":"
              + (if .data | type != "object" then .data | type
                else (.data.kind //.data.val.kind) end)
            )              as $key
          | (.[$key] // 0) as $acc
          | . + { "\($key)": ($acc + 1) }
          # | .[$key] += 1              ## This is somehow slower than set addition..
          )
       '
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
             { inherit
               TraceAcceptPolicy
               TraceBlockFetchClient
               TraceBlockFetchDecisions
               TraceBlockFetchProtocol
               TraceBlockFetchProtocolSerialised
               TraceBlockFetchServer
               TraceChainDb
               TraceChainSyncBlockServer
               TraceChainSyncClient
               TraceChainSyncHeaderServer
               TraceChainSyncProtocol
               TraceConnectionManager
               TraceDNSResolver
               TraceDNSSubscription
               TraceDiffusionInitialization
               TraceErrorPolicy
               TraceForge
               TraceHandshake
               TraceInboundGovernor
               TraceIpSubscription
               TraceLedgerPeers
               TraceLocalChainSyncProtocol
               TraceLocalErrorPolicy
               TraceLocalHandshake
               TraceLocalRootPeers
               TraceLocalTxSubmissionProtocol
               TraceLocalTxSubmissionServer
               TraceMempool
               TraceMux
               TracePeerSelection
               TracePeerSelectionActions
               TracePublicRootPeers
               TraceServer
               TraceTxInbound
               TraceTxOutbound
               TraceTxSubmissionProtocol
               TracingVerbosity
               TurnOnLogMetrics
               TurnOnLogging
               defaultBackends
               defaultScribes
               hasEKG
               hasPrometheus
               minSeverity
               options
               rotation
               setupBackends
               setupScribes;
             }'
    )
    nix eval "${nix_eval_args[@]}" | jq --sort-keys
}
