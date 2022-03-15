usage_analyse() {
     usage "analyse" "Analyse cluster runs" <<EOF
    standard RUN-NAME..   Standard batch of analyses: block-propagation, and
                            machine-timeline

    block-propagation RUN-NAME..
                          Block propagation analysis for the entire cluster.

    machine-timeline RUN-NAME [MACH-NAME=node-1]
                          Performance timeline for MACH-NAME.

    chaininfo RUN-NAME    Print basic parameters of a run, as seen by locli.

    Options of 'analyse' command:

       --filters F,F,F..  Comma-separated list of named chain filters:  see bench/chain-filters
                            Note: filter names have no .json suffix
       --dump-logobjects  Dump the intermediate data: lifted log objects
EOF
}

analyse() {
local dump_logobjects= force_prefilter= prefilter_jq= self_args=() locli_args=() filters=() aws= outdir=
while test $# -gt 0
do case "$1" in
       --dump-logobjects )  dump_logobjects='true';  self_args+=($1);;
       --force-prefilter )  force_prefilter='true';  self_args+=($1);;
       --prefilter-jq )     prefilter_jq='true';     self_args+=($1);;
       --filters )          local filter_names=('base')
                            filter_names+=($(echo $2 | sed 's_,_ _'))
                            local filter_paths=(${filter_names[*]/#/"$global_basedir/chain-filters/"})
                            local filter_files=(${filter_paths[*]/%/.json})
                            for f in ${filter_files[*]}
                            do test -f "$f" ||
                                    fail "no such filter: $f"; done
                            locli_args+=(${filter_files[*]/#/--filter })
                            self_args+=($1 $2); shift;;
       --rts )              self_args+=($1 $2); locli_args+=(+RTS $2         -RTS); shift;;
       --outdir )           self_args+=($1 "$2"); outdir=$2; shift;;
       * ) break;; esac; shift; done

if ! curl --connect-timeout=0.5 http://169.254.169.254/latest/meta-data 2>&1 | grep --quiet 404
then aws='true'; fi

## Work around the odd parallelism bug killing performance on AWS:
if test -n "$aws"
then locli_args+=(+RTS -N1 -A128M -RTS)
     echo "{ \"aws\": true }"
else echo "{ \"aws\": false }"
fi

local op=${1:-$(usage_analyse)}; shift

case "$op" in
    chaininfo | ci )
        local name=${1:-current}; shift
        local dir=$(run get "$name")

        locli_args+=(
            --genesis         "$dir"/genesis-shelley.json
            --run-metafile    "$dir"/meta.json
        )

        time locli 'analyse' 'chaininfo' "${locli_args[@]}"
        ;;
    standard | std )
        for r in $*
        do analyse ${self_args[*]} block-propagation $r
           analyse ${self_args[*]} machine-timeline  $r
        done
        ;;
    block-propagation | bp )
        local usage="USAGE: wb analyse $op [RUN-NAME=current].."

        local name=${1:-current}; shift
        local dir=$(run get "$name")
        test -n "$dir" || fail "malformed run: $name"

        local adir=${outdir:-$dir/analysis}
        mkdir -p "$adir"

        ## 0. subset what we care about into the keyfile
        local keyfile="$adir"/substring-keys
        locli analyse substring-keys > "$keyfile"

        ## 1. enumerate logs, filter by keyfile & consolidate
        local logdirs=($(ls -d "$dir"/node-*/ 2>/dev/null))
        local logfiles=($(ls "$adir"/logs-node-*.flt.json 2>/dev/null))
        # echo logfiles: _${logfiles[*]}_
        local prefilter=$(test "$force_prefilter" = 'true' -o -z "${logfiles[*]}" && echo 'true' || echo 'false')

        echo "{ \"prefilter\": $prefilter }"
        if test x$prefilter = xtrue
        then
            local jq_args=(
                --sort-keys
                --compact-output
                $(wb backend lostream-fixup-jqargs "$dir")
                ' delpaths([["app"],["env"],["loc"],["msg"],["ns"],["sev"]])
                '"$(wb backend lostream-fixup-jqexpr)"
            )
            for d in "${logdirs[@]}"
            do throttle_shell_job_spawns
               local logfiles="$(ls "$d"/stdout* 2>/dev/null | tac) $(ls "$d"/node-*.json 2>/dev/null)"
               if test -z "$logfiles"
               then msg "no logs in $d, skipping.."; fi
               local output="$adir"/logs-$(basename "$d").flt.json
               grep -hFf "$keyfile" $logfiles |
                   if test "$prefilter_jq" = 'true'
                   then jq "${jq_args[@]}" --arg dirHostname "$(basename "$d")"
                   else cat
                   fi > "$output" &
            done
            wait
        fi

        locli_args+=(
            --genesis         "$dir"/genesis-shelley.json
            --run-metafile    "$dir"/meta.json
            ## ->
            --timeline-pretty "$adir"/block-propagation.txt
            --analysis-json   "$adir"/block-propagation.json
            $(if test -n "$dump_logobjects"
              then echo --logobjects-json "$adir"/logs-cluster.logobjects.json; fi)
        )

        time locli 'analyse' 'block-propagation' \
             "${locli_args[@]}" "$adir"/*.flt.json

        ## More than one run passed?
        if test $# -gt 0
        then analyse ${self_args[*]} block-propagation "$@"; fi;;

    machine-timeline | machine | mt )
        local usage="USAGE: wb analyse $op [RUN-NAME=current] [MACH-NAME=node-1]"
        local name=${1:-current}
        local mach=${2:-node-1}
        local dir=$(run get "$name")
        test -n "$dir" || fail "malformed run: $name"

        local adir=${outdir:-$dir/analysis}
        mkdir -p "$adir"

        ## 0. subset what we care about into the keyfile
        local keyfile=$adir/substring-keys
        locli analyse substring-keys > "$keyfile"

        if test "$mach" = 'all'
        then local machs=($(run list-hosts $name))
        else local machs=($mach); fi

        for mach in ${machs[*]}
        do throttle_shell_job_spawns
           (
           ## 1. enumerate logs, filter by keyfile & consolidate
               local logs=($(ls "$dir"/$mach/node-*.json 2>/dev/null))
               local consolidated="$adir"/logs-$mach.flt.json

           test -n "${logs[*]}" ||
               fail "no logs for $mach in run $name"

           local prefilter=$(test "$force_prefilter" = 'true' -o -z "$(ls "$adir"/logs-node-*.flt.json 2>/dev/null)" && echo 'true' || echo 'false')

           echo "{ \"prefilter\": $prefilter }"
           if test x$prefilter = 'xtrue'
           then grep -hFf "$keyfile" "${logs[@]}"  > "$consolidated"; fi

           locli_args+=(
               --genesis         "$dir"/genesis-shelley.json
               --run-metafile    "$dir"/meta.json
               ## ->
               --timeline-pretty "$adir"/logs-$mach.timeline.txt
               --stats-csv       "$adir"/logs-$mach.stats.csv
               --analysis-json   "$adir"/logs-$mach.analysis.json
               # --slotstats-json  "$adir"/logs-$mach.slotstats.json
               # --timeline-csv            "$adir"/logs-$mach.timeline.csv
               # --cpu-spans-histogram-png "$adir"/logs-"$mach".cpu85-span-lens.png
               # --derived-vectors-0-csv   "$adir"/logs-$mach".derived.1.csv
               # --derived-vectors-1-csv   "$adir"/logs-$mach.derived.1.csv
           )
           if test -n "$dump_logobjects"; then
               locli_args+=(--logobjects-json "$adir"/logs-$mach.logobjects.json); fi

           time locli 'analyse' 'machine-timeline' \
                "${locli_args[@]}" "$consolidated"
           ) &
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
