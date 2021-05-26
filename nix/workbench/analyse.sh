usage_analyse() {
     usage "analyse" "Analyse cluster runs" <<EOF
    block-propagation RUN-NAME
                          Block propagation analysis for the entire cluster.

    machine-timeline RUN-NAME MACH-NAME
                          Produce a general performance timeline for MACH-NAME

    Options of 'analyse' command:

       --reanalyse        Skip the preparatory steps and launch 'locli' directly
       --time             Time the 'locli' executable runs
EOF
}

analyse() {
local skip_preparation= time= dump_logobjects=
while test $# -gt 0
do case "$1" in
       --reanalyse | --re ) skip_preparation='true';;
       --dump-logobjects )  dump_logobjects='true';;
       --time )             time='eval time';;
       * ) break;; esac; shift; done

local op=${1:-$(usage_analyse)}; shift

case "$op" in
    block-propagation | bp )
        local usage="USAGE: wb analyse $op [RUN-NAME=current]"

        local name=${1:-current}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        msg "analysing run $(jq .meta.tag "$dir"/meta.json --raw-output)"
        mkdir -p "$adir"

        ## 0. subset what we care about into the keyfile
        local keyfile=$adir/substring-keys
        locli analyse substring-keys | grep -v 'Temporary modify' > "$keyfile"

        ## 1. enumerate logs, filter by keyfile & consolidate
        local logdirs=("$dir"/node-*/)

        if test -z "$skip_preparation" -o -z "$(ls "$adir"/logs-node-*.flt.json 2>/dev/null)"
        then
            msg "filtering logs in: $dir/node-* "
            local jq_args=(
                --sort-keys
                --compact-output
                $(wb backend lostream-fixup-jqargs "$dir")
                ' delpaths([["app"],["env"],["loc"],["msg"],["ns"],["sev"]])
                '"$(wb backend lostream-fixup-jqexpr)"
            )
            for d in "${logdirs[@]}"
            do ## TODO: supervisor-specific logfile layout
                grep -hFf "$keyfile" $(ls "$d"/stdout* | tac) | jq "${jq_args[@]}" --arg dirHostname "$(basename "$d")" > \
                "$adir"/logs-$(basename "$d").flt.json &
            done
            wait
        fi

        msg "log sizes:  (files: $(ls "$adir"/*.flt.json 2>/dev/null | wc -l), lines: $(cat "$adir"/*.flt.json | wc -l))"

        msg "analysing.."
        local locli_args=(
            --genesis         "$dir"/genesis.json
            --run-metafile    "$dir"/meta.json
            ## ->
            --timeline-pretty "$adir"/block-propagation.txt
            --analysis-json   "$adir"/block-propagation.json
        )
        if test -n "$dump_logobjects"; then
            locli_args+=(--logobjects-json "$adir"/logs-cluster.logobjects.json); fi

        ${time} locli 'analyse' 'block-propagation' \
            "${locli_args[@]}" "$adir"/*.flt.json;;

    grep-filtered-logs | grep | g )
        local usage="USAGE: wb analyse $op BLOCK [MACHSPEC=*] [RUN-NAME=current]"
        local expr=$1
        local mach=${2:-*}
        local name=${3:-current}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        grep -h "$expr" "$adir"/logs-$mach.flt.json;;

    list-blocks | blocks | bs )
        local usage="USAGE: wb analyse $op [RUN-NAME=current]"
        local name=${1:-current}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        fgrep -h "TraceForgedBlock" "$adir"/*.flt.json |
            jq '{ at: .at, host: .host } * .data | del(.peer) | del(.slot)' -c |
            sort | uniq;;

    block-propagation-block | bpb )
        local usage="USAGE: wb analyse $op BLOCK [RUN-NAME=current]"
        local block=$1
        local name=${2:-current}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        grep -h "$block" "$adir"/*.flt.json |
            grep 'AddBlock\|TraceForgedBlock\|AddedToCurrentChain' |
            jq '{ at: .at, host: .host } * .data | del(.peer) | del(.slot)' -c |
            sort --stable | uniq;;

    machine-timeline | machine | mt )
        local usage="USAGE: wb analyse $op [RUN-NAME=current] [MACH-NAME=node-1]"
        local name=${1:-current}
        local mach=${2:-node-1}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        msg "analysing run $(jq .meta.tag "$dir"/meta.json --raw-output)"
        mkdir -p "$adir"

        ## 0. subset what we care about into the keyfile
        local keyfile=$adir/substring-keys
        locli analyse substring-keys | grep -v 'Temporary modify' > "$keyfile"

        ## 1. enumerate logs, filter by keyfile & consolidate
        local logs=("$dir"/$mach/stdout) consolidated="$adir"/logs-$mach.json
        if test -z "$skip_preparation" -o -z "$(ls "$adir"/logs-$mach.json 2>/dev/null)"
        then
            grep -hFf "$keyfile" "${logs[@]}"  > "$consolidated"
        fi

        msg "analysing logs of:  $mach  (lines: $(wc -l "$consolidated"))"
        local locli_args=(
            --genesis         "$dir"/genesis.json
            --run-metafile    "$dir"/meta.json
            ## ->
            --slotstats-json  "$adir"/logs-$mach.slotstats.json
            --timeline-pretty "$adir"/logs-$mach.timeline.txt
            --stats-csv       "$adir"/logs-$mach.stats.csv
            --analysis-json   "$adir"/logs-$mach.analysis.json
            # --timeline-csv            "$adir"/logs-$mach.timeline.csv
            # --cpu-spans-histogram-png "$adir"/logs-"$mach".cpu85-span-lens.png
            # --derived-vectors-0-csv   "$adir"/logs-$mach".derived.1.csv
            # --derived-vectors-1-csv   "$adir"/logs-$mach.derived.1.csv
        )
        if test -n "$dump_logobjects"; then
            locli_args+=(--logobjects-json "$adir"/logs-$mach.logobjects.json); fi

        ${time} locli 'analyse' 'machine-timeline' \
            "${locli_args[@]}" "$consolidated";;

    * ) usage_analyse;; esac
}
