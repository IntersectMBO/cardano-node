usage_analyse() {
     usage "analyse" "Analyse cluster runs" <<EOF
    machine RUN-NAME MACH-NAME
                          Analyse logs for MACH-NAME

    whole-cluster RUN-NAME
                          Analyse logs for the entire cluster run

EOF
}

analyse() {
local op=${1:-$(usage_analyse)}; shift

case "$op" in
    machine | mach | m )
        local usage="USAGE: wb analyse $op [RUN-NAME=current] [MACH-NAME=node-1]"
        local name=${1:-current}
        local mach=${2:-node-1}
        local dir=$(run get "$name")
        local adir=$dir/analysis

        mkdir -p "$adir"

        ## 0. subset what we care about into the keyfile
        local keyfile=$adir/substring-keys
        locli analyse substring-keys > "$keyfile"

        ## 1. enumerate logs, filter by keyfile & consolidate
        local logs=("$dir"/$mach/stdout) consolidated="$adir"/logs-$mach.json
        grep -hFf "$keyfile" "${logs[@]}"  > "$consolidated"
        msg "analysing logs of:  $mach  (lines: $(wc -l "$consolidated"))"

        local locli_args=(
            --genesis         "$dir"/genesis/genesis.json
            --run-metafile    "$dir"/meta.json
            --logobjects-json "$adir"/logs-$mach.logobjects.json
            --slotstats-json  "$adir"/logs-$mach.slotstats.json
            --timeline-pretty "$adir"/logs-$mach.timeline.txt
            --stats-csv       "$adir"/logs-$mach.stats.csv
            --analysis-json   "$adir"/logs-$mach.analysis.json
            # --timeline-csv            "$adir"/logs-$mach.timeline.csv
            # --cpu-spans-histogram-png "$adir"/logs-"$mach".cpu85-span-lens.png
            # --derived-vectors-0-csv   "$adir"/logs-$mach".derived.1.csv
            # --derived-vectors-1-csv   "$adir"/logs-$mach.derived.1.csv
        )

        locli 'analyse' 'perf-timeline' \
            "${locli_args[@]}" "$consolidated";;

    * ) usage_analyse;; esac
}
