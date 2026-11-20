# shellcheck shell=bash

## Run logs as Parquet: the workbench side of bench/bench-parquet-cli.
## The CLI knows nothing about runs (it works on a directory of Parquet files,
## PQDIR); this module resolves runs and passes what the run knows.

usage_parquet() {
     usage "parquet" "Run logs as Parquet, queried with DuckDB (bench/bench-parquet-cli)" <<EOF
    $(helpcmd create RUN [--force] [--jobs N] [--keep-temp] [--zstd-level N])
                          Convert the stdout logs of node-* and generator
                            into RUN/parquets/, losslessly.
                            --force      replace an existing parquets/
                            --jobs N     slots (default: from memory and cores); a log larger than
                                         total size / slots gets its share of the slots
                            --keep-temp  keep the temporary directory when a conversion fails
                            --zstd-level N  zstd compression level

    $(helpcmd query RUN [--ui] [DUCKDB-ARGS..])
                          Interactive DuckDB session over the run's logs;
                            --ui also opens the DuckDB UI in a new browser tab
    $(helpcmd run RUN FILE.sql)    Run a SQL file in such a session
    $(helpcmd head RUN PROCESS [-n N])
    $(helpcmd tail RUN PROCESS [-n N])
                          First / last N lines (default 20) of a process's log
    $(helpcmd excerpt RUN ARGS..)  Log lines in a time window, one file per process
    $(helpcmd restore RUN PROCESS -o FILE [--verify])
                          Reconstruct a process's log
    $(helpcmd lines RUN PROCESS [--ns-file FILE])
    $(helpcmd host-summary RUN PROCESS)
                          Input for locli prepare-db-from-lines
    $(helpcmd path RUN)            The run's Parquet directory

  Details of each command: $(yellow bench-parquet --help).
EOF
}

## The CLI itself; any arguments.
bench_parquet() {
    "${WB_CARDANO_NODE_REPO_ROOT:-$global_basedir/../..}"/bench/bench-parquet-cli/bench-parquet "$@"
}

## The Parquet directory of a run, which must exist.
parquet_dir() {
    local run=$1
    local dir; dir=$(run get "$run") || exit 1
    test -f "$dir"/parquets/meta/files.parquet ||
        fail "no Parquet logs in $dir (create them: wb parquet create $run)"
    echo "$dir"/parquets
}

parquet() {
local op=${1:-}; test $# -eq 0 || shift

case "$op" in

    create )
        local usage="USAGE: wb parquet $op RUN [--force] [--jobs N] [--keep-temp] [--zstd-level N]"
        local run=${1:?$usage}; shift
        local dir=$(run get "$run")

        ## The run's knowledge goes in as arguments: which logs (the stdout of
        ## node-* and generator), the run id, genesis timing.
        local logs=() errs=() d p
        for d in "$dir"/node-* "$dir"/generator
        do test -d "$d" || continue
           p=$(basename "$d")
           test -f "$d"/stdout && logs+=("$p=$d/stdout")
           test -s "$d"/stderr && errs+=("$p/stderr")
        done
        test ${#logs[@]} -gt 0 ||
            fail "no node-*/stdout or generator/stdout in $dir"
        local genesis=$dir/genesis/genesis.shelley.json
        test -f "$genesis" || genesis=$dir/genesis-shelley.json
        test -f "$genesis" ||
            fail "no Shelley genesis in $dir (genesis/genesis.shelley.json)"
        local run_id=$(jq -r '.meta.tag // empty' "$dir"/meta.json)
        test -n "$run_id" || run_id=$(basename "$dir")

        bench_parquet convert "$dir"/parquets \
            --run-id       "$run_id" \
            --system-start "$(jq -r .systemStart "$genesis")" \
            --slot-length  "$(jq -r .slotLength  "$genesis")" \
            "$@" "${logs[@]}"

        ## stderr is not converted (no JSON); sizes only, no judgement
        ## (processes may recover from what they report there).
        if test ${#errs[@]} -gt 0
        then progress "parquet | create" "non-empty stderr (not converted):"
             for p in "${errs[@]}"
             do echo "  $p: $(stat -L -c%s "$dir/$p") bytes, $(wc -l < "$dir/$p") lines" >&2; done
        else progress "parquet | create" "stderr: empty or absent for all processes"
        fi
        ;;

    query | run | head | tail | excerpt | restore | lines | host-summary )
        local run=${1:?USAGE: wb parquet $op RUN ARGS..}; shift
        local pq; pq=$(parquet_dir "$run") || exit 1
        bench_parquet "$op" "$pq" "$@";;

    path )
        local run=${1:?USAGE: wb parquet $op RUN}
        parquet_dir "$run";;

    * ) usage_parquet;; esac
}
