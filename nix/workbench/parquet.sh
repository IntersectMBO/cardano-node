# shellcheck shell=bash

## Run logs as Parquet: the workbench side of bench/bench-parquet-cli.
## The CLI knows nothing about runs (it works on a directory of Parquet files,
## PQDIR); this module resolves runs and passes what the run knows.

usage_parquet() {
     usage "parquet" "Run logs as Parquet, queried with DuckDB (bench/bench-parquet-cli)" <<EOF
    $(helpcmd create RUN [--force] [--delete] [--jobs N] [--keep-temp] [--zstd-level N])
                          Convert the stdout logs of node-*, generator and
                            explorer (where present) into RUN/parquets/,
                            losslessly; a log already in parquets/ is skipped.
                            Query sessions read node-* and generator only.
                            --force      replace an existing parquets/ (convert all)
                            --delete     then delete each stdout whose log verifies
                                         (rebuilt from parquets/, sha256 as recorded)
                            --jobs N     slots (default: from memory and cores); a log larger than
                                         total size / slots gets its share of the slots
                            --keep-temp  keep the temporary directory when a conversion fails
                            --zstd-level N  zstd compression level

    $(helpcmd query RUN [--ui] [--load PATH].. [DUCKDB-ARGS..])
                          Interactive DuckDB session over the run's logs;
                            --ui         also opens the DuckDB UI in a new browser tab
                            --load PATH  also reads a SQL file or directory (repeatable)
    $(helpcmd run RUN [--load PATH].. FILE.sql)
    $(helpcmd run RUN [--load PATH].. NAME)
                          Run a SQL file, or a prepared query by name
                            (sql/queries/*/NAME.sql), in such a session
    $(helpcmd head RUN PROCESS [-n N])
    $(helpcmd tail RUN PROCESS [-n N])
                          First / last N lines (default 20) of a process's log
    $(helpcmd excerpt RUN ARGS..)  Log lines in a time window, one file per process
    $(helpcmd restore RUN [PROCESS..] [--jobs N])
                          Write each missing stdout (node-*, generator, explorer)
                            back into the run, verified against the recorded sha256;
                            an existing stdout is skipped
    $(helpcmd restore RUN PROCESS -o FILE [--verify])
                          Reconstruct a process's log into FILE
    $(helpcmd verify RUN [PROCESS..] [--jobs N])
                          Check logs rebuilt from parquets/ against the sha256
                            recorded at conversion; writes nothing
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
        local usage="USAGE: wb parquet $op RUN [--force] [--delete] [--jobs N] [--keep-temp] [--zstd-level N]"
        local run=${1:?$usage}; shift
        local dir=$(run get "$run")
        local force= delete= jobs_args=() conv_args=()
        while test $# -gt 0
        do case "$1" in
               --force )  force=t; conv_args+=("$1");;
               --delete ) delete=t;;
               --jobs )   jobs_args=("$1" "${2:?$usage}"); shift;;
               * )        conv_args+=("$1");;
           esac; shift; done

        ## The run's knowledge goes in as arguments: which logs (the stdout of
        ## node-*, generator and explorer), the run id, genesis values. The
        ## explorer's log is archived only: query sessions read node-* and
        ## generator (bench-parquet's naming contract).
        local logs=() procs=() errs=() d p
        for d in "$dir"/node-* "$dir"/generator "$dir"/explorer
        do test -d "$d" || continue
           p=$(basename "$d")
           test -f "$d"/stdout && { logs+=("$p=$d/stdout"); procs+=("$p"); }
           test -s "$d"/stderr && errs+=("$p/stderr")
        done
        if test ${#logs[@]} -eq 0
        then test -f "$dir"/parquets/meta/files.parquet ||
                 fail "no node-*/stdout, generator/stdout or explorer/stdout in $dir"
             progress "parquet | create" "no stdout left in $dir; parquets/ has the logs"
             return 0
        fi
        local genesis=$dir/genesis/genesis.shelley.json
        test -f "$genesis" || genesis=$dir/genesis-shelley.json
        test -f "$genesis" ||
            fail "no Shelley genesis in $dir (genesis/genesis.shelley.json)"
        local run_id=$(jq -r '.meta.tag // empty' "$dir"/meta.json)
        test -n "$run_id" || run_id=$(basename "$dir")

        ## Per log: one already in parquets/ is skipped (--add), unless --force.
        local add=(--add); test -z "$force" || add=()
        bench_parquet convert "$dir"/parquets \
            --run-id       "$run_id" \
            --system-start "$(jq -r .systemStart "$genesis")" \
            --slot-length  "$(jq -r .slotLength  "$genesis")" \
            --epoch-length "$(jq -r .epochLength "$genesis")" \
            "${add[@]}" "${jobs_args[@]}" "${conv_args[@]}" "${logs[@]}" || exit 1

        ## stderr is not converted (no JSON); sizes only, no judgement
        ## (processes may recover from what they report there).
        if test ${#errs[@]} -gt 0
        then progress "parquet | create" "non-empty stderr (not converted):"
             for p in "${errs[@]}"
             do echo "  $p: $(stat -L -c%s "$dir/$p") bytes, $(wc -l < "$dir/$p") lines" >&2; done
        else progress "parquet | create" "stderr: empty or absent for all processes"
        fi

        ## --delete: a stdout goes only once its log, rebuilt from parquets/,
        ## matches the sha256 recorded at conversion; one that doesn't stays.
        if test -n "$delete"
        then local verdict kept=() deleted=0
             local -A ok=()
             while IFS=$'\t' read -r p verdict
             do test "$verdict" != ok || ok[$p]=t
             done < <(bench_parquet verify "$dir"/parquets "${procs[@]}" "${jobs_args[@]}")
             for p in "${procs[@]}"
             do if test -n "${ok[$p]:-}"
                then rm -f "$dir/$p/stdout"; deleted=$(( deleted + 1 ))
                else kept+=("$p"); fi
             done
             if test ${#kept[@]} -gt 0
             then fail "deleted $deleted stdout files; kept the stdout of ${kept[*]}: not verified against parquets/"
             else progress "parquet | create" "deleted the stdout of $deleted processes (verified against parquets/)"
             fi
        fi
        ;;

    restore )
        ## With -o: the CLI's restore into FILE. Without: back into the run.
        local a
        for a in "$@"
        do if test "$a" = -o
           then local run=${1:?USAGE: wb parquet $op RUN PROCESS -o FILE [--verify]}; shift
                local pq; pq=$(parquet_dir "$run") || exit 1
                bench_parquet restore "$pq" "$@"; return
           fi
        done
        local run=${1:?USAGE: wb parquet $op RUN [PROCESS..] [--jobs N]}; shift
        local dir; dir=$(run get "$run") || exit 1
        local pq; pq=$(parquet_dir "$run") || exit 1
        local procs=() jobs_args=() p f
        while test $# -gt 0
        do case "$1" in
               --jobs ) jobs_args=("$1" "${2:?--jobs needs a number}"); shift;;
               * )      procs+=("$1");;
           esac; shift; done
        if test ${#procs[@]} -eq 0
        then for f in "$pq"/*.parquet; do procs+=("$(basename "$f" .parquet)"); done; fi
        ## Each into its process directory; the CLI skips an existing stdout and
        ## renames a restored one into place only once verified.
        local targets=()
        for p in "${procs[@]}"; do targets+=("$p=$dir/$p/stdout"); done
        bench_parquet restore "$pq" "${targets[@]}" "${jobs_args[@]}"
        ;;

    query | run | head | tail | excerpt | verify | lines | host-summary )
        local run=${1:?USAGE: wb parquet $op RUN ARGS..}; shift
        local pq; pq=$(parquet_dir "$run") || exit 1
        bench_parquet "$op" "$pq" "$@";;

    path )
        local run=${1:?USAGE: wb parquet $op RUN}
        parquet_dir "$run";;

    * ) usage_parquet;; esac
}
