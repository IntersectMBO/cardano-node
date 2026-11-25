# shellcheck shell=bash
# bench-parquet: query sessions: query, run, check.
# Sourced by bench-parquet: uses its helpers (msg, die, sqlq, ...) and
# variables (self_dir, sql_dir, DUCKDB, ...).

# Write the session init script for a run to FILE.
session_init() {
    local pq=$1 file=$2
    compgen -G "$pq/node-*.parquet" >/dev/null ||
        die "no node-*.parquet in $pq: query sessions need at least one node log (process named node-*)"
    local pql session
    pql=$(sqlq "$pq")
    session=$(<"$sql_dir/session.sql")
    {
        # Resource bounds (remote sessions set them; see remote_exec).
        test -z "${BENCH_PARQUET_MEMORY:-}"  || echo "SET memory_limit = $(sqlq "$BENCH_PARQUET_MEMORY");"
        test -z "${BENCH_PARQUET_THREADS:-}" || echo "SET threads = $BENCH_PARQUET_THREADS;"
        test -z "${BENCH_PARQUET_SPILL:-}"   || echo "SET temp_directory = $(sqlq "$BENCH_PARQUET_SPILL");"
        test -z "${BENCH_PARQUET_SPILL_LIMIT:-}" ||
            echo "SET max_temp_directory_size = $(sqlq "$BENCH_PARQUET_SPILL_LIMIT");"
        echo ".read $(sqlq "$sql_dir/lib.sql")"
        # Quoted replacement: no & expansion (bash 5.2 patsub_replacement).
        printf '%s\n' "${session//@PARQUETS@/"$pql"}"
        if test -f "$pq/generator.parquet"
        then echo "CREATE OR REPLACE VIEW raw_generator AS SELECT * FROM read_parquet($(sqlq "$pq/generator.parquet"));"
        else echo "CREATE OR REPLACE VIEW raw_generator AS SELECT * FROM raw WHERE false;"
        fi
        # One view per namespace the nodes emit, named by the quoted namespace.
        # Nodes only: a generator namespace never shadows or mixes into these.
        "$DUCKDB" -init /dev/null -noheader -list -c "
            SELECT DISTINCT ns FROM read_parquet($(sqlq "$pq/node-*.parquet"))
            WHERE ns IS NOT NULL ORDER BY ns;" |
        while IFS= read -r ns
        do echo "CREATE OR REPLACE VIEW $(sqli "$ns") AS SELECT * FROM raw WHERE ns = $(sqlq "$ns");"
        done
        # Package views by tier, then the checks (sql/README.md): a view uses only
        # lower tiers, and DuckDB binds a view's references at creation.
        local f
        for f in "$sql_dir"/views/[0-9]*/*.sql "$sql_dir"/checks/*.sql
        do test -f "$f" && echo ".read $(sqlq "$f")"; done
    } > "$file"
}

with_session() {
    local pq=$1; shift
    pq=$(realpath -m "$pq")
    check_pqdir "$pq"
    local init status=0
    init=$(mktemp --suffix=.sql)
    session_init "$pq" "$init"
    "$@" "$init" || status=$?
    rm -f "$init"
    return $status
}

# --load PATH: the .read lines for a SQL file, for a directory every *.sql
# directly in it, in byte order of the file names (independent of the locale).
# Bail mode off first, and left off: an error in one of them is printed and the
# session goes on (user_sql/README.md). DuckDB ends a session whose init file
# had an error unless bail mode is off when the init file ends.
load_reads() {
    local p f
    test $# -gt 0 || return 0
    echo ".bail off"
    for p in "$@"
    do if test -d "$p"
       then while IFS= read -r -d '' f
            do echo ".read $(sqlq "$f")"
            done < <(find "$p" -maxdepth 1 -type f -name '*.sql' -print0 | LC_ALL=C sort -z)
       else echo ".read $(sqlq "$p")"
       fi
    done
}

# A --load argument, checked and made absolute (sessions run elsewhere).
load_path() {
    test -n "$1" || die "--load needs a path"
    test -f "$1" -o -d "$1" || die "--load: no such file or directory: $1"
    realpath "$1"
}

query_cmd() {
    local pq=${1:?query: PQDIR missing}; shift
    local ui= args=() loads=()
    while test $# -gt 0
    do case "$1" in
           --ui )   ui=t;;
           --load ) loads+=("$(load_path "${2:-}")") || exit 1; shift;;
           * )      args+=("$1");;
       esac; shift; done
    run_query() {
        load_reads "${loads[@]}" >> "$1"
        test -z "$ui" || echo ".read $(sqlq "$sql_dir/ui.sql")" >> "$1"
        "$DUCKDB" -init "$1" "${args[@]}"
    }
    with_session "$pq" run_query
}

run_cmd() {
    local pq=${1:?run: PQDIR missing}; shift
    # Results in full: no row limit on tables (DuckDB shows 40 by default).
    local file= loads=() mode=(-cmd ".maxrows -1")
    while test $# -gt 0
    do case "$1" in
           --load ) loads+=("$(load_path "${2:-}")") || exit 1; shift;;
           --csv )  mode+=(-csv);;
           -* )     die "run: unknown option: $1";;
           * )      test -z "$file" || die "run: more than one FILE.sql"; file=$1;;
       esac; shift; done
    test -n "$file" || die "run: FILE.sql or a prepared query's NAME missing"
    if [[ $file != */* && $file != *.sql ]]
    then # A prepared query by name.
         local found=() f
         for f in "$sql_dir"/queries/*/"$file".sql; do test -f "$f" && found+=("$f"); done
         if test ${#found[@]} -ne 1
         then local names=()
              for f in "$sql_dir"/queries/*/*.sql; do names+=("$(basename "$f" .sql)"); done
              test ${#found[@]} -eq 0 ||
                  die "run: $file names several prepared queries: ${found[*]#"$sql_dir"/}"
              die "run: no prepared query $file; there are: ${names[*]}"
         fi
         file=${found[0]}
    fi
    test -f "$file" || die "no such file: $file"
    file=$(realpath "$file")
    run_file() {
        if test ${#loads[@]} -eq 0
        then "$DUCKDB" -init /dev/null "${mode[@]}" -c ".read $(sqlq "$1")" -c ".read $(sqlq "$file")"
        else load_reads "${loads[@]}" >> "$1"
             # As the init file (see load_reads); FILE itself stops at its first
             # error, with a non-zero exit status.
             "$DUCKDB" -init "$1" "${mode[@]}" -cmd ".bail on" -c ".read $(sqlq "$file")"
        fi
    }
    with_session "$pq" run_file
}

check_cmd() {
    local pq=${1:?check: PQDIR missing}; shift
    local all=() names=() n f
    for f in "$sql_dir"/checks/*.sql; do all+=("$(basename "$f" .sql)"); done
    if test $# -eq 0
    then names=("${all[@]}")
    else for n in "$@"
         do n=chk_${n#chk_}
            [[ " ${all[*]} " == *" $n "* ]] || die "check: no check $n (checks: ${all[*]})"
            names+=("$n")
         done
    fi
    # Each check evaluated once (a materialized CTE), its rows or one "none" row.
    local ctes=() selects=()
    for n in "${names[@]}"
    do ctes+=("$n AS MATERIALIZED (FROM $n)")
       selects+=("SELECT 'violation' AS result, check_name, process, CAST(ts AS VARCHAR) AS ts, observed, expected FROM $n")
       selects+=("SELECT 'none', $(sqlq "${n#chk_}"), NULL, NULL, 'none', NULL WHERE NOT EXISTS (FROM $n)")
    done
    local sql="WITH $(IFS=,; echo "${ctes[*]}") $(printf '%s UNION ALL ' "${selects[@]}" | sed 's/ UNION ALL $//') ORDER BY 2, 1 DESC, 3, 4;"
    check_run() {
        "$DUCKDB" -init /dev/null -noheader -list -separator $'\t' -nullvalue - \
                  -c ".read $(sqlq "$1")" -c "$sql"
    }
    with_session "$pq" check_run
}
