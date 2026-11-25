# shellcheck shell=bash
# bench-parquet: reading logs: lines, host-summary, head, tail, excerpt.
# Sourced by bench-parquet: uses its helpers (msg, die, sqlq, ...) and
# variables (self_dir, sql_dir, DUCKDB, ...).

# lines, host-summary: input for locli prepare-db-from-lines

# The Parquet file of one process, after checks.
process_parquet() {
    local pq=$1 process=$2
    check_pqdir "$pq"
    local pqf=$pq/$process.parquet
    test -f "$pqf" || die "no $pqf"
    echo "$pqf"
}

lines_cmd() {
    local pq= process= nsfile=
    while test $# -gt 0
    do case "$1" in
           --ns-file ) nsfile=${2:?--ns-file needs a file}; shift;;
           -* )        die "lines: unknown option: $1";;
           * )         if   test -z "$pq";      then pq=$1
                       elif test -z "$process"; then process=$1
                       else die "lines: too many arguments"; fi;;
       esac; shift; done
    test -n "$pq" -a -n "$process" || die "lines: PQDIR PROCESS required"
    pq=$(realpath -m "$pq")
    local pqf; pqf=$(process_parquet "$pq" "$process")

    local filter="ns IS NOT NULL" ns list=()
    if test -n "$nsfile"
    then test -r "$nsfile" || die "lines: can't read $nsfile"
         while IFS= read -r ns
         do test -n "$ns" && list+=("$(sqlq "$ns")"); done < "$nsfile"
         test ${#list[@]} -gt 0 || die "lines: no namespaces in $nsfile"
         filter="ns IN ($(IFS=,; echo "${list[*]}"))"
    fi

    # Restoring log order sorts the selected lines: bounded memory, spilling
    # beside PQDIR (several of these may run in parallel, one per host).
    local tmp; tmp=$(dirname "$pq")/.$(basename "$pq").lines.tmp.$$
    mkdir -p "$tmp"
    local status=0
    "$DUCKDB" -init /dev/null <<EOF || status=$?
SET memory_limit = '2GiB';
SET threads = 2;
SET temp_directory = $(sqlq "$tmp");
.read $(sqlq "$sql_dir/lib.sql")
COPY (SELECT bp_line(t) FROM read_parquet($(sqlq "$pqf")) t WHERE $filter ORDER BY seq)
  TO '/dev/stdout' (FORMAT csv, HEADER false, QUOTE '', ESCAPE '', DELIMITER E'\x1e');
EOF
    rm -rf "$tmp"
    return $status
}

host_summary_cmd() {
    local pq=${1:?host-summary: PQDIR missing} process=${2:?host-summary: PROCESS missing}
    test $# -eq 2 || die "host-summary: too many arguments"
    pq=$(realpath -m "$pq")
    local pqf; pqf=$(process_parquet "$pq" "$process")
    "$DUCKDB" -init /dev/null -noheader -list <<EOF
.read $(sqlq "$sql_dir/lib.sql")
.read $(sqlq "$sql_dir/locli.sql")
FROM bp_host_summary($(sqlq "$pqf"));
EOF
}

head_tail_cmd() {
    local cmd=$1; shift
    local pq= process= n=20
    while test $# -gt 0
    do case "$1" in
           -n ) n=${2:?-n needs a number}; shift;;
           -* ) die "$cmd: unknown option: $1";;
           * )  if   test -z "$pq";      then pq=$1
                elif test -z "$process"; then process=$1
                else die "$cmd: too many arguments"; fi;;
       esac; shift; done
    test -n "$pq" -a -n "$process" || die "$cmd: PQDIR PROCESS required"
    [[ $n =~ ^[0-9]+$ ]] || die "$cmd: -n needs a number, got: $n"
    pq=$(realpath -m "$pq")
    local pqf; pqf=$(process_parquet "$pq" "$process")
    "$DUCKDB" -init /dev/null <<EOF
.read $(sqlq "$sql_dir/lib.sql")
COPY (FROM bp_$cmd($(sqlq "$pqf"), $n))
  TO '/dev/stdout' (FORMAT csv, HEADER false, QUOTE '', ESCAPE '', DELIMITER E'\x1e');
EOF
}

##############################################################################
# excerpt

excerpt_cmd() {
    local pq= at= ns= where=true before=5 after=5 outdir= processes=()
    while test $# -gt 0
    do case "$1" in
           --at )      at=${2:?--at needs a timestamp}; shift;;
           --ns )      ns=${2:?--ns needs a namespace}; shift;;
           --where )   where=${2:?--where needs an SQL condition}; shift;;
           --before )  before=${2:?}; shift;;
           --after )   after=${2:?}; shift;;
           --process ) processes+=("${2:?}"); shift;;
           -o )        outdir=${2:?-o needs a directory}; shift;;
           -* )        die "excerpt: unknown option: $1";;
           * )         test -z "$pq" || die "excerpt: more than one PQDIR"; pq=$1;;
       esac; shift; done
    test -n "$pq" -a -n "$outdir" || die "excerpt: PQDIR and -o OUTDIR required"
    test -n "$at" -o -n "$ns" || die "excerpt: --at or --ns required"
    pq=$(realpath -m "$pq")
    check_pqdir "$pq"

    local pfilter=true p
    if test ${#processes[@]} -gt 0
    then pfilter="process IN ($(for p in "${processes[@]}"; do printf '%s,' "$(sqlq "$p")"; done | sed 's/,$//'))"; fi

    local anchor_sql
    if test -n "$at"
    then anchor_sql="SELECT epoch_ns(CAST($(sqlq "${at%Z}") AS TIMESTAMP_NS))"
    else anchor_sql="SELECT epoch_ns(min(ts)) FROM read_parquet($(sqlq "$pq/*.parquet"))
                     WHERE ns = $(sqlq "$ns") AND ($where) AND $pfilter"
    fi
    local anchor
    anchor=$("$DUCKDB" -init /dev/null -noheader -list -nullvalue '' -c "$anchor_sql;")
    test -n "$anchor" || die "excerpt: no event of $ns matching: $where"

    mkdir -p "$outdir"
    local list
    list=$("$DUCKDB" -init /dev/null -noheader -list -c "
        SELECT DISTINCT process FROM read_parquet($(sqlq "$pq/*.parquet"))
        WHERE $pfilter ORDER BY process;")
    {
        echo ".read $(sqlq "$sql_dir/lib.sql")"
        echo "CREATE TEMP TABLE w AS
                SELECT process, seq, bp_line(t) AS l
                FROM read_parquet($(sqlq "$pq/*.parquet")) t
                WHERE $pfilter
                  AND epoch_ns(ts) BETWEEN $anchor - CAST($before * 1e9 AS BIGINT)
                                           AND $anchor + CAST($after  * 1e9 AS BIGINT);"
        while IFS= read -r p
        do echo "COPY (SELECT l FROM w WHERE process = $(sqlq "$p") ORDER BY seq)
                   TO $(sqlq "$outdir/$p.stdout") (FORMAT csv, HEADER false, QUOTE '', ESCAPE '', DELIMITER E'\x1e');"
        done <<< "$list"
        echo "SELECT process, count(*) AS lines FROM w GROUP BY process ORDER BY process;"
    } > "$outdir/.excerpt.sql"
    "$DUCKDB" -init /dev/null -c ".read $(sqlq "$outdir/.excerpt.sql")" >&2
    rm -f "$outdir/.excerpt.sql"
    msg "window: $("$DUCKDB" -init /dev/null -noheader -list -c "SELECT make_timestamp_ns($anchor)") -${before}s / +${after}s; files in $outdir"
}
