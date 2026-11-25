# shellcheck shell=bash
# bench-parquet: convert: logs into PQDIR.
# Sourced by bench-parquet: uses its helpers (msg, die, sqlq, ...) and
# variables (self_dir, sql_dir, DUCKDB, ...).

# Convert one log file. Runs as a background job; all output to stderr.
convert_one() {
    local src=$1 process=$2 dst=$3 stats=$4 spill=$5 mem=$6 threads=$7 zstd=$8

    local size sha final_nl
    size=$(stat -L -c%s "$src")
    if test "$size" -eq 0
    then final_nl=false
    elif test "$(tail -c1 "$src" | wc -l)" -eq 1
    then final_nl=true
    else final_nl=false
    fi

    mkdir -p "$spill"
    stats_sql() {
        echo "SELECT $(sqlq "$process") AS process,
                     $(sqlq "$src") AS source, ${size}::UBIGINT AS size_bytes,
                     $(sqlq "$sha") AS sha256, ${final_nl} AS final_newline"
    }

    if test "$size" -eq 0
    then
        sha=$(sha256sum "$src" | cut -d' ' -f1)
        local stats_sql; stats_sql=$(stats_sql)
        "$DUCKDB" -init /dev/null -c "
            COPY (SELECT NULL::VARCHAR AS process, NULL::UBIGINT AS seq, NULL::TIMESTAMP_NS AS ts,
                         NULL::VARCHAR AS ns, NULL::VARCHAR AS sev, NULL::VARCHAR AS thread,
                         NULL::VARCHAR AS host, NULL::VARCHAR AS data, NULL::VARCHAR AS line
                  WHERE false)
              TO $(sqlq "$dst") (FORMAT parquet, COMPRESSION zstd, COMPRESSION_LEVEL $zstd, ROW_GROUP_SIZE $row_group_size);
            COPY (SELECT *, 0::UBIGINT AS lines_total, 0::UBIGINT AS lines_json, 0::UBIGINT AS lines_text,
                         0::UBIGINT AS lines_text_after_json, 0::UBIGINT AS lines_json_verbatim
                  FROM ($stats_sql))
              TO $(sqlq "$stats") (FORMAT parquet);" >/dev/null
        return
    fi

    local script
    script=$(mktemp -p "$spill" convert.XXXXXX.sql)
    cat > "$script" <<EOF
SET memory_limit = '$mem';
SET threads = $threads;
SET temp_directory = $(sqlq "$spill");
.read $(sqlq "$sql_dir/lib.sql")
.read $(sqlq "$sql_dir/convert.sql")
COPY (FROM bp_parse($(sqlq "$process"), '/dev/stdin') ORDER BY ns NULLS LAST, ts, seq)
TO $(sqlq "$dst") (FORMAT parquet, COMPRESSION zstd, COMPRESSION_LEVEL $zstd, ROW_GROUP_SIZE $row_group_size);
EOF
    # One read of the log feeds the sha256 (through a named pipe), the checks for
    # characters the line reader can't carry through unchanged (CR, the ASCII
    # record separator 0x1e; awk fails at the end if it saw any) and the line
    # numbering for DuckDB.
    local fifo=$spill/sha256.fifo shafile=$spill/sha256 shapid
    mkfifo "$fifo"
    sha256sum < "$fifo" > "$shafile" & shapid=$!
    local status=0
    tee "$fifo" < "$src" |
        awk -v label="$process" '
            /[\r\036]/ { if (index($0, "\r")) cr++; if (index($0, "\036")) rs++ }
            { printf "%d\037%s\n", NR, $0 }
            END { if (cr) printf "bench-parquet: %s: %d lines contain CR; not supported\n", label, cr > "/dev/stderr"
                  if (rs) printf "bench-parquet: %s: %d lines contain the ASCII record separator (0x1e); not supported\n", label, rs > "/dev/stderr"
                  if (cr || rs) exit 3 }' |
        "$DUCKDB" -init /dev/null -c ".read $(sqlq "$script")" >/dev/null || status=$?
    wait $shapid || true
    test $status -eq 0 || return $status
    sha=$(cut -d' ' -f1 "$shafile")
    test ${#sha} -eq 64 || { msg "$process: no sha256 computed"; return 1; }

    # Statistics from the written file.
    local stats_sql; stats_sql=$(stats_sql)
    "$DUCKDB" -init /dev/null -c ".read $(sqlq "$sql_dir/lib.sql")" -c ".read $(sqlq "$sql_dir/convert.sql")" -c "
        COPY (SELECT s.*, st.* FROM ($stats_sql) s, bp_file_stats($(sqlq "$dst")) st)
          TO $(sqlq "$stats") (FORMAT parquet);" >/dev/null
}

convert_cmd() {
    local out= run_id= system_start= slot_length= epoch_length= force= add= jobs= keep_temp= zstd=$zstd_default
    local -A file_of=()
    local a process file
    while test $# -gt 0
    do case "$1" in
           --run-id )       run_id=${2:?--run-id needs an identifier}; shift;;
           --system-start ) system_start=${2:?--system-start needs a timestamp}; shift;;
           --slot-length )  slot_length=${2:?--slot-length needs seconds}; shift;;
           --epoch-length ) epoch_length=${2:?--epoch-length needs a number of slots}; shift
                            [[ $epoch_length =~ ^[1-9][0-9]*$ ]] ||
                                die "convert: --epoch-length: not a number of slots: $epoch_length";;
           --force )        force=t;;
           --add )          add=t;;
           --jobs )         jobs=${2:?--jobs needs a number}; shift;;
           --keep-temp )    keep_temp=t;;
           --zstd-level )   zstd=${2:?--zstd-level needs a number}; shift;;
           -* )             die "convert: unknown option: $1";;
           *=* )            a=$1; process=${a%%=*}; file=${a#*=}
                            [[ $process =~ ^[A-Za-z0-9._-]+$ ]] && test "$process" != meta ||
                                die "convert: not a process name (letters, digits, . _ -; not 'meta'): $process"
                            test -z "${file_of[$process]:-}" || die "convert: process given twice: $process"
                            test -f "$file" || die "convert: no such file: $file"
                            file_of[$process]=$(realpath -s "$file");;
           * )              test -z "$out" || die "convert: more than one PQDIR (logs are PROCESS=FILE)"; out=$1;;
       esac; shift; done
    test -n "$out" || die "convert: PQDIR missing"
    test -n "$run_id" || die "convert: --run-id missing"
    test ${#file_of[@]} -gt 0 || die "convert: no PROCESS=FILE given"
    test -z "$system_start" -a -z "$slot_length" -o -n "$system_start" -a -n "$slot_length" ||
        die "convert: --system-start and --slot-length go together"
    test -z "$force" -o -z "$add" || die "convert: --force and --add exclude each other"
    command -v "$DUCKDB" >/dev/null || die "duckdb not found (DUCKDB=$DUCKDB)"
    out=$(realpath -m "$out")
    test -d "$(dirname "$out")" || die "convert: no such directory: $(dirname "$out")"
    if test -e "$out" -a -z "$force" -a -z "$add"
    then die "$out exists (use --force to replace it, or --add to convert only the logs it lacks)"; fi

    # Genesis values as SQL; NULL when not given (query sessions then refuse
    # slot and epoch arithmetic instead of guessing).
    local start_sql=NULL slot_sql=NULL epoch_sql=NULL
    if test -n "$system_start"
    then start_sql="CAST($(sqlq "${system_start%Z}") AS TIMESTAMP)"
         slot_sql="CAST($(sqlq "$slot_length") AS DOUBLE)"; fi
    test -z "$epoch_length" || epoch_sql=$epoch_length

    # --add to an existing PQDIR: the recorded metadata must agree (checked now,
    # before converting anything), and logs it has are skipped.
    local adding= merge_sql= process_
    if test -n "$add" -a -e "$out"
    then check_pqdir "$out"
         adding=t
         merge_sql="FROM bp_merge_run_meta($(sqlq "$out/meta/run.parquet"), $(sqlq "$run_id"),
                        $start_sql, $slot_sql, $epoch_sql, $(sqlq "$converter_version"))"
         "$DUCKDB" -init /dev/null -c ".read $(sqlq "$sql_dir/lib.sql")" -c ".read $(sqlq "$sql_dir/convert.sql")" -c "$merge_sql;" >/dev/null ||
             die "convert --add: $out doesn't match (see above)"
         while IFS= read -r process_
         do if test -n "${file_of[$process_]:-}" -a -f "$out/$process_.parquet"
            then msg "$process_: in $out already, skipped"; unset "file_of[$process_]"; fi
         done < <("$DUCKDB" -init /dev/null -noheader -list -c "
                      SELECT process FROM read_parquet($(sqlq "$out/meta/files.parquet"));")
    fi

    local -A size_of=() slots_of=()
    local sources=() total=0 src mem_avail_mib ncpu mem_src
    for src in "${!file_of[@]}"
    do size_of[$src]=$(stat -L -c%s "${file_of[$src]}"); sources+=("$src"); done
    plan_slots

    local tmp; tmp=$(dirname "$out")/.$(basename "$out").tmp.$$
    mkdir -p "$tmp"/spill "$tmp"/stats "$tmp"/meta
    # Roles come from process names (node-*, generator); anything else is
    # converted but not part of a query session's raw or raw_generator.
    for src in "${sources[@]}"
    do case "$src" in node-* | generator ) ;;
           * ) msg "$src: not named node-* or generator; not in a query session's raw or raw_generator";; esac
    done
    if test ${#sources[@]} -eq 0
    then msg "no log to convert: $out has all of them"
    else msg "converting ${#sources[@]} logs of $run_id: $jobs slots, each ${slot_threads} threads and ${slot_mem_gib} GiB memory limit"
         msg "  memory available: $(( mem_avail_mib / 1024 )) GiB ($mem_src), cores: $ncpu"
    fi
    for src in "${sources[@]}"
    do test "${slots_of[$src]}" -eq 1 ||
           msg "  $src ($(( size_of[$src] / 1048576 )) MiB of $(( total / 1048576 )) MiB): ${slots_of[$src]} slots"
    done

    convert_job() {
        convert_one "${file_of[$1]}" "$1" "$tmp/$1.parquet" "$tmp/stats/$1.parquet" \
                    "$tmp/spill/$1" "$(( $2 * slot_mem_gib ))GiB" "$(( $2 * slot_threads ))" "$zstd"
    }
    local failed=() t0=$SECONDS
    run_slots convert_job

    if test ${#failed[@]} -gt 0
    then if test -n "$keep_temp"
         then msg "kept $tmp"
         else rm -rf "$tmp"; fi
         die "${#failed[@]} of ${#sources[@]} conversions failed: ${failed[*]}"
    fi

    # Metadata. With --add: the recorded files plus the new ones, and the recorded
    # run row with the genesis values it lacked (bp_merge_run_meta).
    if test -n "$adding"
    then local new_files="SELECT * FROM read_parquet($(sqlq "$tmp/stats/*.parquet"))"
         test ${#sources[@]} -gt 0 || new_files="SELECT * FROM read_parquet($(sqlq "$out/meta/files.parquet")) WHERE false"
         "$DUCKDB" -init /dev/null -c ".read $(sqlq "$sql_dir/lib.sql")" -c ".read $(sqlq "$sql_dir/convert.sql")" -c "
            COPY (SELECT * FROM read_parquet($(sqlq "$out/meta/files.parquet"))
                  WHERE process NOT IN (SELECT process FROM ($new_files))
                  UNION ALL BY NAME ($new_files) ORDER BY process)
              TO $(sqlq "$tmp/meta/files.parquet") (FORMAT parquet);
            COPY ($merge_sql) TO $(sqlq "$tmp/meta/run.parquet") (FORMAT parquet);" >/dev/null
    else
    "$DUCKDB" -init /dev/null -c "
        COPY (SELECT * FROM read_parquet($(sqlq "$tmp/stats/*.parquet")) ORDER BY process)
          TO $(sqlq "$tmp/meta/files.parquet") (FORMAT parquet);
        COPY (SELECT $(sqlq "$run_id")                       AS run_id,
                     CAST($start_sql AS TIMESTAMP)           AS system_start,
                     CAST($slot_sql AS DOUBLE)               AS slot_length_s,
                     CAST($epoch_sql AS BIGINT)              AS epoch_length,
                     $(sqlq "$converter_version")            AS converter_version,
                     ${raw_schema_version}::INTEGER          AS raw_schema_version,
                     version()                               AS duckdb_version,
                     CAST(now() AT TIME ZONE 'UTC' AS TIMESTAMP) AS created)
          TO $(sqlq "$tmp/meta/run.parquet") (FORMAT parquet);" >/dev/null
    fi
    rm -rf "$tmp"/spill "$tmp"/stats

    # Into place: the whole directory, or with --add the new logs, then the
    # metadata that lists them.
    if test -n "$adding"
    then for src in "${sources[@]}"; do mv "$tmp/$src.parquet" "$out/$src.parquet"; done
         mv "$tmp/meta/files.parquet" "$out/meta/files.parquet"
         mv "$tmp/meta/run.parquet" "$out/meta/run.parquet"
         rm -rf "$tmp"
    elif test -e "$out"
    then mv "$out" "$tmp.old"; mv "$tmp" "$out"; rm -rf "$tmp.old"
    else mv "$tmp" "$out"; fi

    # Namespaces differing only in case can't coexist as views (DuckDB matches
    # identifiers case-insensitively, even quoted). Views are made for the nodes only.
    local clashes=
    compgen -G "$out/node-*.parquet" >/dev/null &&
    clashes=$("$DUCKDB" -init /dev/null -noheader -list -c "
        SELECT string_agg(DISTINCT ns, ' / ') FROM read_parquet($(sqlq "$out/node-*.parquet"))
        WHERE ns IS NOT NULL GROUP BY lower(ns) HAVING count(DISTINCT ns) > 1;")
    if test -n "$clashes"
    then msg "namespaces differing only in case (only one gets a view):"
         echo "$clashes" | sed 's/^/  /' >&2; fi

    # Summary: totals, then only the logs worth a look.
    local files=$(sqlq "$out/meta/files.parquet") notable
    local cond="lines_text_after_json > 0 OR lines_json_verbatim > 0"
    "$DUCKDB" -init /dev/null -c "
        SELECT count(*) AS logs, sum(size_bytes) AS size_bytes, sum(lines_total) AS lines_total,
               sum(lines_json) AS lines_json, sum(lines_text) AS lines_text,
               sum(lines_text_after_json) AS lines_text_after_json,
               sum(lines_json_verbatim) AS lines_json_verbatim
        FROM read_parquet($files);" >&2
    notable=$("$DUCKDB" -init /dev/null -noheader -list -c "SELECT count(*) FROM read_parquet($files) WHERE $cond;")
    if test "$notable" -gt 0
    then msg "$notable logs with text after JSON started, or JSON lines kept verbatim:"
         "$DUCKDB" -init /dev/null -c "
            SELECT process, size_bytes, lines_total, lines_json, lines_text,
                   lines_text_after_json, lines_json_verbatim
            FROM read_parquet($files) WHERE $cond ORDER BY process;" >&2
    else msg "no log with text after JSON started, or JSON lines kept verbatim"
    fi
    msg "wrote $out ($(du -sh "$out" | cut -f1)) in $(( SECONDS - t0 )) s"
}
