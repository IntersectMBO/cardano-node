# shellcheck shell=bash
# bench-parquet: restore and verify: logs rebuilt from PQDIR.
# Sourced by bench-parquet: uses its helpers (msg, die, sqlq, ...) and
# variables (self_dir, sql_dir, DUCKDB, ...).

# restore

# Restore one log: rebuilt by DuckDB within the given memory and threads
# (spilling to spill), the final newline as in the original, and with verify the
# written file's sha256 checked against the recorded one.
restore_one() {
    local pqf=$1 outf=$2 final_nl=$3 lines=$4 sha=$5 mem=$6 threads=$7 spill=$8 verify=$9
    mkdir -p "$spill"
    "$DUCKDB" -init /dev/null >/dev/null <<EOF || { rm -rf "$spill"; return 1; }
SET memory_limit = '$mem';
SET threads = $threads;
SET temp_directory = $(sqlq "$spill");
.read $(sqlq "$sql_dir/lib.sql")
COPY (FROM bp_log($(sqlq "$pqf")))
  TO $(sqlq "$outf") (FORMAT csv, HEADER false, QUOTE '', ESCAPE '', DELIMITER E'\x1e');
EOF
    rm -rf "$spill"
    # COPY ends every line with a newline; the original may not have had a final one.
    if test "$final_nl" = false -a "$lines" -gt 0
    then truncate -s -1 "$outf"; fi
    if test -n "$verify"
    then local got
         got=$(sha256sum "$outf" | cut -d' ' -f1)
         if test "$got" = "$sha"
         then msg "verified: $outf matches the sha256 recorded at conversion"
         else msg "verification failed: $outf has sha256 $got, recorded at conversion: $sha"; return 1; fi
    fi
}

restore_cmd() {
    local pq= process= outf= verify= jobs= a
    local -A file_of=()
    while test $# -gt 0
    do case "$1" in
           -o )       outf=${2:?-o needs a file}; shift;;
           --verify ) verify=t;;
           --jobs )   jobs=${2:?--jobs needs a number}; shift;;
           -* )       die "restore: unknown option: $1";;
           *=* )      test -n "$pq" || die "restore: PQDIR first"
                      a=$1; test -z "${file_of[${a%%=*}]:-}" || die "restore: process given twice: ${a%%=*}"
                      file_of[${a%%=*}]=$(realpath -m "${a#*=}");;
           * )        if   test -z "$pq";      then pq=$1
                      elif test -z "$process"; then process=$1
                      else die "restore: too many arguments"; fi;;
       esac; shift; done
    test -n "$pq" || die "restore: PQDIR missing"
    if test ${#file_of[@]} -gt 0
    then test -z "$process" -a -z "$outf" || die "restore: either PROCESS -o FILE or PROCESS=FILE.., not both"
    else test -n "$process" -a -n "$outf" || die "restore: PQDIR PROCESS -o FILE, or PQDIR PROCESS=FILE.., required"
         file_of[$process]=$(realpath -m "$outf")
    fi
    pq=$(realpath -m "$pq")
    check_pqdir "$pq"

    # Metadata of the requested logs.
    local -A size_of=() slots_of=() nl_of=() lines_of=() sha_of=() source_of=()
    local p size final_nl lines sha source
    while IFS=$'\t' read -r p size final_nl lines sha source
    do test -n "${file_of[$p]:-}" || continue
       size_of[$p]=$size nl_of[$p]=$final_nl lines_of[$p]=$lines sha_of[$p]=$sha source_of[$p]=$source
    done < <("$DUCKDB" -init /dev/null -noheader -list -separator $'\t' -c "
                 SELECT process, size_bytes, final_newline, lines_total, sha256, source
                 FROM read_parquet($(sqlq "$pq/meta/files.parquet"));")
    local sources=() skipped=0
    for p in "${!file_of[@]}"
    do test -n "${size_of[$p]:-}" -a -f "$pq/$p.parquet" || die "restore: no log $p in $pq"
       if test -n "$process"
       then # Never over the log it was converted from (the path recorded at
            # conversion), while it exists.
            test "${file_of[$p]}" != "$(realpath -m "${source_of[$p]}")" -o ! -e "${source_of[$p]}" ||
                die "refusing to overwrite the original ${source_of[$p]}"
       elif test -e "${file_of[$p]}"
       then msg "$p: ${file_of[$p]} exists, skipped"; skipped=$(( skipped + 1 )); continue
       fi
       sources+=("$p")
    done
    local total=0 mem_avail_mib ncpu mem_src
    plan_slots
    local spill=$(dirname "$pq")/.$(basename "$pq").restore.$$

    # PROCESS -o FILE: one log, written in place, all slots.
    if test -n "$process"
    then restore_one "$pq/$process.parquet" "${file_of[$process]}" "${nl_of[$process]}" "${lines_of[$process]}" \
                     "${sha_of[$process]}" "$(( slots_of[$process] * slot_mem_gib ))GiB" \
                     "$(( slots_of[$process] * slot_threads ))" "$spill" "$verify" || exit 1
         return 0
    fi

    # PROCESS=FILE..: scheduled, each through FILE.restoring, renamed once verified.
    test ${#sources[@]} -gt 0 && msg "restoring ${#sources[@]} logs: $jobs slots, each ${slot_threads} threads and ${slot_mem_gib} GiB memory limit"
    restore_job() {
        local p=$1 k=$2 f=${file_of[$1]}
        mkdir -p "$(dirname "$f")"
        if restore_one "$pq/$p.parquet" "$f.restoring" "${nl_of[$p]}" "${lines_of[$p]}" "${sha_of[$p]}" \
                       "$(( k * slot_mem_gib ))GiB" "$(( k * slot_threads ))" "$spill/$p" t
        then mv "$f.restoring" "$f"
        else rm -f "$f.restoring"; return 1; fi
    }
    local failed=()
    run_slots restore_job
    rm -rf "$spill"
    msg "restored $(( ${#sources[@]} - ${#failed[@]} )), skipped $skipped (file exists)"
    test ${#failed[@]} -eq 0 || die "${#failed[@]} of ${#sources[@]} logs not restored: ${failed[*]}"
}

##############################################################################
# verify

verify_cmd() {
    local pq= jobs= requested=() p
    while test $# -gt 0
    do case "$1" in
           --jobs ) jobs=${2:?--jobs needs a number}; shift;;
           -* )     die "verify: unknown option: $1";;
           * )      if test -z "$pq"; then pq=$1; else requested+=("$1"); fi;;
       esac; shift; done
    test -n "$pq" || die "verify: PQDIR missing"
    pq=$(realpath -m "$pq")
    check_pqdir "$pq"

    local -A size_of=() slots_of=() sha_of=() trim_of=()
    local sources=() total=0 src mem_avail_mib ncpu mem_src size final_nl lines sha
    while IFS=$'\t' read -r p size final_nl lines sha
    do size_of[$p]=$size sha_of[$p]=$sha
       # COPY ends every line with a newline; the original may not have had a final one.
       if test "$final_nl" = false -a "$lines" -gt 0; then trim_of[$p]=t; else trim_of[$p]=; fi
    done < <("$DUCKDB" -init /dev/null -noheader -list -separator $'\t' -c "
                 SELECT process, size_bytes, final_newline, lines_total, sha256
                 FROM read_parquet($(sqlq "$pq/meta/files.parquet"));")
    if test ${#requested[@]} -gt 0
    then for p in "${requested[@]}"
         do test -n "${size_of[$p]:-}" || die "verify: no log $p in $pq"; sources+=("$p"); done
    else sources=("${!size_of[@]}"); fi
    plan_slots

    local spill; spill=$(dirname "$pq")/.$(basename "$pq").verify.$$
    msg "verifying ${#sources[@]} logs: $jobs slots, each ${slot_threads} threads and ${slot_mem_gib} GiB memory limit"
    # One job: the rebuilt log streamed into sha256sum (bp_log; spilling to disk
    # where it doesn't fit the memory limit).
    verify_job() {
        local p=$1 k=$2 got
        mkdir -p "$spill/$p"
        got=$(set -o pipefail
              "$DUCKDB" -init /dev/null -c ".read $(sqlq "$sql_dir/lib.sql")" -c "
                  SET memory_limit = '$(( k * slot_mem_gib ))GiB';
                  SET threads = $(( k * slot_threads ));
                  SET temp_directory = $(sqlq "$spill/$p");
                  COPY (FROM bp_log($(sqlq "$pq/$p.parquet")))
                    TO '/dev/stdout' (FORMAT csv, HEADER false, QUOTE '', ESCAPE '', DELIMITER E'\x1e');" |
                  if test -n "${trim_of[$p]}"; then head -c -1; else cat; fi |
                  sha256sum | cut -d' ' -f1) || got=
        rm -rf "$spill/$p"
        if test "$got" = "${sha_of[$p]}"
        then printf '%s\tok\n' "$p"
        else printf '%s\tfailed\n' "$p"
             msg "$p: sha256 ${got:-(not computed)}, recorded at conversion: ${sha_of[$p]}"
             return 1
        fi
    }
    local failed=()
    run_slots verify_job
    rm -rf "$spill"
    test ${#failed[@]} -eq 0 || die "${#failed[@]} of ${#sources[@]} logs failed verification: ${failed[*]}"
    msg "verified ${#sources[@]} logs"
}
