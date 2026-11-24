# shellcheck shell=bash

## Sanity check of a finished run: the run's files (exit codes, stderr, logs)
## and, where RUN/parquets/ exists, the package checks (bench-parquet check).
## Writes the verdict to RUN/sanity-check.txt and a summary to the terminal;
## exit status 1 when anything failed. Findings state facts only: what was
## seen, what was expected.

## Policy for the package checks: their violations fail, or warn. A check not
## named here warns.
sanity_check_fails=(unknown_forger missing_ancestry slot_entry_before_start node_window)

## One finding: status, check, process, ts, observed, expected (tab-separated;
## "-" for an empty field).
sanity_row() {
    local IFS=$'\t'; echo "$*"
}

## The run's supervised processes: directories (up to two levels below the run)
## with a start.sh or an exit_code, relative to the run.
sanity_process_dirs() {
    local dir=$1
    find "$dir" -mindepth 2 -maxdepth 4 \( -name start.sh -o -name exit_code \) \
         -not -path "$dir/parquets/*" -not -path "$dir/analysis/*" -printf '%h\n' |
        sed "s|^$dir/||" | LC_ALL=C sort -u
}

## Exit codes, from the exit_code file the supervisord wrapper writes: none =
## never started; empty = stopped by the workbench; a number = exited by itself.
sanity_exit_codes() {
    local dir=$1 p code n=0 stopped=0 zero=0 bad=0
    while IFS= read -r p
    do n=$(( n + 1 ))
       if ! test -f "$dir/$p/exit_code"
       then sanity_row fail exit_code "$p" - "never started (start.sh, no exit_code)" "started"; bad=$(( bad + 1 ))
       else code=$(tr -d '[:space:]' < "$dir/$p/exit_code")
            if   test -z "$code";   then stopped=$(( stopped + 1 ))
            elif test "$code" = 0;  then zero=$(( zero + 1 ))
            else sanity_row fail exit_code "$p" - "exited with code $code" "stopped by the workbench, or exit code 0"
                 bad=$(( bad + 1 )); fi
       fi
    done < <(sanity_process_dirs "$dir")
    if test $n -eq 0
    then sanity_row skip exit_code - - "no process directories in the run" -
    elif test $bad -eq 0
    then sanity_row pass exit_code - - "$n processes: $stopped stopped by the workbench, $zero exited with code 0" -
    fi
}

## stderr of every process (and supervisord's): empty expected. Recognised: the
## trace forwarder's "TraceObject queue overflowed. Dropped N messages from T1
## to T2" (messages lost on the way to cardano-tracer, not from stdout).
sanity_stderr() {
    local dir=$1 f p n=0 bad=0 summary
    while IFS= read -r f
    do n=$(( n + 1 ))
       test -s "$f" || continue
       p=${f#"$dir"/}; p=${p%/stderr}
       summary=$(LC_ALL=C awk '
           /TraceObject queue overflowed\. Dropped [0-9]+ messages from / {
               n = $0; sub(/.*Dropped /, "", n); sub(/ messages.*/, "", n)
               t = $0; sub(/.* messages from /, "", t)
               t1 = t; sub(/ to .*/, "", t1); t2 = t; sub(/.* to /, "", t2)
               if (!ov) first = t1; last = t2; ov++; dropped += n; next }
           { other++; if (!line) line = substr($0, 1, 80) }
           END { s = NR " lines"
                 if (ov)    s = s ", " ov " trace queue overflows: " dropped " messages dropped (" first " to " last ")"
                 if (other) s = s ", first other: " line
                 print s }' "$f")
       sanity_row warn stderr "$p" - "$summary" "empty"; bad=$(( bad + 1 ))
    done < <(find "$dir" -mindepth 2 -maxdepth 4 -name stderr \
                  -not -path "$dir/parquets/*" -not -path "$dir/analysis/*" | LC_ALL=C sort)
    if test $n -eq 0
    then sanity_row skip stderr - - "no stderr files in the run" -
    elif test $bad -eq 0
    then sanity_row pass stderr - - "all $n stderr files empty" -
    fi
}

## A log for every node and the generator: their stdout, or its Parquet copy
## (stdout deleted after verification).
sanity_logs() {
    local dir=$1 d p n=0 bad=0 converted=0
    for d in "$dir"/node-* "$dir"/generator
    do test -d "$d" || continue
       p=$(basename "$d"); n=$(( n + 1 ))
       if test -f "$d"/stdout
       then test -s "$d"/stdout ||
                { sanity_row fail log "$p" - "stdout empty" "trace output"; bad=$(( bad + 1 )); }
       elif test -f "$dir/parquets/$p.parquet"
       then converted=$(( converted + 1 ))
       else sanity_row fail log "$p" - "no stdout, no parquets/$p.parquet" "the process's log"; bad=$(( bad + 1 ))
       fi
    done
    if test $n -eq 0
    then sanity_row skip log - - "no node-* or generator directories in the run" -
    elif test $bad -eq 0
    then sanity_row pass log - - "$n processes have a log ($converted only in parquets/)" -
    fi
}

## The package checks over parquets/, judged by sanity_check_fails.
sanity_parquet_checks() {
    local dir=$1 result check p ts observed expected status out
    if ! test -f "$dir"/parquets/meta/files.parquet
    then sanity_row skip parquet_checks - - "no parquets/ (wb parquet create RUN)" -; return
    fi
    if ! out=$(bench_parquet check "$dir"/parquets)
    then sanity_row skip parquet_checks - - "bench-parquet check failed (see above)" -; return
    fi
    while IFS=$'\t' read -r result check p ts observed expected
    do if test "$result" = none
       then sanity_row pass "$check" - - "none" -
       else status=warn
            [[ " ${sanity_check_fails[*]} " == *" $check "* ]] && status=fail
            sanity_row "$status" "$check" "$p" "${ts:0:19}" "$observed" "$expected"
       fi
    done <<< "$out"
}

## Lays out the findings: a header with the verdict, fail and warn lines (with
## what was expected), then pass and skip lines. With a limit, at most that many
## lines per check in the fail / warn part.
sanity_format() {
    local run=$1 when=$2 limit=${3:-0}
    LC_ALL=C awk -F'\t' -v run="$run" -v when="$when" -v limit="$limit" '
        { st[NR] = $1; ck[NR] = $2; pr[NR] = $3; ts[NR] = $4; ob[NR] = $5; ex[NR] = $6
          cnt[$1]++
          if (length($2) > wc) wc = length($2)
          if ($3 != "-" && length($3) > wp) wp = length($3)
          if ($4 != "-") wt = 19 }
        function line(i,   s) {
            s = sprintf("%-4s  %-" wc "s  %-" wp "s", st[i], ck[i], pr[i] == "-" ? "" : pr[i])
            if (wt) s = s sprintf("  %-" wt "s", ts[i] == "-" ? "" : ts[i])
            return s "  " ob[i] }
        function indent(   n) { n = 4 + 2 + wc + 2 + wp + 2 + (wt ? wt + 2 : 0); return sprintf("%" n "s", "") }
        function flush() { if (more) printf "%s(%d more %s lines in the verdict file)\n", indent(), more, prev; more = 0 }
        function rule(t,   s) { s = "-- " t " "; while (length(s) < 78) s = s "-"; return s }
        END {
            verdict = cnt["fail"] ? "FAIL" : cnt["warn"] ? "WARN" : "PASS"
            printf "sanity-check  %s\n", run
            printf "checked       %s\n", when
            printf "verdict       %s: %d fail, %d warn, %d pass, %d skip\n\n", verdict, cnt["fail"], cnt["warn"], cnt["pass"], cnt["skip"]
            if (cnt["fail"] + cnt["warn"]) {
                print rule("fail, warn")
                for (s = 1; s <= 2; s++) {
                    want = s == 1 ? "fail" : "warn"
                    for (i = 1; i <= NR; i++) {
                        if (st[i] != want) continue
                        if (ck[i] != prev) { flush(); prev = ck[i] }
                        shown[ck[i]]++
                        if (limit && shown[ck[i]] > limit) { more++; continue }
                        print line(i)
                        if (ex[i] != "-") print indent() "expected: " ex[i]
                    }
                    flush(); prev = ""
                }
                print ""
            }
            print rule("pass, skip")
            for (s = 1; s <= 2; s++) {
                want = s == 1 ? "pass" : "skip"
                for (i = 1; i <= NR; i++) if (st[i] == want) print line(i)
            }
        }'
}

sanity_check() {
    local usage="USAGE: wb run sanity-check RUN"
    local run=${1:?$usage}
    local dir; dir=$(run get "$run") || exit 1
    local name; name=$(basename "$dir")
    local when; when=$(date -u '+%Y-%m-%d %H:%M:%S UTC')

    local rows
    rows=$({ sanity_exit_codes "$dir"
             sanity_stderr "$dir"
             sanity_logs "$dir"
             sanity_parquet_checks "$dir"; })

    sanity_format "$name" "$when" <<< "$rows" > "$dir"/sanity-check.txt
    sanity_format "$name" "$when" 5 <<< "$rows" >&2
    progress "run | sanity-check" "verdict in $dir/sanity-check.txt"
    ! grep -q '^verdict       FAIL' "$dir"/sanity-check.txt
}
