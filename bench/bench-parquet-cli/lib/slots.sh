# shellcheck shell=bash
# bench-parquet: scheduling of per-log jobs (convert, verify, restore).
# Sourced by bench-parquet: uses its helpers (msg, die, sqlq, ...) and
# variables (self_dir, sql_dir, DUCKDB, ...).

# Scheduling of per-log jobs (convert, verify, restore). Both functions work on the
# calling command's variables (bash's dynamic scope): sources (log names),
# size_of (bytes per log), jobs (empty: from memory and cores), slots_of, total,
# and failed.

# Per slot: DuckDB's memory limit and threads.
slot_mem_gib=4 slot_threads=4

# Orders sources largest first and sets jobs, total and slots_of; also
# mem_avail_mib, ncpu and mem_src, for the caller's messages.
plan_slots() {
    local src k budget_mib
    # Largest first: the longest job starts at once and overlaps with the rest
    # instead of running alone at the end.
    total=0
    for src in "${sources[@]}"; do total=$(( total + size_of[$src] )); done
    mapfile -t sources < <(for src in "${sources[@]}"; do printf '%s\t%s\n' "${size_of[$src]}" "$src"; done |
                           sort -t$'\t' -k1,1nr -k2,2 | cut -f2)

    # Parallelism: memory budget per slot = DuckDB's limit + 1.5 GiB for what it
    # uses beyond the limit (up to ~0.7 GiB per 4 GiB slot, more for jobs of
    # several slots), and slot_threads DuckDB threads per slot.
    budget_mib=$(( slot_mem_gib * 1024 + 1536 ))
    read -r mem_avail_mib ncpu mem_src < <(resources)
    if test -z "$jobs"
    then jobs=$(( mem_avail_mib / budget_mib ))
         test $jobs -le $(( ncpu / slot_threads )) || jobs=$(( ncpu / slot_threads ))
         test $jobs -ge 1 || jobs=1
    fi

    # Slots per log: a log larger than an even share of the work (total / jobs)
    # would finish last on one slot; it gets its share of the slots instead,
    # round(size * jobs / total), as one job with that many times the threads and
    # memory, so it ends about when the rest does. Memory stays within budget.
    # (Rounding, not ceiling: two equal halves of the work get half the slots each.)
    for src in "${sources[@]}"
    do if test "$total" -gt 0 -a $(( size_of[$src] * jobs )) -gt "$total"
       then k=$(( (2 * size_of[$src] * jobs + total) / (2 * total) ))
            test $k -le "$jobs" || k=$jobs
       else k=1; fi
       slots_of[$src]=$k
    done
}

# Runs "LAUNCH SOURCE SLOTS" for each source as a background job, in the order
# of sources, with at most jobs slots in use; failed gets the sources whose job
# failed.
run_slots() {
    local launch=$1
    local -A running=() started=() weight=()
    local src label pid status used=0 k t_run=$SECONDS
    reap_one() {
        if wait -n -p pid; then status=0; else status=$?; fi
        label=${running[$pid]}
        local took=$(( SECONDS - started[$pid] ))
        used=$(( used - weight[$pid] ))
        unset "running[$pid]" "started[$pid]" "weight[$pid]"
        if test $status -eq 0
        then msg "done: $label in ${took} s (at $(( SECONDS - t_run )) s)"
        else msg "failed: $label (exit $status, after ${took} s)"; failed+=("$label"); fi
    }
    for src in "${sources[@]}"
    do k=${slots_of[$src]}
       while test $(( used + k )) -gt "$jobs"; do reap_one; done
       "$launch" "$src" "$k" &
       running[$!]=$src
       started[$!]=$SECONDS
       weight[$!]=$k
       used=$(( used + k ))
    done
    while test ${#running[@]} -gt 0; do reap_one; done
}

# Memory (MiB) and cores for conversion, and where the memory figure comes from:
# RAM or the cgroup limit as DuckDB sees them (bp_resources, portable), lowered on
# Linux to MemAvailable, i.e. to what other processes leave free.
resources() {
    local mem cores avail src="RAM or cgroup limit, via DuckDB"
    read -r mem cores < <("$DUCKDB" -init /dev/null -noheader -list -separator ' ' \
                              -c ".read $(sqlq "$sql_dir/lib.sql")" -c ".read $(sqlq "$sql_dir/convert.sql")" \
                              -c "FROM bp_resources();")
    test -n "$cores" || die "could not read memory and cores from DuckDB"
    if test -r /proc/meminfo
    then avail=$(awk '/^MemAvailable:/ { printf "%d", $2 / 1024 }' /proc/meminfo)
         if test -n "$avail" && test "$avail" -lt "$mem"; then mem=$avail; src=MemAvailable; fi
    fi
    echo "$mem $cores $src"
}
