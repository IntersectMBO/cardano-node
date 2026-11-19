# bench-parquet-cli

Prototype (bash + SQL): converts the stdout logs of a workbench run's processes into
Parquet files, losslessly, and makes them queryable with DuckDB.

```bash
wb run create-parquets RUN [--force] [--jobs N] [--keep-temp] [--zstd-level N]   # = bench-parquet convert-run
bench-parquet restore RUNDIR PROCESS -o FILE [--verify]
bench-parquet excerpt RUNDIR (--at TIMESTAMP | --ns NAMESPACE [--where SQL]) [--before S] [--after S] [--process P].. -o OUTDIR
bench-parquet lines RUNDIR PROCESS [--ns-file FILE]   # JSON lines in log order (for locli)
bench-parquet host-summary RUNDIR PROCESS             # counts per namespace, first/last timestamp (for locli)
bench-parquet query RUNDIR          # interactive DuckDB session
bench-parquet run RUNDIR FILE.sql   # the same, for a SQL file
bench-parquet query RUNDIR --ui     # query, plus the DuckDB UI on the same session in a new browser tab
```

Needs `duckdb` (from `PATH`, or `DUCKDB=/path/to/duckdb`), `jq` and coreutils.

## Layout

One file per process (each process has exactly one stdout), metadata in `meta/`:

```
parquets/node-0.parquet
parquets/node-1.parquet
parquets/generator.parquet
...
parquets/meta/files.parquet   one row per converted log: size, sha256, line counts
parquets/meta/run.parquet     run id, genesis system start and slot length, versions
```

So ad-hoc queries work without a session: `FROM 'parquets/node-3.parquet'`, `FROM 'parquets/node-*.parquet'`
(all nodes), `FROM 'parquets/*.parquet'` (all logs, generator included).

stderr is not converted: it carries no JSON lines and is fetched with the rest of a process's
directory. Conversion lists the size of each non-empty stderr, without judging it (a process may
recover from what it reports there).

Conversion writes to `RUN/.parquets.tmp.<pid>/` (spill directories inside) and renames it to
`parquets/` only when every log converted; with `--force` the old `parquets/` is replaced only then.

Scheduling: logs are converted largest first, in `--jobs` slots (default: from available memory and
cores; a cgroup memory limit, e.g. of a Nomad alloc, counts), each slot 4 DuckDB threads and a
4 GiB memory limit, budgeted at 5.5 GiB. A log larger than an even share
of the work (total size / slots) gets round(size × slots / total) slots as one job with that many times
the threads and memory, so it finishes about when the rest does. Each log is read once: the same read
feeds its sha256, the check for CR / 0x1e characters and DuckDB.

## Schema (raw schema version 1)

| column    | type           | content |
|-----------|----------------|---------|
| `process` | `VARCHAR`      | the process directory (`node-0`, `generator`) |
| `seq`     | `UBIGINT`      | line number in the original log |
| `at`      | `TIMESTAMP_NS` | envelope `at` |
| `ns`      | `VARCHAR`      | envelope `ns` |
| `sev`     | `VARCHAR`      | envelope `sev` |
| `thread`  | `VARCHAR`      | envelope `thread` |
| `host`    | `VARCHAR`      | envelope `host` (the generator's is the machine's hostname) |
| `data`    | `VARCHAR`      | the payload, verbatim; query it as JSON: `data->>'$.slot'` |
| `line`    | `VARCHAR`      | the original line, only where it doesn't rebuild exactly from the columns above |

Rows are sorted by `ns, at, seq`; lines without a namespace (text, malformed JSON) come last with
`at`/`ns` NULL. Every JSON line is rebuilt during conversion and compared with the original; a line that
differs keeps its original text in `line`, so `restore` reproduces every log byte for byte
(`--verify` checks the sha256 recorded at conversion). Lines containing CR or the ASCII record
separator (0x1e) are rejected.

## Query sessions

`query` and `run` load:

- `raw` (the nodes), `raw_generator` (the generator), `run_meta`, `files`. Combining node and generator
  logs is explicit in a query (`raw UNION ALL BY NAME raw_generator`, or a join), never the default;
- one view per namespace the nodes emit, named by the quoted namespace: `FROM "Forge.Loop.ForgedBlock"`
  (none for generator namespaces: tx-generator and tx-centrifuge need aligning first)
  (DuckDB matches identifiers case-insensitively, so namespaces differing only in case are reported at
  conversion);
- macros for stable fields (strict casts): `slot_no`, `mempool_txs`, `mempool_bytes`, `res_cpu`,
  `res_rss`, `res_heap`, `res_live`, `res_alloc`, `res_gcs_major`, …; `sev_level(sev) >= 'Warning'`;
- `bp_rebuild_line` / `bp_fmt_at` (the original line of a row);
- curated views from `sql/curated/`, loaded in file-name order (a view can only refer to views
  created before it): `forge_steps`; `slot` and `block_ranking`, ported from Federico's Leios analysis;
  `trace_pauses(threshold_s := 2)`, a table macro: gaps in each node's trace output with GC attribution
  from the surrounding `Resources` samples. Nodes trace about once a second (`Resources`, leadership
  checks), so thresholds below 1 s mostly find ordinary quiet periods; `heap_per_slot` (process, slot,
  heap, live, RSS; select nodes with `WHERE process IN (…)`); `mempool_timeout_hard` and
  `mempool_timeout_soft` (one row per timeout) and `mempool_timeouts` (per node and kind, composed from
  the two).
- `slot_at(t)`: a timestamp on the run's slot grid (genesis system start and slot length).

DuckDB pitfalls:

- `at` must be quoted in some positions (`ORDER BY "at"`).
- **Parenthesise JSON extractions in comparisons**: `(f.data->>'$.block') = (a.data->>'$.blockHash')`.
  Without the parentheses `=` binds first and the condition silently never matches.

## Input for locli

`wb analyse prepare-db` builds locli's per-host SQLite DBs from `parquets/` when it exists:

```bash
locli list-interpreted-namespaces > ns.txt
bench-parquet host-summary RUN node-3 > node-3.summary.json
bench-parquet lines RUN node-3 --ns-file ns.txt |
  locli prepare-db-from-lines --mach node-3 --summary node-3.summary.json --db node-3.sqlite3
```

- `lines` writes the JSON lines of the listed namespaces byte for byte as in the log, in log order
  (sorted by `seq`; at most 2 GiB and 2 threads per call, spilling beside the run). Text lines are
  never written: they are out of scope for the analysis.
- `host-summary` writes what locli otherwise counts while decoding every line: `tracefreqs` (JSON
  lines per namespace, the format of `wb analyse trace-frequencies`), `lines` (their sum), `firstAt` /
  `lastAt` (the first and last JSON line in log order). A truncated last line is a text row and doesn't
  count.

Measured: a 52-node host's log (1.26 GB, 4.9M lines) streams in 3 s; `host-summary` takes 0.1 s.
