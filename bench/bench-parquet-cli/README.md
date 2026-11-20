# bench-parquet-cli

Prototype (bash + SQL): converts process logs into Parquet files, losslessly, and makes them
queryable with DuckDB. Independent of the workbench: every command except `convert` needs only a
directory written by `convert` (PQDIR: one `<process>.parquet` per log, plus `meta/`). The workbench
calls it for a run, with `RUN/parquets` as PQDIR.

```bash
wb parquet create RUN [--force] [--jobs N] [--keep-temp] [--zstd-level N]   # convert a workbench run
wb parquet query RUN, head RUN PROCESS, ...   # every command below, with a run instead of PQDIR
bench-parquet convert PQDIR --run-id ID [--system-start T --slot-length S] [--force] [--jobs N] [--keep-temp] [--zstd-level N] PROCESS=FILE..
bench-parquet restore PQDIR PROCESS -o FILE [--verify]
bench-parquet head PQDIR PROCESS [-n N]   # first N lines (default 20) of the log
bench-parquet tail PQDIR PROCESS [-n N]   # last N lines (default 20)
bench-parquet excerpt PQDIR (--at TIMESTAMP | --ns NAMESPACE [--where SQL]) [--before S] [--after S] [--process P].. -o OUTDIR
bench-parquet lines PQDIR PROCESS [--ns-file FILE]   # JSON lines in log order (for locli)
bench-parquet host-summary PQDIR PROCESS             # counts per namespace, first/last timestamp (for locli)
bench-parquet query PQDIR          # interactive DuckDB session
bench-parquet run PQDIR FILE.sql   # the same, for a SQL file
bench-parquet query PQDIR --ui     # query, plus the DuckDB UI on the same session in a new browser tab
```

Needs `duckdb` (from `PATH`, or `DUCKDB=/path/to/duckdb`) and coreutils; `wb parquet create` also
`jq`.

## Conversion

`convert` takes each log as `PROCESS=FILE`. Process names are letters, digits, `.`, `_`, `-` (not
`meta`). Roles come from the name: `node-*` is a node, `generator` the generator; query sessions need
at least one node. `--run-id` is recorded in `meta/run.parquet`, as are genesis system start and slot
length when given (`--system-start`, `--slot-length`, both or neither); without them they are NULL, and
slot arithmetic in a session (`slot_at`, `slot.slot_start`) is an error, never a NULL slot.

`wb parquet create RUN` (`nix/workbench/parquet.sh`) passes the run's knowledge: the stdout of `node-*` and `generator`, the run
id from `meta.json` (`.meta.tag`, else the directory name), system start and slot length from the
Shelley genesis. stderr is not converted: it carries no JSON lines and is fetched with the rest of a
process's directory; `wb` lists the size of each non-empty stderr, without judging it (a process may
recover from what it reports there).

## Layout

One file per process (each process has exactly one log), metadata in `meta/`:

```
parquets/node-0.parquet
parquets/node-1.parquet
parquets/generator.parquet
...
parquets/meta/files.parquet   one row per converted log: source path, size, sha256, line counts
parquets/meta/run.parquet     run id, genesis system start and slot length (or NULL), versions
```

So ad-hoc queries work without a session: `FROM 'parquets/node-3.parquet'`, `FROM 'parquets/node-*.parquet'`
(all nodes), `FROM 'parquets/*.parquet'` (all logs, generator included).

Conversion writes to `.<PQDIR name>.tmp.<pid>/` beside PQDIR (spill directories inside; the parent
directory must exist) and renames it to PQDIR only when every log converted; with `--force` the old
PQDIR is replaced only then.

Scheduling: logs are converted largest first, in `--jobs` slots (default: from available memory and
cores; a cgroup memory limit, e.g. of a Nomad alloc, counts), each slot 4 DuckDB threads and a
4 GiB memory limit, budgeted at 5.5 GiB. A log larger than an even share
of the work (total size / slots) gets round(size × slots / total) slots as one job with that many times
the threads and memory, so it finishes about when the rest does. Each log is read once: the same read
feeds its sha256, the check for CR / 0x1e characters and DuckDB.

## Schema (raw schema version 2)

| column    | type           | content |
|-----------|----------------|---------|
| `process` | `VARCHAR`      | the process directory (`node-0`, `generator`) |
| `seq`     | `UBIGINT`      | line number in the original log |
| `ts`      | `TIMESTAMP_NS` | envelope `at` (renamed: `at` is reserved in DuckDB, unquoted a syntax error) |
| `ns`      | `VARCHAR`      | envelope `ns` |
| `sev`     | `VARCHAR`      | envelope `sev` |
| `thread`  | `VARCHAR`      | envelope `thread` |
| `host`    | `VARCHAR`      | envelope `host` (the generator's is the machine's hostname) |
| `data`    | `VARCHAR`      | the payload, verbatim; query it as JSON: `data->>'$.slot'` |
| `line`    | `VARCHAR`      | the original line, only where it doesn't rebuild exactly from the columns above |

Rows are sorted by `ns, ts, seq`; lines without a namespace (text, malformed JSON) come last with
`ts`/`ns` NULL. Every JSON line is rebuilt during conversion and compared with the original; a line that
differs keeps its original text in `line`, so `restore` reproduces every log byte for byte
(`--verify` checks the sha256 recorded at conversion; it refuses to write over the converted log). Lines containing CR or the ASCII record
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
- `bp_line(t)` / `bp_rebuild_line` / `bp_fmt_at` (the original line of a row); `bp_head(file, n)` /
  `bp_tail(file, n)`: the first / last n lines of one process's file, in log order;
- curated views from `sql/curated/`, loaded in file-name order (a view can only refer to views
  created before it): `forge_steps`; `slot` and `block_ranking`, ported from Federico's Leios analysis;
  `trace_pauses(threshold_s := 2)`, a table macro: gaps in each node's trace output with GC attribution
  from the surrounding `Resources` samples. Nodes trace about once a second (`Resources`, leadership
  checks), so thresholds below 1 s mostly find ordinary quiet periods; `heap_per_slot` (process, slot,
  heap, live, RSS; select nodes with `WHERE process IN (…)`); `mempool_timeout_hard` and
  `mempool_timeout_soft` (one row per timeout) and `mempool_timeouts` (per node and kind, composed from
  the two).
- `slot_at(t)`: a timestamp on the run's slot grid; `run_system_start()`, `run_slot_length_s()`: the
  genesis values (an error when converted without them).

DuckDB pitfalls:

- **Parenthesise JSON extractions in comparisons**: `(f.data->>'$.block') = (a.data->>'$.blockHash')`.
  Without the parentheses `=` binds first and the condition silently never matches.

## Input for locli

`wb analyse prepare-db` builds locli's per-host SQLite DBs from `parquets/` when it exists:

```bash
locli list-interpreted-namespaces > ns.txt
bench-parquet host-summary RUN/parquets node-3 > node-3.summary.json
bench-parquet lines RUN/parquets node-3 --ns-file ns.txt |
  locli prepare-db-from-lines --mach node-3 --summary node-3.summary.json --db node-3.sqlite3
```

- `lines` writes the JSON lines of the listed namespaces byte for byte as in the log, in log order
  (sorted by `seq`; at most 2 GiB and 2 threads per call, spilling beside PQDIR). Text lines are
  never written: they are out of scope for the analysis.
- `host-summary` writes what locli otherwise counts while decoding every line: `tracefreqs` (JSON
  lines per namespace, the format of `wb analyse trace-frequencies`), `lines` (their sum), `firstAt` /
  `lastAt` (the first and last JSON line in log order). A truncated last line is a text row and doesn't
  count.

Measured: a 52-node host's log (1.26 GB, 4.9M lines) streams in 3 s; `host-summary` takes 0.1 s.
