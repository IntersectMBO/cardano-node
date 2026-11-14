# bench-parquet-cli

Prototype (bash + SQL): converts the process logs of a workbench run into Parquet files,
losslessly, and makes them queryable with DuckDB.

```bash
wb run create-parquets RUN [--force] [--mem-16g] [--jobs N] [--keep-temp]   # = bench-parquet convert-run
bench-parquet restore RUNDIR PROCESS [--stream stdout|stderr] -o FILE [--verify]
bench-parquet excerpt RUNDIR (--at TIMESTAMP | --ns NAMESPACE [--where SQL]) [--before S] [--after S] [--process P].. -o OUTDIR
bench-parquet query RUNDIR          # interactive DuckDB session
bench-parquet run RUNDIR FILE.sql   # the same, for a SQL file
```

Needs `duckdb` (from `PATH`, or `DUCKDB=/path/to/duckdb`), `jq` and coreutils.

## Layout

`RUN/parquets/` mirrors the processes' working directories:

```
parquets/node-0/stdout.parquet
parquets/node-0/stderr.parquet
parquets/generator/stdout.parquet
...
parquets/files.parquet   one row per converted log: size, sha256, line counts
parquets/run.parquet     run id, genesis system start and slot length, versions
```

Conversion writes to `RUN/.parquets.tmp.<pid>/` (spill directories inside) and renames it to
`parquets/` only when every log converted; with `--force` the old `parquets/` is replaced only then.

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

- `raw` (stdout of all processes), `raw_stderr`, `run_meta`, `files`;
- one view per namespace present in the run, named by the quoted namespace: `FROM "Forge.Loop.ForgedBlock"`
  (DuckDB matches identifiers case-insensitively, so namespaces differing only in case are reported at
  conversion);
- macros for stable fields (strict casts): `slot_no`, `mempool_txs`, `mempool_bytes`, `res_cpu`,
  `res_rss`, `res_heap`, `res_live`, `res_alloc`, `res_gcs_major`, …; `sev_level(sev) >= 'Warning'`;
- `bp_rebuild_line` / `bp_fmt_at` (the original line of a row);
- curated views from `sql/curated/` (e.g. `forge_steps`).

DuckDB keyword: `at` must be quoted in some positions (`ORDER BY "at"`).
