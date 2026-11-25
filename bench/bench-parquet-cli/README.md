# bench-parquet-cli

Prototype (bash + SQL): converts process logs into Parquet files, losslessly, and makes them
queryable with DuckDB. Independent of the workbench: every command except `convert` needs only a
directory written by `convert` (PQDIR: one `<process>.parquet` per log, plus `meta/`). The workbench
calls it for a run, with `RUN/parquets` as PQDIR.

Code: `bench-parquet` holds the usage, shared helpers and the command dispatch; the commands live in
`lib/` by area (`slots.sh` scheduling, `convert.sh`, `session.sh` for query / run / check,
`restore.sh` with verify, `logs.sh` for lines / host-summary / head / tail / excerpt, `remote.sh`),
sourced by it; their SQL lives in `sql/`.

New here? [HOWTO.md](HOWTO.md) walks through converting, querying and checking a run.

```bash
wb parquet create RUN [--force] [--delete] [--jobs N] [--keep-temp] [--zstd-level N]   # convert a workbench run
wb parquet restore RUN [PROCESS..] [--jobs N]   # write missing stdout files back into the run, verified
wb parquet query RUN, head RUN PROCESS, ...   # every command below, with a run instead of PQDIR
bench-parquet convert PQDIR --run-id ID [--system-start T --slot-length S] [--epoch-length N] [--force | --add] [--jobs N] [--keep-temp] [--zstd-level N] PROCESS=FILE..
bench-parquet restore PQDIR PROCESS -o FILE [--verify]
bench-parquet restore PQDIR PROCESS=FILE.. [--jobs N]   # several logs, scheduled, verified, existing FILEs skipped
bench-parquet verify PQDIR [PROCESS..] [--jobs N]   # logs rebuilt in memory against the recorded sha256
bench-parquet head PQDIR PROCESS [-n N]   # first N lines (default 20) of the log
bench-parquet tail PQDIR PROCESS [-n N]   # last N lines (default 20)
bench-parquet excerpt PQDIR (--at TIMESTAMP | --ns NAMESPACE [--where SQL]) [--before S] [--after S] [--process P].. -o OUTDIR
bench-parquet lines PQDIR PROCESS [--ns-file FILE]   # JSON lines in log order (for locli)
bench-parquet host-summary PQDIR PROCESS             # counts per namespace, first/last timestamp (for locli)
bench-parquet query PQDIR          # interactive DuckDB session
bench-parquet run PQDIR FILE.sql   # the same, for a SQL file
bench-parquet run PQDIR NAME       # a prepared query by name (sql/queries/*/NAME.sql); all rows; --csv for CSV
bench-parquet check PQDIR [NAME..]   # the package checks, one tab-separated line per violation
bench-parquet query PQDIR --ui     # query, plus the DuckDB UI on the same session in a new browser tab
bench-parquet remote SSH-HOST COMMAND PQDIR ARGS..   # query, run, check, ... on PQDIR on another host
wb parquet remote RUN COMMAND ARGS..                 # the same for RUN on $WB_REMOTE
```

Needs `duckdb` (from `PATH`, or `DUCKDB=/path/to/duckdb`), bash ≥ 4 and GNU coreutils (on macOS from
Nix or Homebrew: the system bash is 3.2, and `stat -c`, `realpath -m` are GNU); `wb parquet create`
also `jq`.

## Conversion

`convert` takes each log as `PROCESS=FILE`. Process names are letters, digits, `.`, `_`, `-` (not
`meta`). Roles come from the name: `node-*` is a node, `generator` the generator; query sessions need
at least one node. Any other name (e.g. `explorer`) is converted, verified and restored like the
others, but no session view reads it except `files`. `--run-id` is recorded in `meta/run.parquet`, as are genesis system start and slot
length when given (`--system-start`, `--slot-length`, both or neither), and the epoch length
(`--epoch-length`); without them they are NULL, and slot or epoch arithmetic in a session (`slot_at`,
`slot_start`, `v_slot`, `run_epoch_length()`) is an error, never a NULL slot.

`--add` converts into an existing PQDIR only the logs it lacks (a `PROCESS.parquet` with its row in
`meta/files.parquet`) and merges the metadata; the run id and the converter version must match the
recorded ones, genesis values the PQDIR lacks are added, a differing one is an error (all checked
before anything is converted).

`wb parquet create RUN` (`nix/workbench/parquet.sh`) passes the run's knowledge: the stdout of `node-*` and `generator`, the run
id from `meta.json` (`.meta.tag`, else the directory name), system start, slot length and epoch length
from the Shelley genesis. A log already in `parquets/` is skipped (`--add`); `--force` converts all
anew. With `--delete`, each process's stdout is deleted once `verify` confirms its log; one that
doesn't verify stays. `wb parquet restore RUN [PROCESS..]` writes missing stdout files back
(`restore PQDIR PROCESS=FILE..`); an existing one is skipped. stderr is not converted: it carries no JSON lines and is fetched with the rest of a
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

Scheduling: logs are converted largest first, in `--jobs` slots (default: min(memory / 5.5 GiB,
cores / 4), with memory and cores as DuckDB sees them: RAM or a cgroup limit, e.g. of a Nomad alloc,
and cores or a cgroup CPU quota; on Linux memory is lowered to MemAvailable), each slot 4 DuckDB threads and a
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
- the package views and checks from `sql/` (see [`sql/README.md`](sql/README.md): tiers, kinds,
  naming, template): `v_*` views in three tiers (events; joined; composed), loaded in tier order, and
  `chk_*` checks (one row per violation; `bench-parquet check PQDIR` runs them, `wb run
  sanity-check RUN` judges them with the run's exit codes and stderr). Each has a comment: `SELECT view_name, comment FROM
  duckdb_views() WHERE comment IS NOT NULL`, or
  the DuckDB UI (`query --ui`). Prepared queries (`sql/queries/series/`: series for plots; `sql/queries/reports/`: report tables, e.g. `node_summary`, one row per node) run on
  demand: `bench-parquet run PQDIR heap_bumps` (by name, or by path). Analysts' own SQL lives in
  `user_sql/`, never loaded by default (`query` / `run` with `--load PATH`, repeatable);
- `slot_at(t)`: a timestamp on the run's slot grid; `slot_start(s)`: when slot s began;
  `run_system_start()`, `run_slot_length_s()`, `run_epoch_length()`: the genesis values (an error
  when converted without them).

DuckDB pitfalls:

- **Parenthesise JSON extractions in comparisons**: `(f.data->>'$.block') = (a.data->>'$.blockHash')`.
  Without the parentheses `=` binds first and the condition silently never matches.

## Remote runs

`remote SSH-HOST [--memory SIZE] [--threads N] [--spill-limit SIZE] COMMAND PQDIR ARGS..` runs
`query`, `run`, `check`, `verify`, `head`, `tail`, `lines` or `host-summary` on another host, over a
PQDIR there (absolute, or relative to the home directory), and brings back output and exit status.
Only results cross the network; the Parquet files stay where they are. `wb parquet remote RUN COMMAND
ARGS..` does the same for a run on `$WB_REMOTE` (host `.env`, checkout `.depl`, run directory `.dir`,
default `run`).

What runs on the remote is exactly the local version, uncommitted edits included:

- **The package** (the script, `lib/*.sh` and the `*.sql` files of `sql/` and `user_sql/`) is shipped as a tar into
  `~/.cache/bench-parquet/pkg/HASH/` the first time a version is used (HASH: of its content).
  Versions unused for 30 days are removed by the next call (and shipped again if used later).
- **The tools** (bash, duckdb, coreutils, gawk, sed, findutils) come from the nixpkgs revision the
  enclosing flake locks (its `nixpkgs` input in `flake.lock`). `lib/remote-tools.nix` evaluates their
  store paths locally for x86_64-linux, aarch64-linux and aarch64-darwin (nothing is built; cached in
  `~/.cache/bench-parquet/tools/` until `flake.lock` changes); the remote picks its system's paths,
  fetches them with `nix-store -r` from its substituters (cache.nixos.org) and runs with nothing else
  on `PATH`. So the local side needs nix and a `flake.lock` above the package; the remote needs nix
  and that substituter, but no checkout, no build and no trusted user, and may be another platform
  than the local machine. Without GC roots: after a garbage collection on the remote, the next call
  fetches the tools again. A different nixpkgs (e.g. another DuckDB) comes with a flake bump.
- **Local SQL files** given to `run`, `--load` or `lines --ns-file` are shipped per call into
  `~/.cache/bench-parquet/req/` and removed afterwards; files of the package are used from its
  shipped copy.

Each remote session is bounded: 4 GB of memory and 4 threads (`--memory`, `--threads`), spilling into
its own directory under `~/.cache/bench-parquet/spill/` (removed afterwards), at most 10 GB
(`--spill-limit`). On a 52-node Leios run, package views, checks and prepared queries stay within these
bounds without spilling; the locli cross-check needs more memory. Locally, sessions take the same bounds
from `BENCH_PARQUET_MEMORY`, `BENCH_PARQUET_THREADS`, `BENCH_PARQUET_SPILL` and
`BENCH_PARQUET_SPILL_LIMIT` when set (DuckDB's defaults otherwise).

`query` is interactive (over `ssh -t`) when stdin is a terminal and there is no `-c`; `--ui` is not
available remotely. `convert`, `restore` and `excerpt` write files and are not available remotely.

## Verification

`verify` rebuilds each log from its Parquet file, streams it into sha256 and compares with the sha256
recorded at conversion; nothing is written. Scheduled like conversion (slots of 4 GiB and 4 threads,
larger logs more slots; DuckDB spills beside PQDIR where a log doesn't fit). On stdout one line per
log, `PROCESS<TAB>ok` or `PROCESS<TAB>failed`; the exit status is 1 if any failed.

`restore` uses the same scheduling. `restore PQDIR PROCESS=FILE..` writes each FILE through
`FILE.restoring`, verifies it and renames it into place only then (a failed log leaves nothing); an
existing FILE is skipped. A single `restore PQDIR PROCESS -o FILE` gets all the slots' memory and
threads; `--verify` checks the written file the same way.

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

A 52-node host's log (1.26 GB, 4.9M lines) streams in about 3 s; `host-summary` takes about 0.1 s.
