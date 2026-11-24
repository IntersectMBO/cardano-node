# bench-parquet: getting started

A benchmark run's process logs (the JSON lines each node and the generator write
to stdout) as Parquet files, queried with DuckDB. Conversion is lossless: every log
can be restored byte for byte. This page walks through the everyday tasks; the
[README](README.md) has the details, [`sql/README.md`](sql/README.md) the views.

Two ways in:

- **`wb parquet ...`** (short: `wb pq`) in the workbench shell: works on a run by
  name (`RUN`), finds its logs and genesis itself.
- **`bench-parquet ...`** anywhere else: works on a directory of converted logs
  (`PQDIR`, a run's `parquets/`). Needs `duckdb`, bash 4 or later and GNU coreutils.

Every query-side command exists in both forms: `wb pq query RUN` is
`bench-parquet query RUN-DIR/parquets` (`wb pq path RUN` prints that directory).

## 1. Convert a run

```bash
wb pq create RUN                 # every node-*/stdout, generator/stdout and explorer/stdout into RUN/parquets/
wb pq create RUN --delete        # ... then delete each stdout once its Parquet copy verifies
```

A log already in `parquets/` is skipped, so `create` can be run again after more
logs arrived; `--force` converts everything anew. Conversion runs logs in
parallel as memory and cores allow; a 52-node cluster run takes minutes.

Without the workbench, name the logs yourself:

```bash
bench-parquet convert PQDIR --run-id my-run \
  --system-start 2026-09-28T12:24:00Z --slot-length 1 --epoch-length 600 \
  node-0=run/node-0/stdout node-1=run/node-1/stdout generator=run/generator/stdout
```

Processes named `node-*` are nodes, `generator` the generator; any other process
(such as the explorer) is archived, but left out of query sessions. The genesis values
are optional, but without them slot arithmetic in queries is an error.

## 2. Look at the logs

```bash
bench-parquet head PQDIR node-0 -n 5        # first lines, exactly as in the original
bench-parquet tail PQDIR node-0 -n 5
bench-parquet excerpt PQDIR --ns Forge.Loop.ForgedBlock --where "data->>'\$.slot' = '13'" \
  --before 1 --after 1 -o /tmp/excerpt      # every log, 1 s around that event, one file each
wb pq restore RUN                           # deleted stdout files back into the run
bench-parquet verify PQDIR                  # all logs rebuilt and checked against their sha256
```

## 3. Query

```bash
wb pq query RUN            # interactive DuckDB session over the run's logs
wb pq query RUN --ui       # the same, plus the DuckDB UI in a browser tab
wb pq query RUN -csv -c "SELECT ..."        # one query, CSV out (any duckdb option works)
```

What a session has:

- `raw`: every node log line, one row each: `process`, `seq` (line number), `ts`,
  `ns` (namespace), `sev`, `thread`, `host`, `data` (the payload, JSON text).
  `raw_generator` is the same for the generator.
- One view per namespace, named by it in double quotes: `FROM "Forge.Loop.ForgedBlock"`.
- The package views `v_*` (blocks, forging, mempool, slots, ...) and checks `chk_*`.
  Each has a one-line description:

  ```sql
  SELECT view_name, comment FROM duckdb_views() WHERE comment IS NOT NULL ORDER BY view_name;
  ```
- Macros for common fields and the slot grid: `slot_no(data)`, `res_heap(data)`,
  `slot_at(ts)`, `slot_start(s)`, `run_epoch_length()`.

First queries:

```sql
-- How many blocks did each node forge?
SELECT process, count(*) AS forged FROM v_block_forged GROUP BY process ORDER BY process;

-- The busiest namespaces.
SELECT ns, count(*) AS n FROM raw GROUP BY ns ORDER BY n DESC LIMIT 10;

-- A field straight from the payload.
SELECT ts, process, data->>'$.block' AS hash, slot_no(data) AS slot
FROM "Forge.Loop.ForgedBlock" ORDER BY ts LIMIT 5;
```

Pitfalls:

- **Parenthesise JSON extractions in comparisons**:
  `(a.data->>'$.block') = (b.data->>'$.blockHash')`. Without the parentheses the
  condition silently never matches.
- The time column is `ts` (a timestamp with nanoseconds); `epoch_ns(ts)` gives an
  integer for arithmetic.
- Rows have no order unless you ask for one: `ORDER BY ts` or `ORDER BY seq`.

## 4. Prepared queries

Queries shipped with the package (`sql/queries/`): series for plots and report
tables. Run them by name:

```bash
wb pq run RUN node_summary                  # one row per node: missed slots, blocks, forks, memory, ...
wb pq run RUN heap_bumps
bench-parquet run PQDIR mempool_size_txs
bench-parquet run PQDIR my-query.sql        # or any SQL file of your own
```

An unknown name lists the available ones.

## 5. Check a run

```bash
wb run sanity-check RUN    # verdict in RUN/sanity-check.txt; exit status 1 on a fail
bench-parquet check PQDIR  # just the package checks, one tab-separated line per finding
```

`sanity-check` looks at exit codes, stderr and the logs of every process, and,
once `parquets/` exists, runs the package checks (blocks without a known forger,
missed slots, mempool timeouts, nodes that started late or stopped early, ...).
Each finding is `fail`, `warn`, `pass` or `skip`, with what was observed and what
was expected.

## 6. Your own SQL

Analysts' views and queries go to [`user_sql/`](user_sql/README.md); they are never
loaded by default. Load a file or a directory into a session with `--load`:

```bash
wb pq query RUN --load bench/bench-parquet-cli/user_sql/views
```

For example, the locli cross-check: locli's block propagation and cluster
performance rebuilt in SQL, compared with a locli analysis of the same run:

```bash
wb pq query RUN --load bench/bench-parquet-cli/user_sql/queries/locli
```

```sql
SELECT * FROM locli_clusterperf_report();
SELECT * FROM locli_blockprop_diff('RUN-DIR/analysis/blockprop.json',
                                   filters := locli_filter(first_slot := 16000)) WHERE NOT ok;
```

Something that proves generally useful moves into `sql/`, following
[`sql/README.md`](sql/README.md).
