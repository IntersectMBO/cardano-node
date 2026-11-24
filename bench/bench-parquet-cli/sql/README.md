# bench-parquet SQL

The SQL of `bench-parquet`: what a query session over a PQDIR loads, and the queries
shipped with it. Analysts' own SQL lives next door in `../user_sql/` (not reviewed
like this directory, never loaded by default).

## Layout

```
sql/
  lib.sql session.sql convert.sql locli.sql ui.sql   internal, by consumer
  views/1-events/      tier 1  ┐
  views/2-joined/      tier 2  │ loaded by every query session, in this order
  views/3-composed/    tier 3  │
  checks/                      ┘
  queries/series/      prepared queries, run on demand
  queries/reports/     report tables, run on demand (node_summary; the Leios
                       report tables are in user_sql/ until their observables
                       are final)
```

A session loads `lib.sql` and `session.sql` (the raw layer `raw` / `raw_generator`,
`run_meta`, `files`, field macros, the genesis frame), one view per namespace the
nodes emit (`"Forge.Loop.ForgedBlock"`, ...), then the tiers and the checks.

## Kinds

| Kind | Name | What a row is | Where |
|---|---|---|---|
| view, tier 1 | `v_*` | reads `raw` and macros only; mostly one event of one observable | `views/1-events/` |
| view, tier 2 | `v_*` | built on tier 1: events joined, e.g. with the forger's facts | `views/2-joined/` |
| view, tier 3 | `v_*` | built on tier 2: a block's whole life, the chain, per-node summaries | `views/3-composed/` |
| check | `chk_*` | one violation; no row means passed | `checks/` |
| series | file name | one point of a plot: x, series key, y | `queries/series/` |
| report table | file name | one figure or stage of a report section | `queries/reports/` |

**Tiers.** A view uses only lower tiers (and `raw`, macros, namespace views): it
goes into the lowest tier above everything it reads. Nothing in a tier depends on
another file of the same tier, so the order inside a directory doesn't matter.
DuckDB binds a view's references when it is created, so a dependency on a higher
tier fails at load time. Add a tier directory (`4-...`) only when a view really
needs to sit on tier 3.

**Checks** are views with fixed columns, the same for every check:
`check_name, process, ts, observed, expected` (`process` / `ts` NULL when the
violation is run-wide). They state facts only: what was seen, what was expected,
never a guessed cause. Whether a violation warns or fails is for the consumer
(e.g. `wb run sanity-check`) to decide.

**Prepared queries** are plain `SELECT`s over the views: they create nothing in the
session (helpers are CTEs). Run one by name, `bench-parquet run PQDIR heap_bumps`, or by path.

**Families.** Leios endorser-block views are `v_eb_*`, generator (tx-centrifuge)
views `v_gen_*`; both are experimental until their observables are final, and say so
in their header. They sit in the same tier directories as the rest.

## Conventions

- **One object per file, file name = object name** (`v_block_forged.sql`).
- **Every view gets a `COMMENT ON VIEW`** (`COMMENT ON MACRO TABLE` for a table
  macro): tier and one line on what a row is. It is the help text in the DuckDB UI
  and in `SELECT view_name, comment FROM duckdb_views()`.
- **NULL, never inferred.** Where no trace evidences a value, the cell is NULL.
  Reconstructions (e.g. adoptions no trace names) are not package views.
- **Strict casts** on JSON fields (`CAST`, not `TRY_CAST`): a changed type is an
  error, never a silent NULL. Unstructured text (exception show text) is no
  contract: NULL there, plus a count of the unreadable ones.
- **Read `raw` with `ns IN (...)`**, not the per-namespace views: a namespace that a
  run doesn't have then just gives no rows. Accept old and new spellings of a
  renamed namespace there.
- **Time is `ts`** (`TIMESTAMP_NS`); durations as seconds (`DOUBLE`) via
  `epoch_ns()`. The node is `process`. Slots: `slot_no(data)`, `slot_start(s)`,
  `slot_at(ts)`.
- **No `ORDER BY` in views** (the consumer sorts); queries sort.
- **Names follow the observability spec** (`v_block_forged`, not a Leios term).

## Template

A view's header gives its name and what one row is, then the rules that define the
set, in capitals, then the fields, each with whether and when it can be NULL. A
`COMMENT` may end with `(previously: NAME)`, the view's former name.

```sql
-- "v_block_writes": every WRITE of a block's body into a node's own
-- VolatileDB, one row per trace line. Tier 1.
--
-- NO ROLE, NO FILTER: the forger's own write is a row like any peer's.
--
-- Fields:
-- --------
-- * ts        When the node wrote it.
--   (NOT NULL)
-- * process   The node.
--   (NOT NULL)
-- * hash      The block.
--   (NOT NULL)
CREATE OR REPLACE VIEW v_block_writes AS
SELECT ts, process, data->>'$.block.hash' AS hash
FROM raw WHERE ns IN ('ChainDB.AddBlockEvent.AddedBlockToVolatileDB');

COMMENT ON VIEW v_block_writes IS 'tier 1: every block write into a node''s VolatileDB';
```

## Testing a change

Load a session on a converted run and query the view:
`bench-parquet query PQDIR` (or `--ui`), or `bench-parquet run PQDIR FILE`. A session
that loads without errors has bound every view and check.
