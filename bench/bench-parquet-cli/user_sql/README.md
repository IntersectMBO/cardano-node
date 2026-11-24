# user_sql

Analysts' SQL on top of the package's views (`../sql/`): shared here, but not
reviewed or tested like `sql/`, and never loaded by default.

- `views/`: views to add to a session (`query PQDIR --load user_sql/views`).
- `queries/`: queries to run on one (`bench-parquet run PQDIR FILE`).
- `queries/locli/`: locli's block propagation and cluster performance rebuilt as
  table macros, to cross-check a locli analysis of the same run (`*_diff` macros);
  `--load user_sql/queries/locli`.

Caveats:
- Don't name your views `v_*` or `chk_*`; those prefixes belong to `sql/`.
- `--load PATH` (`query`, `run`; repeatable, read in the order given) reads a file,
  or every `*.sql` directly in a directory, after the package views.
- Files may be out of date with the package views; a file that no longer loads
  prints its error and the session goes on without it.
- Inside a directory, byte order of the file names is load order: DuckDB binds
  a view's or macro's references when it is created, so a file must sort after
  the files it uses.
- Something that proves generally useful moves to the matching place in `sql/`
  (renamed, reviewed, documented per `sql/README.md`).
