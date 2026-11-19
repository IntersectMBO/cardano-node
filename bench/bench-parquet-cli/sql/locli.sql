-- Input for locli prepare-db-from-lines. Loaded after lib.sql (uses bp_fmt_at).

-- One process's host summary, as one JSON value: trace frequencies per namespace,
-- the JSON line count, and the timestamps of the first and last JSON line (in log
-- order, formatted as the node writes them).
CREATE OR REPLACE MACRO bp_host_summary(f) AS TABLE
  WITH j AS (SELECT ns, "at", seq FROM read_parquet(f) WHERE ns IS NOT NULL)
  SELECT json_object(
           'tracefreqs', (SELECT coalesce(json_group_object(ns, n), '{}'::JSON)
                          FROM (SELECT ns, count(*) AS n FROM j GROUP BY ns ORDER BY ns)),
           'lines',      (SELECT count(*) FROM j),
           'firstAt',    (SELECT bp_fmt_at(arg_min("at", seq)) FROM j),
           'lastAt',     (SELECT bp_fmt_at(arg_max("at", seq)) FROM j)) AS summary;
