-- "locli_clusterperf_diff(json_path, ...)": the cross-check of cluster
-- performance. Compares every metric of locli_clusterperf (all sections) with
-- the same metric in a locli analysis of the same run (analysis/clusterperf.json),
-- one row per (metric, statistic). Needs blockprop.sql and clusterperf.sql;
-- the slot range of filters must be the one that analysis used (summary.json
-- .sumFilters; EpochGEq e is first_slot := e * run_epoch_length()).
--
--   SELECT * FROM locli_clusterperf_diff('RUN/analysis/clusterperf.json')
--   WHERE NOT ok;
--
-- PURPOSE: cross-checking locli. User content (user_sql/README.md): not
-- reviewed like sql/.
--
-- TOLERANCE as in locli_blockprop_diff: ok is true when the two values differ by
-- at most abs_tol (default 1e-6) or by rel_tol (default 1e-9) of locli's value;
-- sizes must match exactly. Centile values compare with the mean over the nodes
-- that clusterperf.json keeps per centile (cdfSamples[c].cdfAverage).
--
-- Fields:
-- --------
-- * section    The metric's section (clusterperf.sql); full for a metric in
--              no report table.
-- * metric     As in clusterperf.json ("mpResourceCDFs.CentiCpu", "cdfSnapTaken", ...).
-- * statistic  size, average, median, stddev, min, max, or "centile 0.95" etc.
-- * ours       This side's value.
-- * locli      The value in clusterperf.json (NULL when it has no such field).
-- * ok         Within tolerance.
CREATE OR REPLACE MACRO locli_clusterperf_diff(json_path, filters := NULL, epoch_length := NULL,
                                               abs_tol := 1e-6, rel_tol := 1e-9) AS TABLE
WITH
  j AS (SELECT content::JSON AS j FROM read_text(json_path)),
  o AS (
    SELECT c.*, json_extract(j.j, '/' || replace(c.metric, '.', '/')) AS x
    FROM locli_clusterperf(filters := filters, epoch_length := epoch_length) c, j),
  stats AS (
              SELECT section, metric, 'size' AS statistic, size::DOUBLE AS ours, (x->>'cdfSize')::DOUBLE AS locli, true AS exact FROM o
    UNION ALL SELECT section, metric, 'average', average, (x->>'$.cdfAverage.cdfAverage')::DOUBLE, false FROM o
    UNION ALL SELECT section, metric, 'median',  median,  (x->>'cdfMedian')::DOUBLE, false FROM o
    UNION ALL SELECT section, metric, 'stddev',  stddev,  (x->>'cdfStddev')::DOUBLE, false FROM o
    UNION ALL SELECT section, metric, 'min',     min,     (x->>'$.cdfRange[0]')::DOUBLE, false FROM o
    UNION ALL SELECT section, metric, 'max',     max,     (x->>'$.cdfRange[1]')::DOUBLE, false FROM o
    UNION ALL
      SELECT section, metric, printf('centile %g', cv.centile), cv.value,
             (SELECT (s->>'$[1].cdfAverage')::DOUBLE
              FROM (SELECT unnest(json_extract(x, '$.cdfSamples[*]')) AS s)
              WHERE (s->>'$[0]')::DOUBLE = cv.centile), false
      FROM (SELECT section, metric, x, unnest(centiles) AS cv FROM o))
SELECT section, metric, statistic, ours, locli,
       CASE WHEN exact THEN ours = locli
            ELSE abs(ours - locli) <= greatest(abs_tol, rel_tol * abs(locli)) END AS ok
FROM stats;
