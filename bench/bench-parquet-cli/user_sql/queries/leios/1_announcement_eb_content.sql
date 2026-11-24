-- "1. Endorser block contents": what an announced endorser block holds, one row
-- per metric, over every endorser-block forging in the run.
--
-- USER CONTENT (user_sql/README.md): not reviewed or tested like sql/; the
-- Leios observables aren't final, so it may break with a node change.
--
-- Columns: metric, samples, p50, p90, max, mean, at_max.
WITH
  ebs AS MATERIALIZED (SELECT bytes, tx_refs_count FROM v_eb_forged),
  vals AS (
          SELECT 1 AS ord, 'tx references per EB' AS metric, tx_refs_count * 1.0 AS v FROM ebs
    UNION ALL SELECT 2, 'EB bytes, the reference list', bytes * 1.0 FROM ebs
    UNION ALL SELECT 3, 'bytes per reference', bytes * 1.0 / tx_refs_count FROM ebs WHERE tx_refs_count > 0),
  ranked AS (
    SELECT ord, metric, v
         , row_number() OVER (PARTITION BY ord ORDER BY v) AS rn
         , count(*)     OVER (PARTITION BY ord)            AS n
         , max(v)       OVER (PARTITION BY ord)            AS mx
    FROM vals)
SELECT
    metric
  , n                 AS samples
  , max(CASE WHEN rn = least(CAST(floor(n * 0.50) AS BIGINT), n - 1) + 1 THEN v END) AS p50
  , max(CASE WHEN rn = least(CAST(floor(n * 0.90) AS BIGINT), n - 1) + 1 THEN v END) AS p90
  , mx                AS max
  , round(avg(v), 4)  AS mean
  , count_if(v = mx)  AS at_max
FROM ranked
GROUP BY ord, metric, n, mx
ORDER BY ord;
