-- "Endorser block, wire delivery to kernel acquisition": ONE ROW, the
-- distribution of the gap between the bytes arriving and the kernel calling the
-- body acquired, over every (EB, peer) pair that has both.
--
-- USER CONTENT (user_sql/README.md): not reviewed or tested like sql/; the
-- Leios observables aren't final, so it may break with a node change.
--
-- Columns: pairs, avg, min, p50, p90, p99, max, over_1s, over_2s, over_4s (ms).
WITH
  delivered AS MATERIALIZED (
    SELECT hash, process, min(ts) AS ts FROM v_eb_deliveries GROUP BY hash, process),
  acquired AS MATERIALIZED (
    -- Consensus.LeiosKernel.BlockAcquired is v_eb_writes' trace.
    SELECT hash, process, min(ts) AS ts FROM v_eb_writes GROUP BY hash, process),
  gap AS MATERIALIZED (
    SELECT (epoch_ns(a.ts) - epoch_ns(d.ts)) / 1e6 AS ms
    FROM delivered d
    JOIN acquired a ON a.hash = d.hash AND a.process = d.process
    WHERE NOT EXISTS (SELECT 1 FROM v_eb_forged g WHERE g.hash = d.hash AND g.process = d.process)),
  ranked AS (
    SELECT ms, row_number() OVER (ORDER BY ms) AS rn, count(*) OVER () AS n FROM gap)
SELECT
    n                    AS pairs
  , round(avg(ms), 3)    AS avg
  , min(ms)              AS min
  , max(CASE WHEN rn = least(CAST(floor(n * 0.50) AS BIGINT), n - 1) + 1 THEN ms END) AS p50
  , max(CASE WHEN rn = least(CAST(floor(n * 0.90) AS BIGINT), n - 1) + 1 THEN ms END) AS p90
  , max(CASE WHEN rn = least(CAST(floor(n * 0.99) AS BIGINT), n - 1) + 1 THEN ms END) AS p99
  , max(ms)              AS max
  , count_if(ms > 1000)  AS over_1s
  , count_if(ms > 2000)  AS over_2s
  , count_if(ms > 4000)  AS over_4s
FROM ranked
GROUP BY n;
