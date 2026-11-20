-- Curated view: mempool timeouts per node and kind, composed from the two
-- relations of 21-mempool_timeouts.sql.
--
--   SELECT * FROM mempool_timeouts;                    -- nodes with timeouts
--   SELECT * FROM mempool_timeouts WHERE kind = 'hard';
--
-- One row per (process, kind) with at least one timeout; no row means none.
-- Timeouts are a sanity matter for a run (the node's protection against slow
-- txs), not a performance metric.
--
-- Columns: process, kind ('hard' | 'soft'), timeouts (count), unknown_durations
--   (timeouts whose duration couldn't be read; hard only), max_duration_s (over
--   the known ones), first_at, last_at.
CREATE OR REPLACE VIEW mempool_timeouts AS
SELECT
    process
  , kind
  , count(*)          AS timeouts
  , count(*) - count(duration_s) AS unknown_durations
  , max(duration_s)   AS max_duration_s
  , min(ts)         AS first_at
  , max(ts)         AS last_at
FROM (     SELECT process, ts, duration_s, 'hard' AS kind FROM mempool_timeout_hard
 UNION ALL SELECT process, ts, duration_s, 'soft' AS kind FROM mempool_timeout_soft)
GROUP BY ALL;
