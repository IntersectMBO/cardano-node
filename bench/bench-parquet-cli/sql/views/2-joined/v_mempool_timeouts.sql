-- "v_mempool_timeouts": mempool timeouts per node and kind, counted. Tier 2,
-- over v_mempool_timeout_hard and v_mempool_timeout_soft.
--
--   SELECT * FROM v_mempool_timeouts;                    -- nodes with timeouts
--   SELECT * FROM v_mempool_timeouts WHERE kind = 'hard';
--
-- ONE ROW PER (process, kind) WITH AT LEAST ONE TIMEOUT; no row means none.
-- Timeouts are a sanity matter for a run (the node's protection against slow
-- txs), not a performance metric; chk_mempool_timeouts reports each one.
--
-- Fields:
-- --------
-- * process            The node.
--   (NOT NULL)
--
-- * kind               'hard' or 'soft'.
--   (NOT NULL)
--
-- * timeouts           How many.
--   (NOT NULL)
--
-- * unknown_durations  Timeouts whose duration couldn't be read (hard only).
--   (NOT NULL)
--
-- * max_duration_s     The longest known duration, seconds.
--
-- * first_ts           The first and last timeout.
-- * last_ts
--   (NOT NULL)
CREATE OR REPLACE VIEW v_mempool_timeouts AS
SELECT
    process
  , kind
  , count(*)                     AS timeouts
  , count(*) - count(duration_s) AS unknown_durations
  , max(duration_s)              AS max_duration_s
  , min(ts)                      AS first_ts
  , max(ts)                      AS last_ts
FROM (     SELECT process, ts, duration_s, 'hard' AS kind FROM v_mempool_timeout_hard
 UNION ALL SELECT process, ts, duration_s, 'soft' AS kind FROM v_mempool_timeout_soft)
GROUP BY ALL;

COMMENT ON VIEW v_mempool_timeouts IS 'tier 2: mempool timeouts counted per (process, kind): timeouts, unknown_durations, max_duration_s, first_ts, last_ts';
