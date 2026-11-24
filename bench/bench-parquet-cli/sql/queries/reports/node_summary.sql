-- "node_summary": one row per node, the run at a glance: its slot window and
-- how much of it the node missed, the blocks it forged and adopted, its fork
-- switches, its peak memory, its mempool timeouts and the forge loop's lateness.
-- A report table.
--
-- EVERY NODE WITH A FORGE LOOP is a row (v_node_forge_loop: a node that traced
-- Forge.Loop.StartLeadershipCheck); a node that traced none has no window and
-- is not here. Counts are 0 where the node traced no such event; values the
-- node never sampled are NULL.
--
-- Fields:
-- --------
-- * process             The node.
--   (NOT NULL)
-- * first_slot          Its first slot entered.
--   (NOT NULL)
-- * last_slot           Its last slot entered.
--   (NOT NULL)
-- * missed_slots        Slots of that window it never entered.
--   (NOT NULL)
-- * missed_pct          Their share of the window, percent.
--   (NOT NULL)
-- * longest_missed_run  Its longest run of consecutive missed slots
--   (NULL: none missed).
-- * forged              Blocks it forged (v_block_forged).
--   (NOT NULL)
-- * adopted             Blocks it adopted (v_block_adoptions).
--   (NOT NULL)
-- * fork_switches       Times it switched to a fork (v_fork_switches).
--   (NOT NULL)
-- * heap_peak_gib       Its largest heap, GiB (v_heap_per_slot)
--   (NULL: no Resources sample).
-- * rss_peak_gib        Its largest resident set, GiB
--   (NULL: no Resources sample).
-- * mempool_timeouts    Mempool timeouts, hard and soft (v_mempool_timeouts).
--   (NOT NULL)
-- * delay_median_s      Median lateness of entering a slot, seconds after the
--                       slot's start (v_node_forge_loop).
--   (NOT NULL)
-- * delay_max_s         The largest such lateness, seconds.
--   (NOT NULL)
WITH
  forged   AS (SELECT process, count(*) AS n FROM v_block_forged    GROUP BY process),
  adopted  AS (SELECT process, count(*) AS n FROM v_block_adoptions GROUP BY process),
  switches AS (SELECT process, count(*) AS n FROM v_fork_switches   GROUP BY process),
  runs     AS (SELECT process, max(slots) AS longest FROM v_missed_slot_runs GROUP BY process),
  memory   AS (SELECT process, max(heap_bytes) AS heap, max(rss_bytes) AS rss
               FROM v_heap_per_slot GROUP BY process),
  timeouts AS (SELECT process, CAST(sum(timeouts) AS BIGINT) AS n FROM v_mempool_timeouts GROUP BY process)
SELECT
    l.process
  , l.first_slot
  , l.last_slot
  , l.missed                                          AS missed_slots
  , round(100.0 * l.missed / l.slots, 3)              AS missed_pct
  , r.longest                                         AS longest_missed_run
  , coalesce(f.n, 0)                                  AS forged
  , coalesce(a.n, 0)                                  AS adopted
  , coalesce(s.n, 0)                                  AS fork_switches
  , round(m.heap / 2^30, 2)                           AS heap_peak_gib
  , round(m.rss / 2^30, 2)                            AS rss_peak_gib
  , coalesce(t.n, 0)                                  AS mempool_timeouts
  , round(l.delay_median_s, 4)                        AS delay_median_s
  , round(l.delay_max_s, 3)                           AS delay_max_s
FROM v_node_forge_loop l
LEFT JOIN forged   f USING (process)
LEFT JOIN adopted  a USING (process)
LEFT JOIN switches s USING (process)
LEFT JOIN runs     r USING (process)
LEFT JOIN memory   m USING (process)
LEFT JOIN timeouts t USING (process)
ORDER BY l.process;
