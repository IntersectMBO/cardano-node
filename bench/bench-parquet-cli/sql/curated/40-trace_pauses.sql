-- Curated query: pauses in a node's trace output, with GC attribution.
--
--   SELECT * FROM trace_pauses();                      -- gaps over 2 s
--   SELECT * FROM trace_pauses(threshold_s := 5);
--
-- A pause is a gap of more than threshold_s between consecutive JSON lines of one
-- node's stdout (all namespaces, ordered by time, then line). A stopping-the-world
-- GC causes such gaps, but so do other stalls (blocked logging, disk, an overloaded
-- or lagging node), so a gap alone is evidence, not proof (LOCLI_QUICK.md, item 5).
-- Attribution compares the node's Resources samples around the gap: the last one
-- at or before its start and the first one at or after its end.
--
-- Columns:
--   process, gap_start, gap_end    the node and the two lines around the gap
--   duration_s                     the gap
--   major_gc                       GcsMajor rose between the two samples: a major GC
--                                  happened in that window (NULL: no sample on one side)
--   gc_cpu_s                       CentiGC difference between the samples: GC CPU time
--                                  in the window, summed over GC threads (with parallel
--                                  GC it can exceed the window's wall time)
--   sample_window_s                time between the two samples (>= duration_s); the
--                                  wider it is, the weaker the attribution
--
-- Checked on the Leios cluster run (51 nodes): 2,888 gaps over 2 s, 2,829 with a major
-- GC in a median window of 2.59 s (median gap 2.13 s, median GC CPU 4.03 s, i.e. about
-- two GC threads busy for the whole gap). The other 59 were one node lagging in its
-- last minutes (Resources samples missing for up to 53 s, no GC): not GC pauses.
CREATE OR REPLACE MACRO trace_pauses(threshold_s := 2) AS TABLE
WITH
  t AS (
    SELECT process, ts, lag(ts) OVER (PARTITION BY process ORDER BY ts, seq) AS prev
    FROM raw
    WHERE ns IS NOT NULL),
  gaps AS (
    SELECT process, prev AS gap_start, ts AS gap_end,
           (epoch_ns(ts) - epoch_ns(prev)) / 1e9 AS duration_s
    FROM t
    WHERE prev IS NOT NULL
      AND epoch_ns(ts) - epoch_ns(prev) > CAST(threshold_s * 1e9 AS BIGINT)),
  res AS (
    SELECT process, ts, res_gcs_major(data) AS gcs_major, res_gc(data) AS centi_gc
    FROM raw
    WHERE ns = 'Resources')
SELECT
    g.process
  , g.gap_start
  , g.gap_end
  , g.duration_s
  , (a.gcs_major > b.gcs_major)                       AS major_gc
  , (a.centi_gc - b.centi_gc) / 100.0                  AS gc_cpu_s
  , (epoch_ns(a.ts) - epoch_ns(b.ts)) / 1e9            AS sample_window_s
FROM gaps g
ASOF LEFT JOIN res b ON g.process = b.process AND g.gap_start >= b.ts
ASOF LEFT JOIN res a ON g.process = a.process AND g.gap_end   <= a.ts;
