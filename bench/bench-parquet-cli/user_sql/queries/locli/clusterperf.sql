-- locli's cluster performance (clusterperf), rebuilt from a PQDIR: the shared
-- base of the section macros (clusterperf_report.sql, clusterperf_snapshots.sql,
-- clusterperf_txprocessing.sql) and of locli_clusterperf_diff
-- (diff_clusterperf.sql). Needs blockprop.sql (locli_filter, locli_cdf).
--
--   bench-parquet query PQDIR --load user_sql/queries/locli
--   SELECT * FROM locli_clusterperf();
--   SELECT * FROM locli_clusterperf(section := 'snapshots');
--
-- PURPOSE: CROSS-CHECKING LOCLI, not replacing it (see blockprop.sql).
-- User content (user_sql/README.md): not reviewed like sql/.
--
-- SECTIONS are locli's (Cardano.Analysis.API.Metrics, PerfSubset), the type
-- locli_cp_section:
--   full          every metric below (the default)
--   report        Resource Usage (clusterperf.report.typ)
--   snapshots     Ledger Snapshots (clusterperf.snapshots.typ)
--   txprocessing  Transaction Processing (clusterperf.txprocessing.typ)
-- A metric belongs to one section, or to none (full only: CentiBlkIO, Threads,
-- cdfSpanLensCpu, cdfHostSlots). A section computes only its own metrics; the
-- slot timeline (locli_cp_slots) is computed for every section, so asking for
-- all metrics at once costs less than asking for each section separately.
-- An unknown section name is an error.
--
-- PARAMETERS: filters (blockprop.sql), of which only first_slot / last_slot
-- apply here; epoch_length, for the epoch-transition attribution of CPU peaks
-- (sections full, report and snapshots), by default the genesis value in
-- meta/run.parquet (run_epoch_length()).
--
-- COVERED (clusterperf.json fields), by section:
--   report        mpResourceCDFs.* (CentiCpu, CentiGC, CentiMut, GcsMajor,
--                 GcsMinor, RSS, Heap, Live, Alloc, NetRd, NetWr, FsRd, FsWr),
--                 cdfStarts, cdfSpanLensCpuEpoch, cdfSpanLensCpuOther
--   snapshots     cdfSnapTaken, cdfSnapDuration, cdfSpanLensCpuSnap,
--                 cdfLiveMajGcDuring, cdfLiveMajGcOutside
--   txprocessing  cdfTxsProcessed, cdfTxsAccepted, cdfTxsRejected,
--                 cdfTxsRejectRatio, cdfMempoolTxs, cdfCpuPerTx, cdfGcPerTx,
--                 cdfMutPerTx
--   (none)        mpResourceCDFs.CentiBlkIO, mpResourceCDFs.Threads,
--                 cdfSpanLensCpu (all peaks), cdfHostSlots
-- Not covered: cdfLeads, cdfDensity, cdfBlockGap and the forge-loop timings
-- (cdfStarted, cdfBlkCtx, cdfLgrState, cdfLgrView, cdfLeading).
--
-- THE SLOT TIMELINE IS LOCLI'S (Cardano.Analysis.MachPerf), per node:
--   * only traces at or after genesis system start count;
--   * it starts at the slot of the node's first JSON line (or of the system
--     start, if later) and runs, without gaps, to the highest slot of a
--     Resources sample (its timestamp's slot) or a StartLeadershipCheck
--     (data.slot); only these two open slots. The newest open slot is the
--     HEAD: mempool, tx, snapshot and BlockContext traces count towards the
--     head at the time they are traced, not towards their own timestamp's slot;
--   * a slot with a Resources sample takes its last one; a slot without takes,
--     from the latest sample before the slot was opened, the CPU shares and
--     gauges, with the counter increments (GCs, Alloc, Net*, Fs*) zero; the first
--     slot has no value unless a sample falls into it;
--   * per sample, against the node's previous one: CPU, GC, mutator and block
--     I/O share = ceiling(ticks spent * 100 / (seconds elapsed * 100)) in single
--     precision, as locli computes it; GCs = increase; Alloc = increase div
--     2^20; Net*, Fs* = increase div 2^10; RSS, Heap, Live = value div 2^20;
--     Threads = value. The node's first sample is the baseline: shares and
--     increases 0. Missing optional Resources fields read 0;
--   * mempool fill level: the last numTxs of Mempool.AddedTx / Mempool.RemoveTxs
--     while the slot was head, else carried from the slot before (0 at the
--     start); the block number (for epoch transitions): likewise, from
--     Forge.Loop.BlockContext's tipBlockNo minus one;
--   * per slot: txs collected (TxSubmission.TxInbound.Collected count), txs
--     accepted (TxSubmission.TxInbound.Processed accepted), snapshots completed
--     (ChainDB.LedgerEvent.Snapshot.TookSnapshot with a duration: enclosedTime
--     edge, or contents in older nodes) and the slot's last snapshot (its
--     completion time and duration).
--
-- PER NODE, over the analysed slots:
--   * txs processed = sum collected, accepted = sum accepted, rejected =
--     processed - min(processed, accepted), reject ratio = rejected / processed
--     (0 without txs); CPU (GC, mutator) ms per tx processed = mean CPU share
--     over slots with a value / 100 * analysed slots * slot length * 1000 /
--     processed (0 without txs);
--   * snapshots taken = sum per slot; durations: each slot's last snapshot;
--     write windows: [completion - duration, completion + 5 s];
--   * live after major GC: Live of each slot whose own sample saw a major GC
--     (GcsMajor increase > 0); "during" when the slot's interval [start, start +
--     slot length] overlaps a write window, else "outside";
--   * CPU peaks: maximal runs of consecutive slots with CPU >= 85% (a slot
--     without a value counts as high), length in slots. Attribution, in order:
--     (1) epoch transition: for some epoch whose first slot is analysed, the
--     first analysed slot of that epoch where the block number changes from the
--     analysed slot before it lies 0-3 slots after the peak's last slot, and the
--     peak has slots in that epoch: the sample is the number of those slots; (2)
--     snapshot: [first slot's start, last slot's start + slot length] overlaps a
--     write window: the peak's length; (3) other: the peak's length.
-- locli's timestamps are rounded to about a quarter of a microsecond in its DBs,
-- so a share whose quotient lies within that of an integer can round the
-- other way: expect rare differences of one percent in a slot.
--
-- CLUSTER FIGURES combine the per-node CDFs as locli does: size = sum,
-- average = mean of the node averages, median = the node medians' middle
-- element, stddev = the largest node stddev (or, when every node has a single
-- sample, the stddev of the node values), min / max = over all nodes, each
-- centile = the mean of the nodes' centile values. For the peak lengths, the
-- snapshot durations and live after major GC, nodes without samples are left
-- out (all but one only when every node has none).

-- The sections; locli_cp_in(section, sections) is true when section is one of
-- sections, and an error for a name that is not a section.
DROP TYPE IF EXISTS locli_cp_section;
CREATE TYPE locli_cp_section AS ENUM ('full', 'report', 'snapshots', 'txprocessing');

CREATE OR REPLACE MACRO locli_cp_in(section, sections) AS
  CASE WHEN list_contains(enum_range(NULL::locli_cp_section)::VARCHAR[], section)
       THEN list_contains(CAST(sections AS locli_cp_section[]), CAST(section AS locli_cp_section))
       ELSE error(printf('locli_clusterperf: unknown section ''%s'' (one of: %s)', section,
                         array_to_string(enum_range(NULL::locli_cp_section), ', '))) END;

-- Per node and slot: what locli's timeline holds.
CREATE OR REPLACE MACRO locli_cp_slots(first_slot := NULL, last_slot := NULL) AS TABLE
WITH
  sys AS (SELECT run_system_start() AS t0),
  res AS MATERIALIZED (
    SELECT process, ts, seq, slot_at(ts) AS slot,
           CAST(data->>'$.CentiCpu' AS UBIGINT)                AS cpu,
           coalesce(CAST(data->>'$.CentiGC'    AS UBIGINT), 0) AS gc,
           coalesce(CAST(data->>'$.CentiMut'   AS UBIGINT), 0) AS mut,
           coalesce(CAST(data->>'$.CentiBlkIO' AS UBIGINT), 0) AS blkio,
           coalesce(CAST(data->>'$.GcsMajor'   AS UBIGINT), 0) AS gcs_major,
           coalesce(CAST(data->>'$.GcsMinor'   AS UBIGINT), 0) AS gcs_minor,
           coalesce(CAST(data->>'$.Alloc'      AS UBIGINT), 0) AS alloc,
           coalesce(CAST(data->>'$.Live'       AS UBIGINT), 0) AS live,
           coalesce(CAST(data->>'$.Heap'       AS UBIGINT), 0) AS heap,
           coalesce(CAST(data->>'$.RSS'        AS UBIGINT), 0) AS rss,
           coalesce(CAST(data->>'$.NetRd'      AS UBIGINT), 0) AS net_rd,
           coalesce(CAST(data->>'$.NetWr'      AS UBIGINT), 0) AS net_wr,
           coalesce(CAST(data->>'$.FsRd'       AS UBIGINT), 0) AS fs_rd,
           coalesce(CAST(data->>'$.FsWr'       AS UBIGINT), 0) AS fs_wr,
           coalesce(CAST(data->>'$.Threads'    AS UBIGINT), 0) AS threads
    FROM raw, sys
    WHERE ns = 'Resources' AND ts >= sys.t0),
  -- What locli's accumulators yield after each sample.
  acc AS MATERIALIZED (
    SELECT process, ts, slot,
           CASE WHEN lag(ts) OVER w IS NULL THEN 0
                ELSE ceil(CAST((cpu   - lag(cpu)   OVER w) * 100 AS FLOAT) / (CAST((epoch_ns(ts) - epoch_ns(lag(ts) OVER w)) / 1e9 AS FLOAT) * CAST(100 AS FLOAT))) END AS cpu,
           CASE WHEN lag(ts) OVER w IS NULL THEN 0
                ELSE ceil(CAST((gc    - lag(gc)    OVER w) * 100 AS FLOAT) / (CAST((epoch_ns(ts) - epoch_ns(lag(ts) OVER w)) / 1e9 AS FLOAT) * CAST(100 AS FLOAT))) END AS gc,
           CASE WHEN lag(ts) OVER w IS NULL THEN 0
                ELSE ceil(CAST((mut   - lag(mut)   OVER w) * 100 AS FLOAT) / (CAST((epoch_ns(ts) - epoch_ns(lag(ts) OVER w)) / 1e9 AS FLOAT) * CAST(100 AS FLOAT))) END AS mut,
           CASE WHEN lag(ts) OVER w IS NULL THEN 0
                ELSE ceil(CAST((blkio - lag(blkio) OVER w) * 100 AS FLOAT) / (CAST((epoch_ns(ts) - epoch_ns(lag(ts) OVER w)) / 1e9 AS FLOAT) * CAST(100 AS FLOAT))) END AS blkio,
           coalesce(gcs_major - lag(gcs_major) OVER w, 0)            AS gcs_major,
           coalesce(gcs_minor - lag(gcs_minor) OVER w, 0)            AS gcs_minor,
           coalesce((alloc  - lag(alloc)  OVER w) // 1048576, 0)     AS alloc,
           coalesce((net_rd - lag(net_rd) OVER w) // 1024, 0)        AS net_rd,
           coalesce((net_wr - lag(net_wr) OVER w) // 1024, 0)        AS net_wr,
           coalesce((fs_rd  - lag(fs_rd)  OVER w) // 1024, 0)        AS fs_rd,
           coalesce((fs_wr  - lag(fs_wr)  OVER w) // 1024, 0)        AS fs_wr,
           rss // 1048576 AS rss, heap // 1048576 AS heap, live // 1048576 AS live, threads
    FROM res
    WINDOW w AS (PARTITION BY process ORDER BY ts, seq)),
  -- Events that open slots: Resources samples (their slot) and leadership checks.
  starts AS MATERIALIZED (
    SELECT process, ts, CAST(data->>'$.slot' AS BIGINT) AS slot
    FROM raw, sys
    WHERE ns = 'Forge.Loop.StartLeadershipCheck' AND ts >= sys.t0),
  ext AS (SELECT process, ts, slot FROM acc UNION ALL SELECT process, ts, slot FROM starts),
  bounds AS MATERIALIZED (
    SELECT p.process,
           slot_at(greatest(p.first_ts, (SELECT t0 FROM sys))) AS first_slot,
           (SELECT max(slot) FROM ext WHERE ext.process = p.process) AS last_slot
    FROM (SELECT process, min(ts) AS first_ts FROM raw WHERE ns IS NOT NULL GROUP BY process) p),
  timeline AS (
    SELECT b.process, s.range AS slot, b.first_slot
    FROM bounds b, range(b.first_slot, b.last_slot + 1) s),
  -- When each slot was opened: the earliest event for it or a later slot.
  opened AS (
    SELECT process, slot,
           min(m) OVER (PARTITION BY process ORDER BY slot DESC ROWS UNBOUNDED PRECEDING) AS t_open
    FROM (SELECT process, slot, min(ts) AS m FROM ext GROUP BY process, slot)),
  tl AS (
    SELECT t.process, t.slot, t.first_slot, o.t_open
    FROM timeline t
    ASOF LEFT JOIN opened o ON o.process = t.process AND o.slot >= t.slot),
  inslot AS (
    SELECT process, slot, arg_max(acc, ts) AS r FROM acc GROUP BY process, slot),
  carried AS (
    SELECT tl.process, tl.slot, a AS r
    FROM tl ASOF LEFT JOIN acc a ON a.process = tl.process AND a.ts < tl.t_open),
  nstarts AS (SELECT process, slot, count(*) AS n FROM starts GROUP BY process, slot),
  -- The head slot over time: the highest slot opened so far.
  head AS MATERIALIZED (
    SELECT e.process, e.ts, greatest(b.first_slot, max(e.mx)) AS head
    FROM (SELECT process, ts, max(max(slot)) OVER (PARTITION BY process ORDER BY ts ROWS UNBOUNDED PRECEDING) AS mx
          FROM ext GROUP BY process, ts) e
    JOIN bounds b USING (process)
    GROUP BY e.process, e.ts, b.first_slot),
  -- Traces counted towards the head slot.
  hev AS MATERIALIZED (
    SELECT process, ts, seq,
      CASE WHEN ns IN ('Mempool.AddedTx', 'Mempool.RemoveTxs')         THEN 'mempool'
           WHEN ns = 'Forge.Loop.BlockContext'                         THEN 'blockno'
           WHEN ns = 'TxSubmission.TxInbound.Collected'                THEN 'collected'
           WHEN ns = 'TxSubmission.TxInbound.Processed'                THEN 'accepted'
           ELSE 'snapshot' END AS kind,
      CASE WHEN ns IN ('Mempool.AddedTx', 'Mempool.RemoveTxs')         THEN CAST(data->>'$.mempoolSize.numTxs' AS DOUBLE)
           WHEN ns = 'Forge.Loop.BlockContext'                         THEN CAST(data->>'$.tipBlockNo' AS DOUBLE) - 1
           WHEN ns = 'TxSubmission.TxInbound.Collected'                THEN CAST(data->>'$.count' AS DOUBLE)
           WHEN ns = 'TxSubmission.TxInbound.Processed'                THEN CAST(data->>'$.accepted' AS DOUBLE)
           ELSE CASE WHEN json_type(data, '$.enclosedTime.edge') IN ('UBIGINT', 'BIGINT', 'DOUBLE')
                     THEN CAST(data->>'$.enclosedTime.edge' AS DOUBLE)
                     WHEN json_type(data, '$.enclosedTime.contents') IN ('UBIGINT', 'BIGINT', 'DOUBLE')
                     THEN CAST(data->>'$.enclosedTime.contents' AS DOUBLE) END
      END AS v
    FROM raw, sys
    WHERE ts >= sys.t0
      AND ns IN ( 'Mempool.AddedTx', 'Mempool.RemoveTxs', 'Forge.Loop.BlockContext'
                , 'TxSubmission.TxInbound.Collected', 'TxSubmission.TxInbound.Processed'
                , 'ChainDB.LedgerEvent.Snapshot.TookSnapshot')),
  hevh AS MATERIALIZED (
    SELECT e.*, coalesce(h.head, b.first_slot) AS head
    FROM hev e
    JOIN bounds b USING (process)
    ASOF LEFT JOIN head h ON h.process = e.process AND h.ts <= e.ts
    WHERE e.v IS NOT NULL),
  perhead AS (
    SELECT process, head AS slot,
           arg_max(v, (ts, seq)) FILTER (WHERE kind = 'mempool')        AS mempool_txs,
           arg_max(v, (ts, seq)) FILTER (WHERE kind = 'blockno')        AS block_no,
           coalesce(sum(v) FILTER (WHERE kind = 'collected'), 0)        AS txs_collected,
           coalesce(sum(v) FILTER (WHERE kind = 'accepted'), 0)         AS txs_accepted,
           count(*) FILTER (WHERE kind = 'snapshot')                    AS snapshots,
           arg_max(ts, (ts, seq)) FILTER (WHERE kind = 'snapshot')      AS snap_done,
           arg_max(v, (ts, seq)) FILTER (WHERE kind = 'snapshot')       AS snap_duration_s
    FROM hevh GROUP BY process, head),
  -- Carried values: the last mempool reading and block number at or before the slot.
  lastmem AS (SELECT process, slot, mempool_txs FROM perhead WHERE mempool_txs IS NOT NULL),
  lastblk AS (SELECT process, slot, block_no FROM perhead WHERE block_no IS NOT NULL)
SELECT
    tl.process, tl.slot
  , coalesce(ns.n, 0) AS starts
  , CASE
      WHEN i.r IS NOT NULL THEN
        {'cpu': i.r.cpu, 'gc': i.r.gc, 'mut': i.r.mut, 'blkio': i.r.blkio,
         'gcs_major': i.r.gcs_major, 'gcs_minor': i.r.gcs_minor, 'alloc': i.r.alloc,
         'net_rd': i.r.net_rd, 'net_wr': i.r.net_wr, 'fs_rd': i.r.fs_rd, 'fs_wr': i.r.fs_wr,
         'rss': i.r.rss, 'heap': i.r.heap, 'live': i.r.live, 'threads': i.r.threads}
      WHEN tl.slot = tl.first_slot THEN NULL
      ELSE
        {'cpu': coalesce(c.r.cpu, 0), 'gc': coalesce(c.r.gc, 0), 'mut': coalesce(c.r.mut, 0),
         'blkio': coalesce(c.r.blkio, 0), 'gcs_major': 0, 'gcs_minor': 0, 'alloc': 0,
         'net_rd': 0, 'net_wr': 0, 'fs_rd': 0, 'fs_wr': 0,
         'rss': coalesce(c.r.rss, 0), 'heap': coalesce(c.r.heap, 0), 'live': coalesce(c.r.live, 0),
         'threads': coalesce(c.r.threads, 0)}
    END AS r
  , coalesce(lm.mempool_txs, 0)       AS mempool_txs
  , coalesce(lb.block_no, 0)          AS block_no
  , coalesce(ph.txs_collected, 0)     AS txs_collected
  , coalesce(ph.txs_accepted, 0)      AS txs_accepted
  , coalesce(ph.snapshots, 0)         AS snapshots
  , ph.snap_done
  , ph.snap_duration_s
FROM tl
LEFT JOIN inslot  i  ON i.process  = tl.process AND i.slot  = tl.slot
LEFT JOIN carried c  ON c.process  = tl.process AND c.slot  = tl.slot
LEFT JOIN nstarts ns ON ns.process = tl.process AND ns.slot = tl.slot
LEFT JOIN perhead ph ON ph.process = tl.process AND ph.slot = tl.slot
ASOF LEFT JOIN lastmem lm ON lm.process = tl.process AND lm.slot <= tl.slot
ASOF LEFT JOIN lastblk lb ON lb.process = tl.process AND lb.slot <= tl.slot
WHERE (first_slot IS NULL OR tl.slot >= first_slot)
  AND (last_slot  IS NULL OR tl.slot <= last_slot);

-- Per node: locli's per-node CDFs (MachPerf), one row per (process, metric), for
-- the metrics of one section (default: all).
CREATE OR REPLACE MACRO locli_clusterperf_hosts(filters := NULL, epoch_length := NULL, section := 'full') AS TABLE
WITH
  s AS MATERIALIZED (FROM locli_cp_slots(first_slot := coalesce(filters, locli_filter()).first_slot,
                                         last_slot  := coalesce(filters, locli_filter()).last_slot)),
  el AS (SELECT CASE WHEN locli_cp_in(section, ['full', 'report', 'snapshots'])
                     THEN CAST(coalesce(epoch_length, run_epoch_length()) AS BIGINT)
                END AS n,
                run_slot_length_s() AS slot_len),
  -- Snapshot write windows per node.
  win AS (
    SELECT process,
           snap_done - to_microseconds(CAST(snap_duration_s * 1e6 AS BIGINT)) AS w_from,
           snap_done + INTERVAL 5 SECOND                                       AS w_to,
           snap_duration_s
    FROM s WHERE snap_done IS NOT NULL),
  -- Slots whose own sample saw a major GC, during or outside a write window.
  majgc AS (
    SELECT s.process, s.r.live AS live,
           EXISTS (SELECT 1 FROM win w, el
                   WHERE w.process = s.process
                     AND w.w_from <= slot_start(s.slot) + to_microseconds(CAST(el.slot_len * 1e6 AS BIGINT))
                     AND slot_start(s.slot) <= w.w_to) AS during
    FROM s WHERE s.r IS NOT NULL AND s.r.gcs_major > 0),
  -- CPU peaks: runs of consecutive high slots.
  peaks AS (
    SELECT process, min(slot) AS lo, max(slot) AS hi, count(*) AS len
    FROM (SELECT process, slot, slot - row_number() OVER (PARTITION BY process ORDER BY slot) AS grp
          FROM s WHERE r IS NULL OR r.cpu >= 85)
    GROUP BY process, grp),
  -- Per epoch whose first slot is analysed: the first analysed slot where the
  -- block number changes.
  epochs AS (SELECT DISTINCT s.process, s.slot // el.n AS epoch FROM s, el WHERE s.slot % el.n = 0),
  adoptions AS (
    SELECT c.process, c.slot // el.n AS epoch, min(c.slot) AS slot
    FROM (SELECT process, slot, block_no, lag(block_no) OVER (PARTITION BY process ORDER BY slot) AS prev FROM s) c
    CROSS JOIN el
    SEMI JOIN epochs e ON e.process = c.process AND e.epoch = c.slot // el.n
    WHERE c.prev IS NOT NULL AND c.block_no <> c.prev
    GROUP BY c.process, c.slot // el.n),
  -- A peak's slots in the epoch of an adoption 0-3 slots after it; the first
  -- such epoch decides.
  epochpeak AS (
    SELECT p.process, p.lo, arg_min(n_in, a.epoch) AS epoch_n
    FROM peaks p
    JOIN adoptions a ON a.process = p.process AND p.hi <= a.slot AND a.slot - p.hi <= 3
    CROSS JOIN el
    CROSS JOIN LATERAL (SELECT greatest(0, least(p.hi, (a.epoch + 1) * el.n - 1)
                                           - greatest(p.lo, a.epoch * el.n) + 1) AS n_in)
    WHERE n_in > 0
    GROUP BY p.process, p.lo),
  classified AS (
    SELECT p.process, p.lo, p.hi, p.len, e.epoch_n,
           EXISTS (SELECT 1 FROM win w, el
                   WHERE w.process = p.process
                     AND w.w_from <= slot_start(p.hi) + to_microseconds(CAST(el.slot_len * 1e6 AS BIGINT))
                     AND slot_start(p.lo) <= w.w_to) AS snap
    FROM peaks p LEFT JOIN epochpeak e USING (process, lo)),
  tot AS (
    SELECT process, count(*) AS slots,
           sum(txs_collected) AS processed, sum(txs_accepted) AS accepted, sum(snapshots) AS taken,
           coalesce(avg(r.cpu) FILTER (WHERE r IS NOT NULL), 0) AS cpu_share,
           coalesce(avg(r.gc)  FILTER (WHERE r IS NOT NULL), 0) AS gc_share,
           coalesce(avg(r.mut) FILTER (WHERE r IS NOT NULL), 0) AS mut_share
    FROM s GROUP BY process),
  -- Peak lengths by cause and live after major GC, per node (empty lists for none).
  pk AS (
    SELECT p.process,
           coalesce(c.lens,  []) AS lens,  coalesce(c.epoch, [])  AS epoch,
           coalesce(c.snap,  []) AS snap,  coalesce(c.other, [])  AS other,
           coalesce(m.during, []) AS during, coalesce(m.outside, []) AS outside
    FROM (SELECT DISTINCT process FROM s) p
    LEFT JOIN (SELECT process,
                      list(len::DOUBLE)                                                AS lens,
                      list(epoch_n::DOUBLE) FILTER (WHERE epoch_n IS NOT NULL)         AS epoch,
                      list(len::DOUBLE) FILTER (WHERE epoch_n IS NULL AND snap)        AS snap,
                      list(len::DOUBLE) FILTER (WHERE epoch_n IS NULL AND NOT snap)    AS other
               FROM classified GROUP BY process) c USING (process)
    LEFT JOIN (SELECT process,
                      list(live::DOUBLE) FILTER (WHERE during)                         AS during,
                      list(live::DOUBLE) FILTER (WHERE NOT during)                     AS outside
               FROM majgc GROUP BY process) m USING (process)),
  -- One branch per section and source; a branch whose section is not asked
  -- for is dropped before evaluation, with what only it reads.
  per AS (
      SELECT process, 'report' AS sec, unnest([
          {'metric': 'mpResourceCDFs.CentiCpu', 'cdf': locli_cdf(list(r.cpu::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'mpResourceCDFs.CentiGC', 'cdf': locli_cdf(list(r.gc::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'mpResourceCDFs.CentiMut', 'cdf': locli_cdf(list(r.mut::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'mpResourceCDFs.GcsMajor', 'cdf': locli_cdf(list(r.gcs_major::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'mpResourceCDFs.GcsMinor', 'cdf': locli_cdf(list(r.gcs_minor::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'mpResourceCDFs.Alloc', 'cdf': locli_cdf(list(r.alloc::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'mpResourceCDFs.NetRd', 'cdf': locli_cdf(list(r.net_rd::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'mpResourceCDFs.NetWr', 'cdf': locli_cdf(list(r.net_wr::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'mpResourceCDFs.FsRd', 'cdf': locli_cdf(list(r.fs_rd::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'mpResourceCDFs.FsWr', 'cdf': locli_cdf(list(r.fs_wr::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'mpResourceCDFs.RSS', 'cdf': locli_cdf(list(r.rss::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'mpResourceCDFs.Heap', 'cdf': locli_cdf(list(r.heap::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'mpResourceCDFs.Live', 'cdf': locli_cdf(list(r.live::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'cdfStarts', 'cdf': locli_cdf(list(starts::DOUBLE))}
        ]) AS x
      FROM s WHERE locli_cp_in(section, ['full', 'report']) GROUP BY process
    UNION ALL
      SELECT process, 'report', unnest([
          {'metric': 'cdfSpanLensCpuEpoch', 'cdf': locli_cdf(epoch)},
          {'metric': 'cdfSpanLensCpuOther', 'cdf': locli_cdf(other)}
        ])
      FROM pk WHERE locli_cp_in(section, ['full', 'report'])
    UNION ALL
      SELECT process, 'snapshots', unnest([
          {'metric': 'cdfSnapDuration', 'cdf': locli_cdf(coalesce(list(snap_duration_s) FILTER (WHERE snap_duration_s IS NOT NULL), []))}
        ])
      FROM s WHERE locli_cp_in(section, ['full', 'snapshots']) GROUP BY process
    UNION ALL
      SELECT process, 'snapshots', unnest([
          {'metric': 'cdfSnapTaken', 'cdf': locli_cdf([taken::DOUBLE])}
        ])
      FROM tot WHERE locli_cp_in(section, ['full', 'snapshots'])
    UNION ALL
      SELECT process, 'snapshots', unnest([
          {'metric': 'cdfSpanLensCpuSnap',  'cdf': locli_cdf(snap)},
          {'metric': 'cdfLiveMajGcDuring',  'cdf': locli_cdf(during)},
          {'metric': 'cdfLiveMajGcOutside', 'cdf': locli_cdf(outside)}
        ])
      FROM pk WHERE locli_cp_in(section, ['full', 'snapshots'])
    UNION ALL
      SELECT process, 'txprocessing', unnest([
          {'metric': 'cdfMempoolTxs', 'cdf': locli_cdf(list(mempool_txs::DOUBLE))}
        ])
      FROM s WHERE locli_cp_in(section, ['full', 'txprocessing']) GROUP BY process
    UNION ALL
      SELECT process, 'txprocessing', unnest([
          {'metric': 'cdfTxsProcessed',   'cdf': locli_cdf([processed])},
          {'metric': 'cdfTxsAccepted',    'cdf': locli_cdf([accepted])},
          {'metric': 'cdfTxsRejected',    'cdf': locli_cdf([processed - least(processed, accepted)])},
          {'metric': 'cdfTxsRejectRatio', 'cdf': locli_cdf([CASE WHEN processed = 0 THEN 0
                                                                 ELSE (processed - least(processed, accepted)) / processed END])},
          {'metric': 'cdfCpuPerTx',       'cdf': locli_cdf([CASE WHEN processed = 0 THEN 0
                                                                 ELSE cpu_share / 100 * slots * (SELECT slot_len FROM el) * 1000 / processed END])},
          {'metric': 'cdfGcPerTx',        'cdf': locli_cdf([CASE WHEN processed = 0 THEN 0
                                                                 ELSE gc_share  / 100 * slots * (SELECT slot_len FROM el) * 1000 / processed END])},
          {'metric': 'cdfMutPerTx',       'cdf': locli_cdf([CASE WHEN processed = 0 THEN 0
                                                                 ELSE mut_share / 100 * slots * (SELECT slot_len FROM el) * 1000 / processed END])}
        ])
      FROM tot WHERE locli_cp_in(section, ['full', 'txprocessing'])
    UNION ALL
      SELECT process, 'full', unnest([
          {'metric': 'mpResourceCDFs.CentiBlkIO', 'cdf': locli_cdf(list(r.blkio::DOUBLE) FILTER (WHERE r IS NOT NULL))},
          {'metric': 'mpResourceCDFs.Threads', 'cdf': locli_cdf(list(r.threads::DOUBLE) FILTER (WHERE r IS NOT NULL))}
        ])
      FROM s WHERE locli_cp_in(section, ['full']) GROUP BY process
    UNION ALL
      SELECT process, 'full', unnest([
          {'metric': 'cdfHostSlots', 'cdf': locli_cdf([slots::DOUBLE])}
        ])
      FROM tot WHERE locli_cp_in(section, ['full'])
    UNION ALL
      SELECT process, 'full', unnest([
          {'metric': 'cdfSpanLensCpu', 'cdf': locli_cdf(lens)}
        ])
      FROM pk WHERE locli_cp_in(section, ['full']))
SELECT process, sec AS section, x.metric AS metric, x.cdf.size AS size, x.cdf.average AS average,
       x.cdf.median AS median, x.cdf.stddev AS stddev, x.cdf.min AS min, x.cdf.max AS max,
       x.cdf.centiles AS centiles
FROM per;

-- The cluster: locli's ClusterPerf figures (see the header), one row per metric,
-- for the metrics of one section (default: all).
CREATE OR REPLACE MACRO locli_clusterperf(filters := NULL, epoch_length := NULL, section := 'full') AS TABLE
WITH
  h0 AS MATERIALIZED (FROM locli_clusterperf_hosts(filters := filters, epoch_length := epoch_length,
                                                   section := section)),
  -- Metrics whose empty per-node CDFs are left out (locli's nonEmptyCDFs).
  ne AS (SELECT unnest(['cdfSpanLensCpu', 'cdfSpanLensCpuEpoch', 'cdfSpanLensCpuSnap', 'cdfSpanLensCpuOther',
                        'cdfSnapDuration', 'cdfLiveMajGcDuring', 'cdfLiveMajGcOutside']) AS metric),
  hh AS (
    SELECT h0.* FROM h0
    WHERE h0.metric NOT IN (SELECT metric FROM ne)
       OR h0.size > 0
       OR (NOT EXISTS (SELECT 1 FROM h0 x WHERE x.metric = h0.metric AND x.size > 0)
           AND h0.process = (SELECT min(process) FROM h0 x WHERE x.metric = h0.metric))),
  cen AS (
    SELECT metric, list({'centile': centile, 'value': value} ORDER BY centile) AS centiles
    FROM (SELECT metric, c.centile, avg(c.value) AS value
          FROM (SELECT metric, unnest(centiles) AS c FROM hh) GROUP BY metric, c.centile)
    GROUP BY metric)
SELECT
    hh.section                                                        AS section
  , metric
  , sum(size)                                                         AS size
  , avg(average)                                                      AS average
  , list_sort(list(median))[count(*) // 2 + 1]                        AS median
  , CASE WHEN bool_and(size = 1) THEN coalesce(stddev_samp(average), 0) ELSE max(stddev) END AS stddev
  , min(min)                                                          AS min
  , max(max)                                                          AS max
  , any_value(cen.centiles)                                           AS centiles
FROM hh JOIN cen USING (metric)
GROUP BY hh.section, metric;
