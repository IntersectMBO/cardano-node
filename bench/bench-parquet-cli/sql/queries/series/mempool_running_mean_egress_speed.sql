-- "mempool_running_mean_egress_speed": the total bytes that have left each
-- node's mempool so far, divided by the time elapsed since that node's first
-- mempool event. A running mean, not a rate. A series: x = minute,
-- key = process, y = bytes_per_s (and its certifying-view part).
--
-- WHERE THE BYTES COME FROM. Every mempool trace carries
-- data.mempoolSize.bytes, the pool's size AFTER the operation, so the DROP from
-- the previous mempool event's value is exactly the bytes that left. Two events
-- remove txs: Mempool.RemoveTxs (an adopted block contained them, or they no
-- longer validate) and, on Leios nodes, Mempool.SwitchedToCertifyingView (the
-- txs of a certified endorser block). Both count as egress; the second is also
-- reported on its own (certifying_view_bytes_per_s): on Leios nodes it can
-- carry most of the egress, and on Praos nodes it is absent.
-- IT IS EGRESS FROM THE MEMPOOL, NOT FROM THE CHAIN: a local fact about one
-- pool; summing it across nodes counts the same transactions many times over.
-- THE PREVIOUS EVENT IS OF ANY KIND, in log order (seq): a removal that follows
-- adds is measured against the post-add size. THE DROP IS REPORTED POSITIVE.
-- NO WINDOW, NO WEIGHTING, NO SMOOTHING: cumulative bytes divided by the span
-- from that node's first mempool event to the end of the minute.
-- MINUTE 0 IS THE FIRST MEMPOOL EVENT OF THE RUN, the same anchor in every
-- mempool_* series here.
--
-- Fields:
-- --------
-- * process                      The node (series key).
-- * minute                       Whole minutes since the anchor (x).
-- * bytes_per_s                  Bytes removed from that pool (both events)
--                                from its first event through the end of this
--                                minute, divided by that span in seconds (y).
-- * certifying_view_bytes_per_s  The part removed by
--                                Mempool.SwitchedToCertifyingView (0 on Praos).
WITH
  readings AS (
    SELECT process, ts, seq, ns, mempool_bytes(data) AS bytes
    FROM raw
    WHERE ns IN ('Mempool.AddedTx', 'Mempool.RemoveTxs', 'Mempool.RejectedTx',
                 'Mempool.SwitchedToCertifyingView')),
  t0 AS (SELECT min(ts) AS t0 FROM readings),
  deltas AS MATERIALIZED (
    SELECT process, ts, ns,
           CAST(bytes AS BIGINT) - lag(CAST(bytes AS BIGINT)) OVER (PARTITION BY process ORDER BY seq) AS delta
    FROM readings),
  started AS (SELECT process, min(ts) AS first_ts FROM readings GROUP BY process),
  per_minute AS (
    SELECT process, (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute,
           -sum(delta) AS bytes,
           -coalesce(sum(delta) FILTER (WHERE ns = 'Mempool.SwitchedToCertifyingView'), 0) AS cv_bytes
    FROM deltas
    WHERE ns IN ('Mempool.RemoveTxs', 'Mempool.SwitchedToCertifyingView') AND delta IS NOT NULL
    GROUP BY ALL),
  cumulative AS (
    SELECT process, minute,
           sum(bytes)    OVER (PARTITION BY process ORDER BY minute) AS bytes_total,
           sum(cv_bytes) OVER (PARTITION BY process ORDER BY minute) AS cv_total
    FROM per_minute),
  span AS (
    SELECT c.*, ((c.minute + 1) * 60000000000.0
                 - (epoch_ns(s.first_ts) - epoch_ns((SELECT t0 FROM t0)))) / 1e9 AS span_s
    FROM cumulative c JOIN started s USING (process))
SELECT process, minute,
       round(bytes_total / span_s, 1) AS bytes_per_s,
       round(cv_total    / span_s, 1) AS certifying_view_bytes_per_s
FROM span
ORDER BY minute, process;
