-- "mempool_running_mean_ingress_speed": the total bytes that have entered each
-- node's mempool so far, divided by the time elapsed since that node's first
-- mempool event. A running mean, not a rate. A series: x = minute,
-- key = process, y = bytes_per_s.
--
-- WHERE THE BYTES COME FROM. Every Mempool.AddedTx carries
-- data.mempoolSize.bytes, the pool's size AFTER the operation, so the RISE over
-- the previous mempool event's value is exactly the accepted transaction's
-- bytes entering the pool. Nothing is joined, no size is looked up.
-- THE PREVIOUS EVENT IS OF ANY KIND, in log order (seq): an add that follows a
-- removal must be measured against the post-removal size, or the rise would
-- absorb the drop. On Leios nodes that includes Mempool.SwitchedToCertifyingView,
-- which removes the txs of a certified endorser block and can carry most of
-- the egress there; leaving it out would net those drops against the next
-- add.
-- REJECTED DELIVERIES ARE EXCLUDED BY CONSTRUCTION: a rejection reports the
-- same size as the event before it (delta 0) and is not an add anyway.
-- NO WINDOW, NO WEIGHTING, NO SMOOTHING: each point is cumulative bytes divided
-- by the span from that node's first mempool event to the end of the minute.
-- MINUTE 0 IS THE FIRST MEMPOOL EVENT OF THE RUN, so minute numbers align with
-- the other mempool_* series even though each node's denominator starts at its
-- own first event.
--
-- Fields:
-- --------
-- * process       The node (series key).
-- * minute        Whole minutes since the anchor (x).
-- * bytes_per_s   Bytes admitted to that pool from its first event through the
--                 end of this minute, divided by that span in seconds (y).
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
           sum(delta) AS bytes
    FROM deltas
    WHERE ns = 'Mempool.AddedTx' AND delta IS NOT NULL
    GROUP BY ALL),
  cumulative AS (
    SELECT process, minute,
           sum(bytes) OVER (PARTITION BY process ORDER BY minute) AS bytes_total
    FROM per_minute)
SELECT c.process, c.minute,
       round(c.bytes_total
             / (((c.minute + 1) * 60000000000.0
                 - (epoch_ns(s.first_ts) - epoch_ns((SELECT t0 FROM t0)))) / 1e9), 1) AS bytes_per_s
FROM cumulative c JOIN started s USING (process)
ORDER BY minute, process;
