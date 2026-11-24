-- "mempool_size_bytes": how full each node's mempool was, in bytes, one row per
-- (node, minute). A series: x = minute, key = process, y = bytes.
--
-- THE POOL REPORTS ITS OWN SIZE. Every mempool trace that changes or states it
-- -- Mempool.AddedTx, Mempool.RejectedTx, Mempool.RemoveTxs and, on Leios
-- nodes, Mempool.SwitchedToCertifyingView -- carries data.mempoolSize, the
-- pool's size AFTER that operation. So this is not accumulated from deltas and
-- cannot drift: each row is a reading the node itself published.
-- REJECTIONS ARE READINGS TOO: a rejected transaction does not move the size,
-- but it still states it, and keeps a quiet node's line alive.
-- THE VALUE IS THE LAST READING OF THE MINUTE (by log order, seq), not a mean:
-- this is a gauge. A minute with no mempool event has no row and the plotted
-- line holds at its last value.
-- MINUTE 0 IS THE FIRST MEMPOOL EVENT OF THE RUN, the same anchor in every
-- mempool_* series here, so these series share one axis.
--
-- Fields:
-- --------
-- * process   The node (series key).
-- * minute    Whole minutes since the anchor (x).
-- * bytes     The pool's size in bytes at that node's last mempool
--             event in this minute (y).
WITH
  readings AS (
    SELECT process, ts, seq, mempool_bytes(data) AS bytes
    FROM raw
    WHERE ns IN ('Mempool.AddedTx', 'Mempool.RemoveTxs', 'Mempool.RejectedTx',
                 'Mempool.SwitchedToCertifyingView')),
  t0 AS (SELECT min(ts) AS t0 FROM readings)
SELECT process,
       (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute,
       arg_max(bytes, seq) AS bytes
FROM readings
GROUP BY ALL
ORDER BY minute, process;
