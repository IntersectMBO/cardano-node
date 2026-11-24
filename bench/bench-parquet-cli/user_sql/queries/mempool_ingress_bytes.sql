-- "Mean ingress speed, bytes/s" by node over time, per minute: a rate, not the
-- running mean of the package series mempool_running_mean_ingress_speed (a
-- per-minute rate swings with every idle minute; the running mean smooths
-- that). User content (user_sql/README.md): not reviewed like sql/.
--
-- No trace carries a tx's bytes, but the mempoolSize.bytes gauge moves by
-- exactly the touched bytes at each event: AddedTx raises it by the tx's size; RejectedTx leaves it. The
-- per-event delta in log order (seq) per node, summed per minute over 60, is the
-- ingress byte flow. Minute 0 is the cluster's first mempool event.
-- On Leios nodes Mempool.SwitchedToCertifyingView also removes txs (those of a
-- certified endorser block) and states the pool's size; it is read as a
-- reading, so the next AddedTx is measured against the post-removal size, and
-- is not counted as ingress.
WITH
  readings AS (
    SELECT process, ts, seq, ns,
           CAST(mempool_bytes(data) AS BIGINT) AS bytes,
           CAST(mempool_txs(data)   AS BIGINT) AS txs,
           CASE WHEN ns = 'Mempool.RemoveTxs' THEN json_array_length(data->'$.txs') END AS listed
    FROM raw
    WHERE ns IN ('Mempool.AddedTx', 'Mempool.RemoveTxs', 'Mempool.RejectedTx',
                 'Mempool.SwitchedToCertifyingView')),
  t0 AS (SELECT min(ts) AS t0 FROM readings),
  deltas AS (
    SELECT process, ts, ns, listed, bytes, txs,
           bytes - lag(bytes) OVER (PARTITION BY process ORDER BY seq) AS dbytes,
           txs   - lag(txs)   OVER (PARTITION BY process ORDER BY seq) AS dtxs
    FROM readings)
SELECT process, (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute, round(sum(dbytes) / 60.0, 1) AS bytes_per_s
FROM deltas
WHERE ns IN ('Mempool.AddedTx') AND dbytes IS NOT NULL
GROUP BY ALL
ORDER BY minute, process;
