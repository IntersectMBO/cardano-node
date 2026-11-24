-- "Mean egress speed, txs/s" by node over time, per minute. Each RemoveTxs
-- carries the list of txs that left the mempool (into a block, or no longer
-- valid); SwitchedToCertifyingView (Leios) lists none, so its removals are the
-- drop of mempoolSize.numTxs against the node's previous mempool event. Summed
-- per minute over 60 is the rate. Minute 0 is the cluster's first mempool event.
-- User content (user_sql/README.md): not reviewed like sql/.
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
SELECT process, (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute,
       round(sum(CASE WHEN ns = 'Mempool.RemoveTxs' THEN listed ELSE -dtxs END) / 60.0, 2) AS txs_per_s
FROM deltas
WHERE ns = 'Mempool.RemoveTxs'
   OR (ns = 'Mempool.SwitchedToCertifyingView' AND dtxs IS NOT NULL)
GROUP BY ALL
ORDER BY minute, process;
