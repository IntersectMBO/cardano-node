-- Mempool flow per minute, cluster-wide: inclusions are AddedTx events (one tx
-- each), removals the length of the txs list each RemoveTxs carries (txs leave
-- when a block takes them or they stop validating), rejections RejectedTx
-- events. On Leios nodes SwitchedToCertifyingView removes txs too, without a
-- list: counted from the numTxs drop, in its own column. Every column sums
-- events over all nodes, so one tx counts once per node that handled it.
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
SELECT (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute,
       count(*) FILTER (WHERE ns = 'Mempool.AddedTx')                    AS txs_added,
       coalesce(sum(listed), 0)                                          AS txs_removed,
       coalesce(-sum(dtxs) FILTER (WHERE ns = 'Mempool.SwitchedToCertifyingView'), 0)
                                                                         AS txs_removed_certifying_view,
       count(*) FILTER (WHERE ns = 'Mempool.RejectedTx')                 AS txs_rejected
FROM deltas
GROUP BY ALL
ORDER BY minute;
