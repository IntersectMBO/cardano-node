-- Mempool totals per node: inclusions, removals, rejections, and the pool's
-- peak content. Not a series: one row per node. On Leios nodes
-- SwitchedToCertifyingView removals (no tx list; the numTxs drop) are in their
-- own column. User content (user_sql/README.md): not reviewed like sql/.
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
SELECT process,
       count(*) FILTER (WHERE ns = 'Mempool.AddedTx')                    AS txs_added,
       coalesce(sum(listed), 0)                                          AS txs_removed,
       coalesce(-sum(dtxs) FILTER (WHERE ns = 'Mempool.SwitchedToCertifyingView'), 0)
                                                                         AS txs_removed_certifying_view,
       count(*) FILTER (WHERE ns = 'Mempool.RejectedTx')                 AS txs_rejected,
       max(txs)   FILTER (WHERE ns = 'Mempool.AddedTx')                  AS peak_txs,
       round(max(bytes) FILTER (WHERE ns = 'Mempool.AddedTx') / 1048576.0, 2) AS peak_mib
FROM deltas
GROUP BY ALL
ORDER BY process;
