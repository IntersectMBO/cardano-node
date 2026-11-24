-- "Mean ingress speed, txs/s" by node over time, per minute: AddedTx events are
-- one tx each, so the count per minute over 60 is the rate. Minute 0 is the
-- cluster's first mempool event. User content (user_sql/README.md): not reviewed like sql/.
WITH
  t0 AS (SELECT min(ts) AS t0 FROM raw
         WHERE ns IN ('Mempool.AddedTx', 'Mempool.RemoveTxs', 'Mempool.RejectedTx',
                      'Mempool.SwitchedToCertifyingView'))
SELECT process, (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute, round(count(*) / 60.0, 2) AS txs_per_s
FROM raw
WHERE ns IN ('Mempool.AddedTx')
GROUP BY ALL
ORDER BY minute, process;
