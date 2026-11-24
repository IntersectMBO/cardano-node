-- "tx_centrifuge_delivered_txs": the CUMULATIVE number of transactions the
-- tx-centrifuge has handed to each target, over time. A series: x = minute,
-- key = target, y = txs_total. Generator family: experimental.
--
-- The count twin of tx_centrifuge_delivered_bytes: one per {id, size} entry of
-- each TxCentrifuge.TxSubmission.ReplyTxs (v_gen_tx_sent). Under mixed
-- transaction sizes the two are not proportional, so neither replaces the other.
-- ONE ROW PER (target, minute) IN WHICH THAT TARGET WAS SENT ANYTHING: a step
-- series; the counter never decreases.
-- MINUTE 0 IS THE GENERATOR'S FIRST TxSubmission EVENT, the same anchor in every
-- tx_centrifuge_* series here.
--
-- Fields:
-- --------
-- * target     The node the transactions were sent to (series key).
-- * minute     Whole minutes since the anchor (x).
-- * txs_total  Transactions delivered to that target from the anchor through the
--              end of this minute (y).
WITH
  t0 AS (SELECT min(ts) AS t0 FROM raw_generator
         WHERE ns IN ('TxCentrifuge.TxSubmission.RequestTxIds', 'TxCentrifuge.TxSubmission.ReplyTxIds',
                      'TxCentrifuge.TxSubmission.ReplyTxs')),
  per_minute AS (
    SELECT target, (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute, count(*) AS txs
    FROM v_gen_tx_sent GROUP BY ALL)
SELECT target, minute,
       sum(txs) OVER (PARTITION BY target ORDER BY minute) AS txs_total
FROM per_minute
ORDER BY minute, target;
