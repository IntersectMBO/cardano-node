-- "Delivered txs/s" by target over time, per minute: the txs the generator
-- delivered to each target (v_gen_tx_sent), counted per minute over 60. A rate;
-- tx_centrifuge_delivered_txs is the cumulative package series. Minute 0 is the
-- generator's first TxSubmission event.
-- User content (user_sql/README.md): not reviewed like sql/.
WITH
  t0 AS (SELECT min(ts) AS t0 FROM raw_generator
         WHERE ns IN ('TxCentrifuge.TxSubmission.RequestTxIds', 'TxCentrifuge.TxSubmission.ReplyTxIds',
                      'TxCentrifuge.TxSubmission.ReplyTxs'))
SELECT target, (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute, round(count(*) / 60.0, 2) AS txs_per_s
FROM v_gen_tx_sent
GROUP BY ALL
ORDER BY minute, target;
