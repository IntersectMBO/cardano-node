-- "Delivered bytes/s" by target over time, per minute: the sizes of the txs the
-- generator delivered to each target (v_gen_tx_sent), summed per minute over 60.
-- A rate, which swings with every idle minute; the package series
-- tx_centrifuge_running_mean_delivery_speed is the running mean that smooths
-- it. Minute 0 is the generator's first TxSubmission event.
-- User content (user_sql/README.md): not reviewed like sql/.
WITH
  t0 AS (SELECT min(ts) AS t0 FROM raw_generator
         WHERE ns IN ('TxCentrifuge.TxSubmission.RequestTxIds', 'TxCentrifuge.TxSubmission.ReplyTxIds',
                      'TxCentrifuge.TxSubmission.ReplyTxs'))
SELECT target, (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute, round(sum(bytes) / 60.0, 2) AS bytes_per_s
FROM v_gen_tx_sent
GROUP BY ALL
ORDER BY minute, target;
