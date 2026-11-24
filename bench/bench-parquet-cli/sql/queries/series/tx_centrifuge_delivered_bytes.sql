-- "tx_centrifuge_delivered_bytes": the CUMULATIVE raw CBOR the tx-centrifuge has
-- handed to each target, over time. A series: x = minute, key = target,
-- y = bytes_total. Generator family: experimental.
--
-- Every TxCentrifuge.TxSubmission.ReplyTxs carries its batch as a list of
-- {id, size} (v_gen_tx_sent, one row per tx); size is the transaction's own
-- serialized length, so summing it is summing exactly what went on the wire as
-- transaction payload -- no framing, no protocol overhead, nothing estimated.
-- ONE ROW PER (target, minute) IN WHICH THAT TARGET WAS SENT ANYTHING: a step
-- series; a minute with no delivery has no row and the line holds. The counter
-- never decreases.
-- MINUTE 0 IS THE GENERATOR'S FIRST TxSubmission EVENT (RequestTxIds,
-- ReplyTxIds or ReplyTxs), the same anchor in every tx_centrifuge_* series
-- here. A target whose first delivery comes later simply starts later.
--
-- Fields:
-- --------
-- * target       The node the transactions were sent to, as the generator names
--                it ("node-15") (series key).
-- * minute       Whole minutes since the anchor (x).
-- * bytes_total  Raw CBOR transaction bytes delivered to that target from the
--                anchor through the end of this minute (y).
WITH
  t0 AS (SELECT min(ts) AS t0 FROM raw_generator
         WHERE ns IN ('TxCentrifuge.TxSubmission.RequestTxIds', 'TxCentrifuge.TxSubmission.ReplyTxIds',
                      'TxCentrifuge.TxSubmission.ReplyTxs')),
  per_minute AS (
    SELECT target, (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute, sum(bytes) AS bytes
    FROM v_gen_tx_sent GROUP BY ALL)
SELECT target, minute,
       sum(bytes) OVER (PARTITION BY target ORDER BY minute) AS bytes_total
FROM per_minute
ORDER BY minute, target;
