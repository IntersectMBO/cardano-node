-- "tx_centrifuge_unserved_demand": a CUMULATIVE counter of what each target has
-- asked the tx-centrifuge for and not been given, over time. A series:
-- x = minute, key = target, y = unserved_txs. Generator family: experimental.
--
-- Every TxCentrifuge.TxSubmission.RequestTxIds adds its data.req -- the number of
-- NEW transaction ids that target is asking for this round -- and every
-- TxCentrifuge.TxSubmission.ReplyTxs subtracts the transactions that batch
-- actually delivered.
-- A GROWING VALUE IS DEMAND THE GENERATOR CANNOT SERVE. Flat means it is keeping
-- up. The counter can fall as well as rise: not monotonic.
-- data.req IS THE ASK, NOT THE BACKLOG. data.ack (ids the target acknowledges out
-- of its window) is NOT read: acknowledging releases what the target already
-- has, which is not demand.
-- ONE ROW PER (target, minute) IN WHICH EITHER STREAM HAD AN EVENT; a minute with
-- neither has no row and the line holds.
-- MINUTE 0 IS THE GENERATOR'S FIRST TxSubmission EVENT, the same anchor in every
-- tx_centrifuge_* series here.
--
-- Fields:
-- --------
-- * target        The node doing the asking, as the generator names it (series
--                 key).
-- * minute        Whole minutes since the anchor (x).
-- * unserved_txs  Transaction ids asked for minus transactions delivered, summed
--                 from the anchor through the end of this minute (y).
WITH
  t0 AS (SELECT min(ts) AS t0 FROM raw_generator
         WHERE ns IN ('TxCentrifuge.TxSubmission.RequestTxIds', 'TxCentrifuge.TxSubmission.ReplyTxIds',
                      'TxCentrifuge.TxSubmission.ReplyTxs')),
  events AS (
    SELECT data->>'$.target' AS target, (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute,
           CAST(data->>'$.req' AS BIGINT) AS asked, 0 AS delivered
    FROM raw_generator WHERE ns IN ('TxCentrifuge.TxSubmission.RequestTxIds')
    UNION ALL
    SELECT data->>'$.target', (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000,
           0, json_array_length(data->'$.txs')
    FROM raw_generator WHERE ns IN ('TxCentrifuge.TxSubmission.ReplyTxs')),
  per_minute AS (
    SELECT target, minute, sum(asked) AS asked, sum(delivered) AS delivered
    FROM events GROUP BY ALL)
SELECT target, minute,
       sum(asked - delivered) OVER (PARTITION BY target ORDER BY minute) AS unserved_txs
FROM per_minute
ORDER BY minute, target;
