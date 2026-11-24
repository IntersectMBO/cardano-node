-- "tx_centrifuge_circulating_funds": how many UTxO the tx-centrifuge's own funds
-- are spread across on the chain, over time. NOT per target and NOT per node --
-- there is one chain, so there is one series: x = minute, y = circulating.
-- Generator family: experimental.
--
-- WHAT MOVES IT. Sending a transaction to a node puts its OUTPUTS on the chain
-- (TxCentrifuge.TxSubmission.ReplyTxs names which transactions, v_gen_tx_sent),
-- so a delivery ADDS that many UTxO. Recycling retires them again
-- (TxCentrifuge.Recycler.Backlog.Released.*), so a release REMOVES that many.
-- The running difference is how many UTxO the funds are spread across.
-- data.inputs IS A MISLEADING FIELD NAME and is not read. On .Confirmed it holds
-- the UTxO confirmed for RE-USE, the transaction's own outputs; on .Orphaned it
-- is the other side of a transaction a node discarded. Only the COUNT matters,
-- and data.count states it (it equals the length of data.inputs).
-- THE OUTPUT COUNT IS READ, NOT ASSUMED. ReplyTxs names each transaction but not
-- how many outputs it has, so the delivered ids are joined to
-- TxCentrifuge.Builder.NewTx, whose data.outputs list is counted: not
-- hardcoding the profile's outputs-per-transaction.
-- A TRANSACTION ADDS ITS OUTPUTS ONCE, at its first delivery: one built and
-- delivered twice (same id, same hash) reaches the chain once. Counting per
-- delivery and per NewTx row would count such a tx four times.
-- BOTH RELEASE PATHS RETIRE UTxO, and both count; .Orphaned is also carried in
-- its own column, so a reading that wants only the confirmed path can subtract
-- it back out.
-- MINUTE 0 IS THE GENERATOR'S FIRST TxSubmission EVENT, the same anchor in every
-- tx_centrifuge_* series here. A MINUTE WITH NO EVENT HAS NO ROW.
--
-- Fields:
-- --------
-- * minute            Whole minutes since the anchor (x).
-- * added             UTxO created this minute: the outputs of the transactions
--                     delivered in it.
-- * removed           UTxO consumed this minute, both release paths together.
-- * removed_orphaned  The part of removed that came from .Orphaned.
-- * circulating       The running total of added minus removed, from the anchor
--                     through the end of this minute (y). It starts at zero (the
--                     opening balance was created before the run), so it is the
--                     CHANGE in that spread, not its absolute size.
WITH
  t0 AS (SELECT min(ts) AS t0 FROM raw_generator
         WHERE ns IN ('TxCentrifuge.TxSubmission.RequestTxIds', 'TxCentrifuge.TxSubmission.ReplyTxIds',
                      'TxCentrifuge.TxSubmission.ReplyTxs')),
  outputs AS (
    SELECT data->>'$.txId' AS id, any_value(json_array_length(data->'$.outputs')) AS n
    FROM raw_generator WHERE ns IN ('TxCentrifuge.Builder.NewTx') GROUP BY id),
  first_sent AS (SELECT id, min(ts) AS ts FROM v_gen_tx_sent GROUP BY id),
  events AS (
    SELECT (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute, o.n AS added, 0 AS removed, 0 AS removed_orphaned
    FROM first_sent d JOIN outputs o USING (id)
    UNION ALL
    SELECT (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000, 0, CAST(data->>'$.count' AS BIGINT), 0
    FROM raw_generator WHERE ns IN ('TxCentrifuge.Recycler.Backlog.Released.Confirmed')
    UNION ALL
    SELECT (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000, 0, CAST(data->>'$.count' AS BIGINT), CAST(data->>'$.count' AS BIGINT)
    FROM raw_generator WHERE ns IN ('TxCentrifuge.Recycler.Backlog.Released.Orphaned')),
  per_minute AS (
    SELECT minute, sum(added) AS added, sum(removed) AS removed,
           sum(removed_orphaned) AS removed_orphaned
    FROM events GROUP BY minute)
SELECT minute, added, removed, removed_orphaned,
       sum(added - removed) OVER (ORDER BY minute) AS circulating
FROM per_minute
ORDER BY minute;
