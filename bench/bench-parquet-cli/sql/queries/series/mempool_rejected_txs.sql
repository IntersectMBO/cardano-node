-- "mempool_rejected_txs": a CUMULATIVE count of the transactions each node's
-- mempool refused, over time. A series: x = minute, key = process,
-- y = rejected_total.
--
-- ONE EVENT IS ONE TRANSACTION. "Mempool.RejectedTx" names exactly one
-- transaction in data.tx.txid, so the counter adds one per event -- there is no
-- list to measure and no size to sum. data.errdetails says why it was refused
-- and is not read here (v_mempool_timeout_soft reads the timeout case).
-- A REJECTION MOVES NOTHING ELSE. The transaction never enters the pool, so the
-- size does not change and no bytes flow.
-- MINUTE 0 IS THE FIRST MEMPOOL EVENT OF THE RUN (any node, any of AddedTx,
-- RemoveTxs, RejectedTx, SwitchedToCertifyingView), the same anchor in every
-- mempool_* series here. A minute in which a node rejected nothing has no row,
-- and the plotted line holds -- the counter never decreases.
--
-- Fields:
-- --------
-- * process         The node (series key).
-- * minute          Whole minutes since the anchor (x).
-- * rejected_total  Transactions that node refused from the anchor through the
--                   end of this minute (y).
WITH
  t0 AS (SELECT min(ts) AS t0 FROM raw
         WHERE ns IN ('Mempool.AddedTx', 'Mempool.RemoveTxs', 'Mempool.RejectedTx',
                      'Mempool.SwitchedToCertifyingView')),
  per_minute AS (
    SELECT process, (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute,
           count(*) AS rejected
    FROM raw WHERE ns IN ('Mempool.RejectedTx')
    GROUP BY ALL)
SELECT process, minute,
       sum(rejected) OVER (PARTITION BY process ORDER BY minute) AS rejected_total
FROM per_minute
ORDER BY minute, process;
