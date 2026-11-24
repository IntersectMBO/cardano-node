-- "eb_closure_fetched_txs", titled "Endorser-block closure fetching,
-- transactions": for each endorser block and each node that held it, how many
-- transactions that node pulled for that block's closure, against how many the
-- block references.
--
-- Leios family: EXPERIMENTAL until the Leios observables are final. A series
-- (sql/README.md): x = second, series key = process, one row per (endorser block, peer).
--
-- ONE ROW IS ONE NODE'S WHOLE CLOSURE FETCH FOR ONE ENDORSER BLOCK, placed
-- at the second that block was FORGED. Every request that node sent for that
-- block is summed into the row, however many peers it asked and however many
-- times. So a point is not a request and not a minute: it is the complete
-- cost, to one node, of closing one endorser block.
-- txs_fetched OVER refs IS THE OVER-FETCH RATIO. 1.0 means the node pulled
-- exactly as many transactions as the block references; above 1.0 the same
-- transaction arrived more than once, because the node asks several peers
-- for overlapping ranges and keeps asking until a transaction is held. Below
-- 1.0 it already had some of the closure and fetched only the rest -- see
-- eb_closure_missing for how much it was missing to begin with.
-- A NODE THAT FETCHED NOTHING IS A ZERO ROW, not an absent one: it held the
-- block and needed none of its closure.
-- THE FORGER IS NOT A PEER: it built the block from its own mempool.
-- THE X AXIS IS THE FORGING, NOT A TIME BUCKET. The endorser block is the
-- unit the question is about, and its forging is the one instant every
-- peer's answer refers to.
--
--
-- Fields:
-- --------
-- * second         When the endorser block was FORGED (its first forging), in
--   (NOT NULL)     whole seconds since the run's first endorser block -- the x
--                  axis, shared by every eb_closure_* series.
--
-- * eb             The endorser block.
--   (NOT NULL)
--
-- * slot_no        The slot it was (first) forged in, and when that slot
-- * slot_start     began, per genesis.
--   (NOT NULL)
--
-- * process        The node that held it -- never the forger.
--   (NOT NULL)
--
-- * refs           How many transactions the block references.
--   (NOT NULL)
--
-- * requests       Closure requests this node sent for this block, answered
--   (NOT NULL)     or not. 0 when it needed none of the closure.
--
-- * txs_fetched    Transactions that arrived in answer, repeats included.
--   (NOT NULL)     txs_fetched / refs is the over-fetch ratio (the y).
--
-- Example (in chronological order):
-- --------
-- 
-- at    2026-10-23T19:14:46.281672403Z
-- ns    LeiosFetch.Remote.Send.BlockTxsRequest
-- data  { "msg":{ "bitmaps":"[\"5:0x0421000007fffc00\",\"6:0x0.."
--               , "ebHash":"360a96.."
--               , "ebSlot":501
--               , "kind":"MsgLeiosBlockTxsRequest"
--               , "numTxs":158
--               }
--       , "peer":{"connectionId":"172.31.14.36:30005 44.216.245.43:30004"}
--       }
-- (requests, and the endorser block they are for: the only trace naming it,
-- so the replies and arrivals are tied to the block through it.)
--
-- at    2026-10-23T19:14:46.614769975Z
-- ns    LeiosFetch.Remote.Receive.BlockTxs
-- data  { "msg":{ "kind":"MsgLeiosBlockTxs"
--               , "numTxs":158
--               , "txsBytesSize":58618
--               }
--       , "peer":{"connectionId":"172.31.14.36:30005 44.216.245.43:30004"}
--       }
-- (txs_fetched: numTxs, summed into this row.)
--
-- at    2026-10-23T19:14:46.615736007Z
-- ns    Consensus.LeiosKernel.FetchTxsArrival
-- data  { "evictedBytes":0
--       , "extraBytes":58618
--       , "goodBytes":0
--       , "invalidBytes":0
--       }
-- (The kernel's account of that arrival; eb_closure_fetched_bytes uses it.
-- Here the node needed NONE of the 58,618 bytes it had just pulled: all 158
-- transactions were ones it already held.)
--
WITH
  t0 AS (SELECT min(ts) AS t0 FROM v_eb_forged),
  req AS MATERIALIZED (
    -- Closure requests, ranked per connection in log order.
    SELECT process, data->>'$.peer.connectionId' AS conn, data->>'$.msg.ebHash' AS eb
         , row_number() OVER (PARTITION BY process, data->>'$.peer.connectionId' ORDER BY ts, seq) AS rn
    FROM raw WHERE ns = 'LeiosFetch.Remote.Send.BlockTxsRequest'),
  rep AS MATERIALIZED (
    -- Their replies, ranked the same way: the n-th reply on a connection
    -- answers its n-th request. fn ranks the replies per node, to pair them
    -- with the kernel's arrivals.
    SELECT process, data->>'$.peer.connectionId' AS conn
         , CAST(data->>'$.msg.numTxs' AS UBIGINT)       AS n
         , CAST(data->>'$.msg.txsBytesSize' AS UBIGINT) AS b
         , row_number() OVER (PARTITION BY process, data->>'$.peer.connectionId' ORDER BY ts, seq) AS rn
         , row_number() OVER (PARTITION BY process ORDER BY ts, seq) AS fn
    FROM raw WHERE ns = 'LeiosFetch.Remote.Receive.BlockTxs'),
  forging AS MATERIALIZED (
    -- One row per endorser-block hash: a re-forge is the same node forging the
    -- same transaction list again, so the forging time is the first.
    SELECT hash, min(ts) AS ts, min(slot_no) AS slot_no, min(slot_start) AS slot_start
         , min(process) AS process, min(tx_refs_count) AS refs
    FROM v_eb_forged
    GROUP BY hash),
  peer AS MATERIALIZED (
    -- Every node that held the endorser block, the forger aside: the
    -- population, including the ones that fetched none of its closure.
    SELECT DISTINCT w.hash AS eb, w.process
    FROM v_eb_writes w JOIN forging f ON f.hash = w.hash
    WHERE w.process <> f.process)
SELECT
    (epoch_ns(f.ts) - epoch_ns((SELECT t0 FROM t0))) // 1000000000 AS second
  , p.eb
  , f.slot_no
  , f.slot_start
  , p.process
  , f.refs
  , coalesce(g.requests, 0)  AS requests
  , coalesce(g.txs, 0)       AS txs_fetched
FROM peer p
JOIN forging f ON f.hash = p.eb
-- LEFT: a node that fetched nothing must still be a row -- a zero.
LEFT JOIN (
    SELECT q.process, q.eb, count(*) AS requests, sum(r.n) AS txs
    FROM req q
    LEFT JOIN rep r ON r.process = q.process AND r.conn = q.conn AND r.rn = q.rn
    GROUP BY q.process, q.eb) g ON g.eb = p.eb AND g.process = p.process
ORDER BY second, eb, process;
