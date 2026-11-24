-- "eb_closure_fetched_bytes", titled "Endorser-block closure fetching, bytes":
-- for each endorser block and each node that held it, how many bytes of that
-- block's closure the node pulled, split into what it kept and what it did not
-- need.
--
-- Leios family: EXPERIMENTAL until the Leios observables are final. A series
-- (sql/README.md): x = second, series key = process, one row per (endorser block, peer).
--
-- ONE ROW IS ONE NODE'S WHOLE CLOSURE FETCH FOR ONE ENDORSER BLOCK, placed
-- at the second that block was FORGED -- the same point as the transactions
-- twin, counted in bytes instead.
-- THE SPLIT IS THE KERNEL'S OWN. "Consensus.LeiosKernel.FetchTxsArrival"
-- states data.goodBytes, which it kept, and data.extraBytes, which it
-- already had or did not need. extra_bytes is what the re-asking costs, in
-- the node's own accounting rather than anything inferred here.
-- THE SPLIT IS NOT CLEANLY PER ENDORSER BLOCK, AND THIS IS THE ONE PLACE IT
-- SHOWS. The kernel fills transactions ACROSS endorser blocks: bytes kept
-- while fetching for one block may satisfy another, and the arrival is
-- credited to the request that triggered it. So good_bytes for a single
-- block can exceed that block's own closure, and the ratio good/(good+extra)
-- is sound in aggregate but not as a per-block statement. bytes_fetched is
-- exact either way -- it is what the reply carried.
-- A NODE THAT FETCHED NOTHING IS A ZERO ROW, not an absent one.
-- THE FORGER IS NOT A PEER: it built the block from its own mempool.
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
-- * requests       Closure requests this node sent for this block, answered
--   (NOT NULL)     or not. 0 when it needed none of the closure.
--
-- * bytes_fetched  Transaction bytes that arrived in answer, as the replies
--   (NOT NULL)     state them. Exact, repeats included.
--
-- * good_bytes     Of those, what the kernel kept, and
-- * extra_bytes    what it already had or did not need. See above on why
--   (NOT NULL)     these two are not a clean per-block split.
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
-- (bytes_fetched: txsBytesSize, summed into this row.)
--
-- at    2026-10-23T19:14:46.615736007Z
-- ns    Consensus.LeiosKernel.FetchTxsArrival
-- data  { "evictedBytes":0
--       , "extraBytes":58618
--       , "goodBytes":0
--       , "invalidBytes":0
--       }
-- (good_bytes and extra_bytes. Here the node needed NONE of the 58,618 bytes
-- it had just pulled: all 158 transactions were ones it already held.)
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
  arr AS MATERIALIZED (
    -- The kernel's account of each arrival, ranked per node: the n-th arrival
    -- is the n-th reply (fn).
    SELECT process
         , CAST(data->>'$.goodBytes'  AS UBIGINT) AS g
         , CAST(data->>'$.extraBytes' AS UBIGINT) AS e
         , row_number() OVER (PARTITION BY process ORDER BY ts, seq) AS fn
    FROM raw WHERE ns = 'Consensus.LeiosKernel.FetchTxsArrival'),
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
  , coalesce(g.requests, 0)  AS requests
  , coalesce(g.bytes, 0)     AS bytes_fetched
  , coalesce(g.good, 0)      AS good_bytes
  , coalesce(g.extra, 0)     AS extra_bytes
FROM peer p
JOIN forging f ON f.hash = p.eb
LEFT JOIN (
    SELECT q.process, q.eb, count(*) AS requests, sum(r.b) AS bytes, sum(a.g) AS good, sum(a.e) AS extra
    FROM req q
    LEFT JOIN rep r ON r.process = q.process AND r.conn = q.conn AND r.rn = q.rn
    LEFT JOIN arr a ON a.process = r.process AND a.fn = r.fn
    GROUP BY q.process, q.eb) g ON g.eb = p.eb AND g.process = p.process
ORDER BY second, eb, process;
