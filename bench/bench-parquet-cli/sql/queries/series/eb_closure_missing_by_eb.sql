-- "eb_closure_missing_by_eb", titled "What each endorser block cost its peers
-- to close": one row per endorser block, saying how many of the transactions it
-- references the nodes that held it did NOT already have, and so had to fetch.
--
-- Leios family: EXPERIMENTAL until the Leios observables are final. A series
-- (sql/README.md): x = second, one point per endorser block (no series key).
--
-- ONE ROW PER ENDORSER BLOCK, NOT PER NODE. The per-(EB, node) detail is
-- eb_closure_missing, which carries the same measure unaveraged and on the
-- same axis. This is the summary: one point per endorser block rather than
-- fifty, and the peers' spread -- min, median, max -- beside their mean, so
-- a block the whole cluster was equally short of reads differently from one
-- a single lagging peer dominates.
-- THE NODE STATES IT ITSELF, in "Consensus.LeiosKernel.BodyHits":
-- data.txsInEb is how many transactions the block references, and
-- data.missedBoth how many that node had in NEITHER its mempool nor its
-- transaction cache. Nothing is counted from closure requests, decoded from
-- bitmaps or inferred: a request is neither the missing set nor a batch of
-- it, so counting from the requests gives a different, wrong number.
-- EVERY PEER IS COUNTED, INCLUDING THE ONES THAT NEEDED NOTHING. BodyHits
-- fires for every node that held the block, the forger aside, so peers is
-- every node that held it and a zero-need node pulls the average DOWN
-- rather than vanishing.
-- THE FORGER IS NOT A PEER: it built the block from its own mempool.
-- IT IS A MEMPOOL-DIVERGENCE MEASURE. If every mempool held the same
-- transactions, missing_txs would be zero. The share is how far apart the
-- pools had drifted when that block was made.
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
-- * forger_process  The node that forged it.
--   (NOT NULL)
--
-- * refs            How many transactions it references.
--   (NOT NULL)
--
-- * peers           How many peers reported it (the forger aside).
--   (NOT NULL)
--
-- * missing_avg     Over those peers: the mean (rounded), min, median
-- * missing_min     (nearest rank, as 3_propagation_total and
-- * missing_median  fetched_to_acquired use) and max of what each was
-- * missing_max     missing, in transactions.
--   (NOT NULL)
--
-- * missing_share   Everything the peers were missing over everything they
--   (NOT NULL)      were offered: sum(missing) / (peers * refs), 4 decimals.
--
-- Example (in chronological order):
-- --------
-- 
-- at    2026-10-23T19:14:23.509309789Z
-- ns    Consensus.LeiosKernel.BlockForged
-- data  { "closureSize":25718
--       , "ebSize":25718
--       , "hash":"0b302b.."
--       , "mempoolRestSize":0
--       , "numTxs":695
--       , "slot":592
--       }
-- (second: the forging, via v_eb_forged. The forger's own statement.)
--
-- at    2026-10-20T20:47:33.884023475Z
-- ns    Consensus.LeiosKernel.BodyHits
-- data  { "acquired":0
--       , "cacheLoad":0.0005981922149658203
--       , "cacheTxCount":2509
--       , "ebHash":"46ae8e.."
--       , "ebSlot":506
--       , "mempoolHits":2509
--       , "missedBoth":0
--       , "tracked":0
--       , "txsInEb":2509
--       , "validated":0
--       }
-- (THE MEASURE, whole. This node already held all 2,509 transactions the
-- block references: mempoolHits = txsInEb, missedBoth = 0, so it is a zero
-- row and fetched nothing. A node missing some would read missedBoth > 0.)
--
WITH
  t0 AS (SELECT min(ts) AS t0 FROM v_eb_forged),
  forging AS MATERIALIZED (
    -- One row per endorser-block hash: a re-forge is the same node forging the
    -- same transaction list again, so the forging time is the first.
    SELECT hash, min(ts) AS ts, min(slot_no) AS slot_no, min(slot_start) AS slot_start
         , min(process) AS process, min(tx_refs_count) AS refs
    FROM v_eb_forged
    GROUP BY hash),
  ranked AS MATERIALIZED (
    -- The peers of one endorser block, ordered by how much each was missing,
    -- so that rank n/2 is that block's middle peer.
    SELECT hash AS eb, txs_in_eb AS refs, missed_both AS missed
         , row_number() OVER (PARTITION BY hash ORDER BY missed_both) AS rn
         , count(*)     OVER (PARTITION BY hash)                      AS n
    FROM v_eb_bodyhits
    WHERE peer_process IS NOT NULL)
SELECT
    (epoch_ns(f.ts) - epoch_ns((SELECT t0 FROM t0))) // 1000000000 AS second
  , h.eb
  , f.slot_no
  , f.slot_start
  , f.process                                          AS forger_process
  , max(h.refs)                                        AS refs
  , count(*)                                           AS peers
  , round(avg(h.missed))                               AS missing_avg
  , min(h.missed)                                      AS missing_min
  , max(h.missed) FILTER (WHERE h.rn = least(CAST(floor(h.n * 0.50) AS BIGINT), h.n - 1) + 1)
                                                       AS missing_median
  , max(h.missed)                                      AS missing_max
  , round(sum(h.missed) / (count(*) * max(h.refs)), 4) AS missing_share
FROM ranked h
JOIN forging f ON f.hash = h.eb
GROUP BY h.eb, f.ts, f.slot_no, f.slot_start, f.process
ORDER BY second, eb;
