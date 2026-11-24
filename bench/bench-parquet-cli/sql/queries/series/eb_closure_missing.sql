-- "eb_closure_missing", titled "Mempool fragmentation, against the endorser
-- blocks": for each endorser block and each node that held it, how many of the
-- transactions that block references the node did NOT already have.
--
-- Leios family: EXPERIMENTAL until the Leios observables are final. A series
-- (sql/README.md): x = second, series key = process, one row per (endorser block, peer).
--
-- THAT IS THE FRAGMENTATION. If every mempool held the same transactions, an
-- endorser block built by one node would reference only transactions every
-- other node already had, missing_txs would be 0, and no closure would ever
-- need fetching. missing_share of 1 is the opposite: the node shares nothing
-- with the forger's mempool and must pull the whole closure.
-- THE NODE STATES IT ITSELF, in "Consensus.LeiosKernel.BodyHits", one row per
-- endorser block it held: data.txsInEb is how many transactions the block
-- references, data.mempoolHits how many it already had, and data.missedBoth
-- how many it had in NEITHER its mempool nor its transaction cache and so
-- had to fetch. Nothing is counted from requests, decoded from bitmaps or
-- inferred; the kernel publishes the answer.
-- hits + missed = txsInEb ON EVERY ROW, so the three columns are a complete
-- partition of the block's references and missing_share cannot exceed 1.
-- A NODE THAT NEEDED NOTHING IS A ZERO ROW, not a missing one. BodyHits
-- fires for every node that held the block, needed or not, the forger
-- aside, so the population needs no reconstruction and no LEFT JOIN.
-- DO NOT COUNT THIS FROM THE CLOSURE REQUESTS. A node's requests are not its
-- missing set: it may re-ask for what is still outstanding, and it may fill
-- some references locally before asking. Taking the first request
-- undercounts, and summing all requests overcounts. The bitmaps do name exactly which references each
-- request covers, and their union is a node's requested set, but that is a
-- different question from what it was missing.
-- ONE ROW PER (ENDORSER BLOCK, NODE), placed at the second the block was
-- FORGED: the one instant every peer's answer refers to.
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
-- * process        The node that held it -- never the forger (v_eb_bodyhits).
--   (NOT NULL)
--
-- * refs           How many transactions the block references (txsInEb).
--   (NOT NULL)
--
-- * mempool_hits   How many of them the node's mempool supplied.
--   (NOT NULL)
--
-- * missing_txs    How many it did NOT already have (missedBoth): the y in
--   (NOT NULL)     absolute numbers.
--
-- * missing_share  missing_txs / refs, 4 decimals: the y.
--   (NOT NULL)
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
    GROUP BY hash)
SELECT
    (epoch_ns(f.ts) - epoch_ns((SELECT t0 FROM t0))) // 1000000000 AS second
  , b.hash                                       AS eb
  , f.slot_no
  , f.slot_start
  , b.peer_process                               AS process
  , b.txs_in_eb                                  AS refs
  , b.mempool_hits
  , b.missed_both                                AS missing_txs
  , round(b.missed_both / b.txs_in_eb, 4)        AS missing_share
FROM v_eb_bodyhits b
JOIN forging f ON f.hash = b.hash
WHERE b.peer_process IS NOT NULL
ORDER BY second, eb, process;
