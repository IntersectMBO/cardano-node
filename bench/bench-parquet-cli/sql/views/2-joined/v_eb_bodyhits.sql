-- "v_eb_bodyhits": every "Consensus.LeiosKernel.BodyHits" the
-- kernel traced, one row per (endorser block, peer), with the block's own
-- facts beside it. It is a node's own account of what it already had the
-- moment a block's body landed: how much of the referenced transaction list
-- its cache tracked, how much its mempool held, and how much was in neither
-- and so had to be fetched.
--
-- Tier 2, Leios family: EXPERIMENTAL until the Leios observables are final.
--
-- THE TRACE FIRES ON THE CACHE INSERT, once per received body, before the
-- node decides what of the closure it still needs.
-- THE FOUR CACHE COUNTS NEST, as the kernel states them:
--   txs_in_eb >= tracked >= acquired >= validated
-- tracked is, of the referenced transactions, how many the cache already
-- tracked; acquired is, of those tracked, how many were already in hand;
-- validated is, of those acquired, how many had already been validated.
-- THEY DO NOT REDUCE THE FETCH SET. The cache records transaction hashes
-- seen, but bytes are owned per (block, offset), so a hash seen in another
-- block says nothing about whether THIS block's row is filled; the kernel
-- does not consult the cache when deciding what to fetch. Only the mempool
-- does that, which is why tracked can run well above mempool_hits while
-- missed_both still counts nearly the whole list.
-- mempool_hits AND missed_both ARE ONE LOOKUP, TWO HALVES. The kernel looks
-- the FULL reference list up in the mempool and keeps both sides of the
-- answer: what the mempool supplied, and what it could not. So
-- mempool_hits + missed_both = txs_in_eb by construction, not by luck, and
-- either column determines the other.
-- DESPITE THE NAME, missed_both IS NOT "missed by the cache too". The cache
-- is deliberately not consulted: it records hashes seen, but bytes are owned
-- per (block, offset), and letting a hash seen elsewhere suppress a fetch is
-- how a closure never completes. The name records the intent; the number is
-- the mempool's complement.
-- mempool_hits may therefore OVERLAP the cache counts and must never be
-- added to them.
-- ONE ROW PER (BLOCK, PEER), never two: a given body is inserted once.
-- A PEER HAS A ROW EXACTLY WHEN IT WROTE THE BODY: no row here lacks a write
-- in v_eb_writes, and the only write per endorser block without a row here
-- is the forger's own. So a block with fewer rows than the cluster holds is
-- a block not every peer received, which is what the last blocks of a run
-- look like, and not a gap in the trace. A peer without a row is NOT a peer
-- that needed nothing: that reads as a row with missed_both = 0.
-- THE FORGER HAS NO ROW FOR ITS OWN BLOCK HERE, as in every *_by_peer view.
-- Whether the forger traces BodyHits for its own block depends on the node
-- build; where it does, the counts describe the forging, not a reception,
-- and are left out (see peer_process below).
-- A BLOCK NO PEER REPORTED STILL HAS A ROW, peer columns NULL, so every
-- forged endorser block is accounted for.
-- Axiomatic means a property of the event as the protocol defines it; the
-- traces are only the evidence, and where no trace evidences a value the cell
-- is NULL, never inferred.
--
-- NO slot_start HERE. A slot start is the instant a slot was SCHEDULED
-- for, and the row below is a body insert, not a forging: its slot is the
-- BLOCK's identity, carried so the row can be read against the forging,
-- not a clock for when it was inserted. v_block_forged and v_eb_forged
-- carry slot_start because their ts IS the forging the slot scheduled;
-- join one of them on hash to measure against it.
--
--
-- Fields:
-- --------
-- * hash            The endorser block: the hash of its (tx hash, size) list.
--   (NOT NULL)
--
-- * slot_no         Its slot. On a peer's row it is the trace's own ebSlot, so
--   (NULL)          a hash forged more than once needs no disambiguating -- the
--                   node names the forging whose body it inserted. NULL only
--                   on the row of a block no peer reported AND forgings > 1,
--                   where no trace says which forging is meant.
--
-- * forgings        How many times this hash was forged. A re-forge is the same
--   (NOT NULL)      node forging an unchanged transaction list again, into a
--                   later slot.
--
-- * forger_process  The node that forged it.
--   (NOT NULL)
--
-- * peer_process    The node this row speaks for -- never the forger. NULL on
--   (NULL)          the row of a block no peer reported.
--                   A forger's BodyHits for its own EB, where its build
--                   traces them, are left out here, as in every *_by_peer
--                   view; read "Consensus.LeiosKernel.BodyHits" for them.
--
-- * peer_at         When that node inserted the body into its transaction
--   (NULL)          cache. EVERY COUNT BELOW IS READ AT THIS INSTANT -- they
--                   are a snapshot of what the node already had the moment
--                   the body landed, not a record of what it then did. The
--                   work that follows is in v_eb_validated, seconds later.
--
-- * txs_in_eb       How many transactions the block references -- the total
--   (NULL)          every count below is relative to. The series call this
--                   refs.
--
-- * tracked         Of those referenced transactions, how many hashes the
--   (NULL)          node's cache had already seen. SEEN, NOT HELD: the cache
--                   is an in-memory INDEX over the LeiosDb, not a store of
--                   bytes. A hash it saw in ANOTHER block says nothing about
--                   whether this block's bytes are present. The widest of the
--                   three cache counts.
--
-- * acquired        Of those tracked, how many the cache had already acquired
--   (NULL)          -- inserted or validated. A subset of tracked.
--
-- * validated       Of those acquired, how many had already been validated
--   (NULL)          BEFORE this body landed. Prior state, NOT work this block
--                   caused: v_eb_validated's applied answers that, and the two
--                   do not agree.
--
--                   The three above nest, txs_in_eb >= tracked >= acquired >=
--                   validated, and NONE of them reduces what must be fetched.
--
-- * mempool_hits    How many of the referenced transactions the node's mempool
--   (NULL)          supplied. Counted over the FULL reference list, so it MAY
--                   OVERLAP tracked, acquired and validated and must never be
--                   added to them. The only count that spares a fetch.
--
-- * missed_both     The other half of that same mempool lookup:
--   (NULL)          txs_in_eb - mempool_hits exactly, by construction. 0 says
--                   the mempool already held the whole closure. An upper
--                   bound on what had to be fetched, not a wire count.
--
-- * cache_txs       How many transactions the node's cache tracks IN TOTAL
--   (NULL)          after this insert -- a gauge of the node, not of the block.
--
-- * cache_load      cache_txs over the index's allocated capacity -- a hash
--   (NULL)          table LOAD FACTOR; the kernel notes latency degrades past
--                   about 0.5 and it risks failing past 1.0.
--
-- Example (in chronological order):
-- --------
--
-- at    2026-10-23T20:10:16.275038341Z
-- ns    Consensus.LeiosKernel.BlockForged
-- data  { "closureSize":25718
--       , "ebSize":25718
--       , "hash":"7f9895.."
--       , "mempoolRestSize":0
--       , "numTxs":695
--       , "slot":3832
--       }
-- (hash, slot_no, forger_process and forgings, through v_eb_forged.)
--
-- at    2026-10-23T20:10:16.398305513Z
-- ns    Consensus.LeiosKernel.BodyHits
-- data  { "acquired":191
--       , "cacheLoad":0.0054242610931396484
--       , "cacheTxCount":22751
--       , "ebHash":"7f9895.."
--       , "ebSlot":3832
--       , "mempoolHits":46
--       , "missedBoth":649
--       , "tracked":224
--       , "txsInEb":695
--       , "validated":14
--       }
-- (Everything else. The nesting holds -- 695 >= 224 >= 191 >= 14 -- and so
-- does the partition: 46 + 649 = 695. Note what the counts do NOT buy: the
-- cache tracked 224 of the referenced transactions and held 191 of them, yet
-- the node still had to fetch 649, because only the 46 the mempool supplied
-- came off the fetch set.)
--
CREATE OR REPLACE VIEW v_eb_bodyhits AS
WITH
  forged AS MATERIALIZED (
    -- One row per EB hash. An EB hash has one forger (a re-forge is the same
    -- node forging an unchanged tx list again); a second forger stops the
    -- query rather than being hidden. The slot only when one forging states it.
    SELECT hash
         , CASE WHEN count(DISTINCT process) = 1 THEN any_value(process)
                ELSE error('EB ' || hash || ' forged by more than one node') END AS process
         , CASE WHEN count(*) = 1 THEN min(slot_no) END                         AS slot_no
         , count(*)                                                             AS forgings
    FROM v_eb_forged
    GROUP BY hash),
  hits AS MATERIALIZED (
    -- Every BodyHits as traced. The slot is the trace's own ebSlot.
    SELECT data->>'$.ebHash'                          AS hash
         , CAST(data->>'$.ebSlot'       AS UBIGINT)   AS slot_no
         , process                                    AS peer_process
         , ts                                         AS peer_at
         , CAST(data->>'$.txsInEb'      AS UBIGINT)   AS txs_in_eb
         , CAST(data->>'$.tracked'      AS UBIGINT)   AS tracked
         , CAST(data->>'$.acquired'     AS UBIGINT)   AS acquired
         , CAST(data->>'$.validated'    AS UBIGINT)   AS validated
         , CAST(data->>'$.mempoolHits'  AS UBIGINT)   AS mempool_hits
         , CAST(data->>'$.missedBoth'   AS UBIGINT)   AS missed_both
         , CAST(data->>'$.cacheTxCount' AS UBIGINT)   AS cache_txs
         , CAST(data->>'$.cacheLoad'    AS DOUBLE)    AS cache_load
    FROM raw
    WHERE ns = 'Consensus.LeiosKernel.BodyHits')
-- Every peer's report, with the forging's facts attached.
SELECT h.hash, h.slot_no, f.forgings, f.process AS forger_process
     , h.peer_process, h.peer_at, h.txs_in_eb, h.tracked, h.acquired, h.validated
     , h.mempool_hits, h.missed_both, h.cache_txs, h.cache_load
FROM hits h
JOIN forged f ON f.hash = h.hash AND h.peer_process <> f.process
UNION ALL
-- The blocks no peer reported: one row each, peer columns NULL.
SELECT f.hash, f.slot_no, f.forgings, f.process
     , NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
FROM forged f
ANTI JOIN hits h ON h.hash = f.hash AND h.peer_process <> f.process;

COMMENT ON VIEW v_eb_bodyhits IS 'tier 2, Leios (experimental): every Consensus.LeiosKernel.BodyHits, one row per (EB, peer) with the forging''s facts (one row with NULL peer for an EB no peer reported): cache and mempool hit counts at body insert (previously: block_endorser_bodyhits)';
