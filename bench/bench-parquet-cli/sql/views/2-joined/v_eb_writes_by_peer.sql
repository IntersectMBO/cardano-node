-- "v_eb_writes_by_peer": every write v_eb_writes
-- reports, arranged BY PEER — one row per write, with the EB's own facts
-- beside it and its forger's own write alongside.
--
-- Tier 2, Leios family: EXPERIMENTAL until the Leios observables are final.
--
-- THE FORGER HAS A TIMING COLUMN HERE, unlike the fetched pivots. A node
-- writes the endorser block it forged without fetching anything, so the
-- forging has a write of its own; it is carried as forger_written_at rather
-- than as a row, the same arrangement v_block_adoptions_by_peer uses.
-- A (EB, peer) PAIR CAN HAVE SEVERAL ROWS, and here that is a real repeat,
-- not a bracket: the trace has no risingEdge and no closing row, and the
-- kernel settles once per delivered body, so a node offered the same EB by
-- several peers writes it several times. Each of those happened, so each is
-- a row. Nothing here picks a first; that reading belongs downstream. This
-- is the one place the two written pivots differ — its ranking twin is one
-- row per peer, because a bracketed write is still one write.
-- AN EB NOBODY WROTE STILL HAS A ROW, peer columns NULL, so every forged
-- endorser block is accounted for.
-- Axiomatic means a property of the event as the protocol defines it; the
-- traces are only the evidence, and where no trace evidences a value the cell
-- is NULL, never inferred.
--
-- NO slot_start HERE. A slot start is the instant a slot was SCHEDULED
-- for, and the row below is a write, not a forging: its slot is the
-- BLOCK's identity, carried so the row can be read against the forging,
-- not a clock for when it was written. v_block_forged and v_eb_forged
-- carry slot_start because their ts IS the forging the slot scheduled;
-- join one of them on hash to measure against it.
--
--
-- Fields:
-- --------
-- * hash               The endorser block.
--   (NOT NULL)
--
-- * slot_no            Its slot, when ONE forging states it; NULL for a hash
--   (NULL)             forged more than once.
--
-- * forgings           How many times this hash was forged.
--   (NOT NULL)
--
-- * forger_process     The node that forged it.
--   (NOT NULL)
--
-- * forger_written_at  The forger's FIRST write of its own EB; NULL when it
--   (NULL)             has none.
--
-- * peer_process       The node that wrote it -- never the forger. NULL only
--   (NULL)             on the row of an EB no peer wrote.
--
-- * peer_written_at    When THIS write happened. NULL as peer_process.
--   (NULL)
--
-- Example (in chronological order):
-- --------
-- 
-- at    2026-10-18T18:41:11.274194691Z
-- ns    Consensus.LeiosKernel.BlockForged
-- data  { "closureSize":25718
--       , "ebSize":25718
--       , "hash":"0b302b.."
--       , "mempoolRestSize":0
--       , "numTxs":695
--       , "slot":592
--       }
-- (Via v_eb_forged: hash, slot_no, forger_process.)
--
-- at    2026-10-18T18:41:11.285045726Z
-- ns    Consensus.LeiosKernel.BlockAcquired
-- data  { "ebHash":"0b302b.."
--       , "ebSlot":592
--       }
-- (node-8, which FORGED it: forger_written_at. bodyAgeSeconds is absent on
-- a forger's own row.)
--
-- at    2026-10-18T18:41:11.791392668Z
-- ns    Consensus.LeiosKernel.BlockAcquired
-- data  { "bodyAgeSeconds":0.784606042
--       , "ebHash":"0b302b.."
--       , "ebSlot":592
--       }
-- (node-5, a peer: peer_written_at.)
--
CREATE OR REPLACE VIEW v_eb_writes_by_peer AS
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
  by_forger AS MATERIALIZED (
    -- One row per EB, with its forger's FIRST write of it when there was one.
    SELECT f.hash, f.slot_no, f.forgings, f.process, min(w.ts) AS written_at
    FROM forged f
    LEFT JOIN v_eb_writes w ON w.hash = f.hash AND w.process = f.process
    GROUP BY ALL),
  by_peer AS MATERIALIZED (
    -- Every write by a node OTHER than the EB's forger, repeats included.
    SELECT b.hash, b.slot_no, b.forgings, b.process AS forger_process
         , b.written_at AS forger_written_at
         , w.process AS peer_process, w.ts AS peer_written_at
    FROM v_eb_writes w
    JOIN by_forger b ON b.hash = w.hash AND w.process <> b.process)
SELECT * FROM by_peer
UNION ALL
-- The EBs no peer wrote: one row each, peer columns NULL.
SELECT b.hash, b.slot_no, b.forgings, b.process, b.written_at, NULL, NULL
FROM by_forger b
ANTI JOIN by_peer p ON p.hash = b.hash;

COMMENT ON VIEW v_eb_writes_by_peer IS 'tier 2, Leios (experimental): every endorser-block write by a peer with the forger''s own first write alongside, one row per peer write (one with NULL peer for an EB no peer wrote) (previously: block_endorser_writes_by_peer)';
