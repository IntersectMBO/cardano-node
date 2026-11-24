-- "v_eb_deliveries_by_peer": every delivery v_eb_deliveries
-- reports, arranged BY PEER — one row per DELIVERY, with the EB's own
-- facts beside it. The deliveries themselves are that view's business and
-- are taken from it verbatim; this one adds the forging's facts and names
-- the forger.
--
-- Tier 2, Leios family: EXPERIMENTAL until the Leios observables are final.
--
-- A (EB, peer) PAIR CAN HAVE SEVERAL ROWS, and that is deliberate. A node
-- offered the same body by several peers is handed it several times, and
-- each hand-over happened, so each is a row. Nothing here picks a first,
-- a best or a last. If you want one row per (EB, peer), say which
-- delivery you mean and make that choice where you make it -- as
-- v_eb_timing does.
-- IT MIRRORS v_block_deliveries_by_peer, column for column, because it is
-- the same question asked of the other kind of block: who received its body
-- over the wire, and when. That twin is what to read it against. The ADOPTED
-- views ask a different question, chain selection, which an endorser block
-- never undergoes.
-- THERE IS NO FORGER COLUMN, and that is the gain from measuring the wire. A
-- node never fetches a block it forged itself, for either kind of block, so
-- there is nothing forger-shaped to carry and no special case to remember.
-- The kernel's "Consensus.LeiosKernel.BlockAcquired" does fire on the
-- forger, once per endorser block, so a view built on it would need one.
-- EVERY FORGED EB HAS A ROW, fetched or not. Filtering one out because
-- nobody took it is not this view's job: an EB no peer ever fetched reads as
-- one row with peer_process and peer_fetched_at NULL, which states exactly
-- that. So v_eb_forged's EBs and this view's EBs are the same set.
-- ABSENCE IS NOT NON-POSSESSION. A node that already holds a body fetches
-- nothing and so has no row here; the trace cannot distinguish that from a
-- node that never had it, and nothing is inferred either way.
--
-- NO slot_start HERE. A slot start is the instant a slot was SCHEDULED
-- for, and the row below is a delivery, not a forging: its slot is the
-- BLOCK's identity, carried so the row can be read against the forging,
-- not a clock for when it was delivered. v_block_forged and v_eb_forged
-- carry slot_start because their ts IS the forging the slot scheduled;
-- join one of them on hash to measure against it.
--
--
-- Fields:
-- --------
-- * hash             The endorser block.
--   (NOT NULL)
--
-- * slot_no          Its slot, when ONE forging states it; NULL for a hash
--   (NULL)           forged more than once (the delivery names no slot).
--
-- * forgings         How many times this hash was forged.
--   (NOT NULL)
--
-- * forger_process   The node that forged it.
--   (NOT NULL)
--
-- * peer_process     The node that received the body. NULL only on the row
--   (NULL)           of an EB nobody fetched.
--
-- * peer_fetched_at  When THIS delivery completed. NULL as peer_process.
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
-- (Via v_eb_forged: forger_process and slot_no.)
--
-- at    2026-10-18T18:41:11.784593689Z
-- ns    LeiosFetch.Remote.Receive.Block
-- data  { "msg":{ "ebBytesSize":25718
--               , "ebHash":"0b302b.."
--               , "kind":"MsgLeiosBlock"
--               }
--       , "peer":{"connectionId":"172.31.14.36:30005 13.238.107.224:30008"}
--       }
-- (Via v_eb_deliveries, one row per peer: peer_fetched_at.)
--
CREATE OR REPLACE VIEW v_eb_deliveries_by_peer AS
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
  by_peer AS MATERIALIZED (
    -- Every delivery, repeats included, carrying the EB's facts with it.
    SELECT f.hash, f.slot_no, f.forgings, f.process AS forger_process
         , d.process AS peer_process, d.ts AS peer_fetched_at
    FROM v_eb_deliveries d
    JOIN forged f ON f.hash = d.hash)
SELECT * FROM by_peer
UNION ALL
-- The EBs nobody fetched: one row each, peer columns NULL.
SELECT f.hash, f.slot_no, f.forgings, f.process, NULL, NULL
FROM forged f
ANTI JOIN by_peer p ON p.hash = f.hash;

COMMENT ON VIEW v_eb_deliveries_by_peer IS 'tier 2, Leios (experimental): every endorser-block delivery with the forging''s facts, one row per delivery (one with NULL peer for an EB nobody fetched): hash, slot_no, forgings, forger_process, peer_process, peer_fetched_at (previously: block_endorser_deliveries_by_peer)';
