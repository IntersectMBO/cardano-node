-- "v_block_adoptions_by_peer": every adoption v_block_adoptions
-- reports, arranged BY PEER — one row per (block, peer), with the block's own
-- facts and its forger's adoption repeated alongside, so the two can be read
-- against each other. The adoptions themselves are that view's business and
-- are taken from it verbatim; this one only decides which of them is the
-- forger's and which is a peer's, using the forger v_block_forged
-- names. The pipeline a block travelled before a peer could adopt it is
-- v_block_adoptions_timing, built on this view.
-- EVERY FORGED BLOCK HAS A ROW, adopted or not. Filtering a block out
-- because nobody adopted it is not this view's job: a forging whose
-- adoptions are all missing reads as one row with forger_adopted_at,
-- peer_process and peer_adopted_at all NULL, which states exactly that. So
-- v_block_forged's blocks and this view's blocks are the same set, and
-- a block its forger adopted and no peer did keeps one row with the peer
-- columns NULL.
-- THE FORGER'S ADOPTION IS NOT A PRECONDITION and is not necessarily the
-- first. Pipelining lets a node serve a header and body before its own
-- ChainDB finishes selecting the block, so peers can adopt ahead of the
-- forger, by up to seconds. A peer adoption therefore never depends on
-- the forger's, in this view or in the protocol.
-- A block its forger discards (Forge.Loop.DidntAdoptBlock) is normally
-- adopted by no peer either; the view does not rely on that: it is a LEFT
-- join either way, so a counter-example appears as rows rather than
-- vanishing.
-- Every timestamp is named for whose step it is; there is no bare `ts`,
-- because a row carries two.
--
-- NO slot_start HERE. A slot start is the instant a slot was SCHEDULED
-- for, and the row below is an adoption, not a forging: its slot is the
-- BLOCK's identity, carried so the row can be read against the forging,
-- not a clock for when it was adopted. v_block_forged and "v_eb_forged"
-- carry slot_start because their ts IS the forging the slot scheduled;
-- join one of them on hash to measure against it.
--
-- (Tier 2.)
--
-- Fields:
-- --------
-- * hash               The block.
--   (NOT NULL)
--
-- * block_no           Its height and slot, both from its header. They are
-- * slot_no            here so that a fork can be filtered by the height or
--   (NOT NULL)         slot it was fought at: v_fork_switches names those
--                      without naming any hash.
--
-- * forger_process     The node that forged it.
--   (NOT NULL)
--
-- * forger_adopted_at  When the forger put it on its own chain: the
--   (NULL)             self-adoption. The forge loop states this a second time
--                      of its own (Forge.Loop.AdoptedBlock, a few ms later, the
--                      source of v_block_forged's bytes); this column is
--                      always v_block_adoptions's, so forger and peer are the
--                      same kind of statement. NULL when the forger never
--                      adopted it, which is what Forge.Loop.DidntAdoptBlock
--                      leaves behind.
--
-- * peer_process       The other node that adopted it. NULL only on the
--   (NULL)             single row of a block no peer adopted, as is
--                      peer_adopted_at.
--
-- * peer_adopted_at    When that peer put it on its chain, by either path,
--   (NULL)             from v_block_adoptions verbatim.
--
-- Example (in chronological order):
------------------------------------
--
-- at    2026-10-18T18:41:11.362292619Z
-- ns    ChainDB.AddBlockEvent.AddedToCurrentChain
-- data  { "newSuffixSelectView":
--         { "blockNo":24
--         , "issueNo":0
--         , "issuerHash":"6484e5.."
--         , "kind":"PraosTiebreakerView"
--         , "slotNo":592
--         , "tieBreakVRF":"9a0ee6.."
--         , "weightBoost":0
--         }
--       , "newtip":"bb416f..@592"
--       }
-- (node-8, which FORGED this block: forger_process / forger_adopted_at.)
--
-- at    2026-10-18T18:41:11.365516629Z
-- ns    ChainDB.AddBlockEvent.AddedToCurrentChain
-- data  { "newSuffixSelectView":
--         { "blockNo":24
--         , "issueNo":0
--         , "issuerHash":"6484e5.."
--         , "kind":"PraosTiebreakerView"
--         , "slotNo":592
--         , "tieBreakVRF":"9a0ee6.."
--         , "weightBoost":0
--         }
--       , "newtip":"bb416f..@592"
--       }
-- (node-44, a peer: peer_process / peer_adopted_at. The two rows differ
-- only by process and timestamp -- which is the point: one trace, and who
-- emitted it decides which pair of columns it lands in. Both come
-- through v_block_adoptions; neither is re-read from ChainDB here.)
--
CREATE OR REPLACE VIEW v_block_adoptions_by_peer AS
WITH
  by_forger AS (
    SELECT f.hash, f.block_no, f.slot_no, f.process, a.ts AS adopted_at
    FROM v_block_forged f
    LEFT JOIN v_block_adoptions a ON a.hash = f.hash AND a.process = f.process),
  by_peer AS (
    SELECT b.hash, b.block_no, b.slot_no, b.process AS forger_process,
           b.adopted_at AS forger_adopted_at,
           a.process AS peer_process, a.ts AS peer_adopted_at
    FROM v_block_adoptions a
    JOIN by_forger b ON b.hash = a.hash AND a.process <> b.process)
SELECT * FROM by_peer
UNION ALL
-- A block no peer adopted: one row, peer columns NULL.
SELECT b.hash, b.block_no, b.slot_no, b.process, b.adopted_at, NULL, NULL
FROM by_forger b
ANTI JOIN by_peer p ON p.hash = b.hash;

COMMENT ON VIEW v_block_adoptions_by_peer IS 'tier 2: every adoption by a peer with the forger''s own adoption alongside, one row per (block, peer); every forged block has a row (previously: block_adoptions_by_peer)';
