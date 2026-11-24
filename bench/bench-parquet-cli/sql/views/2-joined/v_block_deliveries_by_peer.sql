-- "v_block_deliveries_by_peer": every delivery v_block_deliveries
-- reports, arranged BY PEER — one row per DELIVERY, with the block's own
-- facts beside it. The deliveries themselves are that view's business and
-- are taken from it verbatim; this one adds the forging's facts and names
-- the forger.
-- A (block, peer) PAIR CAN HAVE SEVERAL ROWS, and that is deliberate. A node
-- offered the same body by several peers is handed it several times, and
-- each hand-over happened, so each is a row. Nothing here picks a first,
-- a best or a last. If you want one row per (block, peer), say which
-- delivery you mean and make that choice where you make it -- as
-- v_block_adoptions_timing does.
-- IT MIRRORS "v_eb_deliveries"_by_peer, column for column, because it
-- is the same question asked of the other kind of block: who received its
-- body over the wire, and when. That twin is what to read it against.
-- FETCHING IS NOT ADOPTING, and v_block_adoptions_by_peer is the view that
-- asks the other question of these same blocks -- chain selection, which a
-- ranking block does undergo and an endorser block never does. Holding the
-- bytes is not putting the block on a chain.
-- THERE IS NO FORGER COLUMN, and that is the gain from measuring the wire. A
-- node never fetches a block it forged itself, for either kind of block, so
-- there is nothing forger-shaped to carry and no special case to remember.
-- The kernel's "Consensus.LeiosKernel.BlockAcquired" does fire on the
-- forger, once per ranking block, so a view built on it would need one.
-- EVERY FORGED block HAS A ROW, fetched or not. Filtering one out because
-- nobody took it is not this view's job: a block no peer ever fetched reads as
-- one row with peer_process and peer_fetched_at NULL, which states exactly
-- that. So v_block_forged's blocks and this view's blocks are the
-- same set.
-- ABSENCE IS NOT NON-POSSESSION. A node that already holds a body fetches
-- nothing and so has no row here; the trace cannot distinguish that from a
-- node that never had it, and nothing is inferred either way.
--
-- NO slot_start HERE. A slot start is the instant a slot was SCHEDULED
-- for, and the row below is a delivery, not a forging: its slot is the
-- BLOCK's identity, carried so the row can be read against the forging,
-- not a clock for when it was delivered. v_block_forged and "v_eb_forged"
-- carry slot_start because their ts IS the forging the slot scheduled;
-- join one of them on hash to measure against it.
--
-- (Tier 2.)
--
-- Fields:
-- --------
-- * hash              The block.
--   (NOT NULL)
--
-- * block_no          Its height and slot, both from v_block_forged: the
-- * slot_no           FORGING, not a delivery, so that a block nobody fetched
--   (NOT NULL)        still reports one.
--
-- * forger_process    The node that forged it.
--   (NOT NULL)
--
-- * peer_process      The node that received the body. NULL only on the
--   (NULL)            single row of a block no node ever fetched, as is
--                     peer_fetched_at.
--
-- * peer_fetched_at   When THIS delivery completed, from
--   (NULL)            "BlockFetch.Client.CompletedBlockFetch" verbatim: not the
--                     first such delivery and not a chosen one. NULL only on
--                     the no-peer row.
--
-- Example (in chronological order):
------------------------------------
--
-- at    2026-10-18T18:41:11.276099020Z
-- ns    Forge.Loop.ForgedBlock
-- data  { "block":"bb416f.."
--       , "blockNo":24
--       , "blockPrev":"7a09e9.."
--       , "slot":592
--       }
-- (Via v_block_forged: who forged it and when. The forger never
-- fetches its own block, so it has no row of the kind below.)
--
-- at    2026-10-18T18:41:11.296208081Z
-- ns    BlockFetch.Client.CompletedBlockFetch
-- data  { "block":"bb416f.."
--       , "delay":0.296118713
--       , "peer":{"connectionId":"172.31.10.123:30044 13.238.107.224:30008"}
--       , "size":962
--       }
-- (One row per peer that fetched it: peer_process / peer_fetched_at.)
--
CREATE OR REPLACE VIEW v_block_deliveries_by_peer AS
WITH by_peer AS (
  SELECT b.hash, b.block_no, b.slot_no, b.process AS forger_process,
         d.process AS peer_process, d.ts AS peer_fetched_at
  FROM v_block_deliveries d
  JOIN v_block_forged b ON b.hash = d.hash)
SELECT * FROM by_peer
UNION ALL
-- A block no node ever fetched: one row, peer columns NULL.
SELECT b.hash, b.block_no, b.slot_no, b.process, NULL, NULL
FROM v_block_forged b
ANTI JOIN by_peer p ON p.hash = b.hash;

COMMENT ON VIEW v_block_deliveries_by_peer IS 'tier 2: every block delivery with the forging''s facts, one row per delivery (one with NULL peer for a block nobody fetched): hash, block_no, slot_no, forger_process, peer_process, peer_fetched_at (previously: block_ranking_deliveries_by_peer)';
