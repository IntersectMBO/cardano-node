-- "v_block_writes_by_peer": every write v_block_writes reports,
-- arranged BY PEER — one row per write, with the block's own facts beside it
-- and its forger's own write alongside.
-- THE FORGER HAS A TIMING COLUMN HERE, unlike the fetched pivots. A node
-- writes the block it forged without fetching anything, so the forging has a
-- write of its own; it is carried as forger_written_at rather than as a row,
-- the same arrangement v_block_adoptions_by_peer uses for the self-adoption.
-- ONE ROW PER PEER, because a node writes a given block once. The trace is
-- bracketed and the closing row carries no risingEdge; that row is the same
-- write ending, not a second one, so only the opening row is taken. This is
-- a guard on what counts as the event, not a collapse of repeats: were a
-- node ever to write a block twice, both would be rows here.
-- A BLOCK NOBODY WROTE STILL HAS A ROW, peer columns NULL, so every forged
-- block is accounted for. forger_written_at is NULL in turn when the forging
-- was never written by its own node.
-- Axiomatic means a property of the event as the protocol defines it; the
-- traces are only the evidence, and where no trace evidences a value the cell
-- is NULL, never inferred.
--
-- NO slot_start HERE. A slot start is the instant a slot was SCHEDULED
-- for, and the row below is a write, not a forging: its slot is the
-- BLOCK's identity, carried so the row can be read against the forging,
-- not a clock for when it was written. v_block_forged and "v_eb_forged"
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
-- * block_no          Its height and slot, both from its header, via
-- * slot_no           v_block_forged.
--   (NOT NULL)
--
-- * forger_process    The node that forged it.
--   (NOT NULL)
--
-- * forger_written_at When the forger wrote it into its own VolatileDB. NULL
--   (NULL)            when it never did, which is what a discarded forge
--                     leaves behind.
--
-- * peer_process      The other node that wrote it. NULL only on the single
--   (NULL)            row of a block no peer wrote, as is peer_written_at.
--
-- * peer_written_at   When that peer wrote it, from v_block_writes verbatim
--   (NULL)            (the opening row of the write).
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
-- (Via v_block_forged: hash, block_no, slot_no, forger_process.)
--
-- at    2026-10-18T18:41:11.291183493Z
-- ns    ChainDB.AddBlockEvent.AddedBlockToVolatileDB
-- data  { "block":{ "hash":"bb416f.."
--                 , "kind":"Point"
--                 , "slot":592
--                 }
--       , "blockNo":"24"
--       , "risingEdge":1
--       }
-- (node-8, which FORGED it: forger_written_at. No fetch preceded this.)
--
-- at    2026-10-18T18:41:11.296308614Z
-- ns    ChainDB.AddBlockEvent.AddedBlockToVolatileDB
-- data  { "block":{ "hash":"bb416f.."
--                 , "kind":"Point"
--                 , "slot":592
--                 }
--       , "blockNo":"24"
--       , "risingEdge":1
--       }
-- (node-44, a peer: peer_written_at. The two rows differ only by process and
-- timestamp; who emitted it decides which pair of columns it lands in.)
--
CREATE OR REPLACE VIEW v_block_writes_by_peer AS
WITH
  written AS (
    SELECT hash, process, ts FROM v_block_writes WHERE rising_edge),
  by_forger AS (
    SELECT f.hash, f.block_no, f.slot_no, f.process, w.ts AS written_at
    FROM v_block_forged f
    LEFT JOIN written w ON w.hash = f.hash AND w.process = f.process),
  by_peer AS (
    SELECT b.hash, b.block_no, b.slot_no, b.process AS forger_process,
           b.written_at AS forger_written_at,
           w.process AS peer_process, w.ts AS peer_written_at
    FROM written w
    JOIN by_forger b ON b.hash = w.hash AND w.process <> b.process)
SELECT * FROM by_peer
UNION ALL
-- A block no peer wrote: one row, peer columns NULL.
SELECT b.hash, b.block_no, b.slot_no, b.process, b.written_at, NULL, NULL
FROM by_forger b
ANTI JOIN by_peer p ON p.hash = b.hash;

COMMENT ON VIEW v_block_writes_by_peer IS 'tier 2: every block write by a peer with the forger''s own write alongside, one row per peer write (one with NULL peer for a block no peer wrote) (previously: block_ranking_writes_by_peer)';
