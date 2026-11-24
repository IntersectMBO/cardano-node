-- "v_chain_blocks": THE WALK — every forged ranking block that the walk can
-- REACH, one row per block, carrying that block's own facts from
-- v_block_forged, how many nodes built ON it, and how much of the
-- cluster ended up HOLDING it from v_block_adoptions_by_peer. A chain here is
-- what the headers themselves say it is: prev_hash is the walk's only edge.
-- No fork trace and no node's opinion decides membership.
-- WALKABLE IS THE ONLY MEMBERSHIP TEST. A block is here when its whole
-- ancestry is present: its prev_hash names a forged block that is itself
-- walkable, down to the run's first block — the lowest block_no any header
-- states, whose own predecessor is genesis and genesis is forged by nobody.
-- Nothing else is required of a block. A block NO ONE adopted is here, a
-- block no one built on is here, a block its own forger discarded is here:
-- each of them was forged onto a chain, so each of them is walkable, and
-- children and peer_adoptions are what say how far it then got.
-- WHAT IS NOT HERE: a forging whose ancestry the logs do not contain, which
-- means some block in its prev_hash line was never traced. Then the walk
-- from it stops short of the run's first block and it is left out rather
-- than attached to a chain nobody can see. When the logs contain every
-- ancestor this excludes nothing and the row count equals
-- v_block_forged's; a short count is the signal that a node's log is
-- missing or truncated.
-- A CHAIN PARTITION IS HERE WHOLE, and so is every fork. Nothing picks a
-- winner — there is no "longest walk wins" step, because that step is
-- exactly what would hide the losing side. Read the shape off the two
-- counts: children = 0 is a block nobody extended, children >= 2 is the
-- height where the cluster disagreed, and peer_adoptions says how much of
-- the cluster was holding each side while it did.
--
-- THE FIRST NINE FIELDS ARE v_block_forged's OWN COLUMNS under their
-- own names, verbatim; that view's header states which trace evidences each.
-- The last two are counts of rows — of forgings, and of adoptions — and
-- nothing else.
--
-- (Tier 3.)
--
-- Fields:
-- --------
-- * ts                 When the block was forged,
--   (NOT NULL)
--
-- * process            the node that forged it, and
--   (NOT NULL)
--
-- * hash               the block, as v_block_forged states them.
--   (NOT NULL)
--
-- * prev_hash          The predecessor its header names: the walk's edge. It
--   (NOT NULL)         is a row here too, EXCEPT for the run's first block,
--                      whose predecessor is genesis, and genesis is forged by
--                      nobody.
--
-- * block_no           Its height, from its header. NOT unique: every rival at
--   (NOT NULL)         a contested height has its own row.
--
-- * slot_no            Its slot, from its header.
--   (NOT NULL)
--
-- * slot_start         When that slot began, per genesis: the baseline the
--   (NOT NULL)         forging's own lateness.
--
-- * bytes              The whole serialized block's size. NULL when the
--   (NULL)             forging logged no Forge.Loop.AdoptedBlock, which is
--                      what a forger that discarded its own block leaves
--                      behind (DidntAdoptBlock).
--
-- * announced_eb_hash  The endorser block announced in the header (Leios).
--   (NULL)             NULL when the header announces none.
--
-- * certified_eb_hash  The endorser block the body's certificate certifies
--   (NULL)             (Leios). NULL when the block carries no certificate.
--
-- * children           How many forgings name THIS block as their
--   (NOT NULL)         predecessor: how many nodes built on it. It is the
--                      chain membership test for anyone who wants one:
--                        0  nobody extended it. Either a chain's last block
--                           when the log ended, still waiting for a
--                           successor, or a block the cluster abandoned; the
--                           traces cannot tell those two apart, so neither is
--                           called the other;
--                        1  the ordinary case, one successor;
--                        >1 a FORK POINT: the block two or more nodes built
--                           on at once, which is where a height battle starts.
--                      "children > 0" is exactly the main chain minus its
--                      run-end tip, which is the population locli analyses.
--
-- * peer_adoptions     The block's WEIGHT: how many nodes OTHER than its
--   (NOT NULL)         forger put it on their own chain, counted as rows in
--                      v_block_adoptions_by_peer, so by whichever path each
--                      took it in. The forger itself is never counted, so a
--                      block the whole cluster holds reads as the cluster
--                      size minus one. 0 is a value, not a gap: no peer
--                      ever adopted the block.
--
-- Example (in chronological order):
------------------------------------
--
-- at    2026-10-18T18:41:11.003262335Z
-- ns    Consensus.LeiosKernel.BlockCertified
-- data  { "atSlot":592
--       , "ebHash":"e10e32.."
--       , "ebSlot":536
--       }
-- (certified_eb_hash -- a DIFFERENT endorser block, the one this
-- block's body certifies rather than the one its header announces.)
--
-- at    2026-10-18T18:41:11.276065669Z
-- ns    Consensus.LeiosKernel.BlockAnnounced
-- data  { "ebHash":"0b302b.."
--       , "ebSlot":592
--       , "rbHash":"bb416f.."
--       }
-- (announced_eb_hash)
--
-- at    2026-10-18T18:41:11.276099020Z
-- ns    Forge.Loop.ForgedBlock
-- data  { "block":"bb416f.."
--       , "blockNo":24
--       , "blockPrev":"7a09e9.."
--       , "slot":592
--       }
-- (ts, process, hash, prev_hash, block_no, slot_no; prev_hash is the
-- walk's only edge and children counts the rows naming this hash.)
--
-- at    2026-10-18T18:41:11.364308439Z
-- ns    Forge.Loop.AdoptedBlock
-- data  { "blockHash":"bb416f.."
--       , "blockSize":962
--       , "slot":592
--       }
-- (bytes. Absent when a forger discarded its own block, which is why
-- bytes is NULL there.)
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
-- (One of the adoptions that peer_adoptions counts, via
-- v_block_adoptions_by_peer.)
--
CREATE OR REPLACE VIEW v_chain_blocks AS
WITH RECURSIVE
  -- The edges, computed once: without MATERIALIZED, DuckDB re-evaluates
  -- v_block_forged (raw scan and joins) on every step of the walk.
  edges AS MATERIALIZED (
    SELECT hash, prev_hash, block_no FROM v_block_forged),
  -- The walk: from the run's first block (lowest block_no any header states)
  -- along prev_hash. Each block has exactly one predecessor, so every block is
  -- reached at most once and UNION ALL suffices.
  walkable(hash) AS (
      SELECT hash FROM edges WHERE block_no = (SELECT min(block_no) FROM edges)
    UNION ALL
      SELECT e.hash FROM edges e JOIN walkable w ON e.prev_hash = w.hash),
  children AS (
    SELECT prev_hash AS hash, count(*) AS children
    FROM v_block_forged GROUP BY prev_hash),
  adoptions AS (
    SELECT hash, count(peer_process) AS peer_adoptions
    FROM v_block_adoptions_by_peer GROUP BY hash)
SELECT
    b.ts, b.process, b.hash, b.prev_hash, b.block_no, b.slot_no, b.slot_start
  , b.bytes, b.announced_eb_hash, b.certified_eb_hash
  , coalesce(c.children, 0)  AS children
  , a.peer_adoptions         AS peer_adoptions
FROM v_block_forged b
SEMI JOIN walkable w ON w.hash = b.hash
LEFT JOIN children  c ON c.hash = b.hash
LEFT JOIN adoptions a ON a.hash = b.hash;

COMMENT ON VIEW v_chain_blocks IS 'tier 3: every forged block whose whole prev_hash ancestry is in the logs (the walk), with children (blocks built on it) and peer_adoptions (previously: chain_blocks)';
