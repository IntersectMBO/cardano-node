-- "v_block_timing": the WHOLE LIFE of every forged ranking block in one
-- row per (block, peer) — the forging on the left, and what each other node
-- did with it on the right: announced, delivered, written, adopted.
-- EVERY FORGED BLOCK HAS A ROW, even one no other node ever touched; its peer
-- columns are then NULL. So this view's distinct hashes are v_block_forged's
-- rows exactly, and a block missing from an analysis is a block, not a gap.
-- A PEER IS ANY NODE BUT THE FORGER that did at least ONE of the four things.
-- The stages are independent here: a node can announce a block it never
-- adopted, and each column is NULL on its own evidence, never because another
-- is.
-- NOTHING IS FILTERED. No minimum adoption count, no fork resolved, no
-- winner picked, no block dropped for having lost a height battle. Every
-- forged block gets a row, every node that touched it gets a row, and each
-- event on the path is COUNTED where it happened and TIMED at its first.
-- What those numbers mean is the reader's to decide, not this view's.
-- MIN AND COUNT TOGETHER, because the underlying events repeat. The time is
-- the FIRST of its kind and the count says how many there were, so a reading
-- can use the first moment without losing that there were others. Both are
-- taken here, in the pivot; the axioms below collapse nothing.
-- ONE ANNOUNCEMENT IS TWO TRACE ROWS, SO A COUNT TAKES ONE OF THEM. The
-- ChainSync server brackets each send: a row with risingEdge = 1, then a row
-- without the field about half a millisecond later. Per (block, node,
-- connection) the two are ALWAYS paired -- no group holds a lone row of
-- either kind -- so counting rising_edge counts sends, exactly once each.
-- The write trace brackets the same way. A delivery is a single row and is
-- counted as it stands. v_eb_timing needs none of this: no Leios trace is
-- bracketed, so there one row is one event.
-- Axiomatic means a property of the event as the protocol defines it; the
-- traces are only the evidence, and where no trace evidences a value the cell
-- is NULL, never inferred.
--
-- (Tier 2.)
--
-- Fields:
-- --------
-- * ts                   When it was forged,
--   (NOT NULL)
--
-- * process              the node that forged it,
--   (NOT NULL)
--
-- * hash                 the block, and
--   (NOT NULL)
--
-- * slot_no              its slot: all four from v_block_forged, repeated on
--   (NOT NULL)           every one of the block's rows.
--
-- * slot_start           When the slot named above began, per genesis. The
--   (NOT NULL)           baseline for the forging's own lateness.
--
-- * bytes                The whole serialized block's size. NULL when the
--   (NULL)               forging logged no adoption of its own, which is what
--                        a discarded forge leaves behind.
--
-- * forger_announced_at  When the FORGER first announced it to anyone, and
-- * forger_announcements how many announcements it made. NULL when it never
--   (NULL)               announced the block at all.
--
-- * peer_process         The other node this row is about. NULL only on the
--   (NULL)               single row of a block no other node touched, and then
--                        so is every column below.
--
-- * peer_announced_at    When that peer first announced it, and
-- * peer_announcements   how many announcements it made;
--   (NULL)
--
-- * peer_delivered_at    when the body first arrived at that peer, and
-- * peer_deliveries      how many times it was delivered to it;
--   (NULL)
--
-- * peer_written_at      when it first wrote the body down, and
-- * peer_writes          how many writes it made;
--   (NULL)
--
-- * peer_adopted_at      when it put the block on its own chain. NULL when it
--   (NULL)               never did, which is the ordinary case for the losing
--                        side of a height battle.
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
-- (ts, process, hash, slot_no -- via v_block_forged.)
--
-- at    2026-10-18T18:41:11.296208081Z
-- ns    BlockFetch.Client.CompletedBlockFetch
-- data  { "block":"bb416f.."
--       , "delay":0.296118713
--       , "peer":{"connectionId":"172.31.10.123:30044 13.238.107.224:30008"}
--       , "size":962
--       }
-- (node-44: peer_delivered_at / peer_deliveries)
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
-- (peer_written_at / peer_writes)
--
-- at    2026-10-18T18:41:11.298146298Z
-- ns    ChainSync.ServerHeader.Update
-- data  { "blockingRead":1
--       , "peer":{"connectionId":"172.31.10.123:30044 3.24.127.13:30041"}
--       , "risingEdge":1
--       , "tip":{ "block":"7a09e9.."
--               , "blockNo":23
--               , "slot":536
--               }
--       , "addBlock":"bb416f..@592"
--       }
-- (peer_announced_at / peer_announcements -- the peer relaying it onward.)
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
-- (peer_adopted_at -- the last rung.)
--
CREATE OR REPLACE VIEW v_block_timing AS
WITH
  acts AS (
          SELECT hash, process, ts, 1 AS k FROM v_block_announcements WHERE rising_edge
    UNION ALL SELECT hash, process, ts, 2 FROM v_block_deliveries
    UNION ALL SELECT hash, process, ts, 3 FROM v_block_writes WHERE rising_edge
    UNION ALL SELECT hash, process, ts, 4 FROM v_block_adoptions),
  agg AS (
    SELECT hash, process
         , min(ts) FILTER (WHERE k = 1) AS ann_at, count(*) FILTER (WHERE k = 1) AS ann_n
         , min(ts) FILTER (WHERE k = 2) AS del_at, count(*) FILTER (WHERE k = 2) AS del_n
         , min(ts) FILTER (WHERE k = 3) AS wri_at, count(*) FILTER (WHERE k = 3) AS wri_n
         , min(ts) FILTER (WHERE k = 4) AS ado_at
    FROM acts GROUP BY hash, process),
  forged AS (
    SELECT f.ts, f.process, f.hash, f.slot_no, f.slot_start, f.bytes
         , g.ann_at AS forger_announced_at
         , CASE WHEN g.ann_n > 0 THEN g.ann_n END AS forger_announcements
    FROM v_block_forged f
    LEFT JOIN agg g ON g.hash = f.hash AND g.process = f.process)
SELECT f.*
     , p.process AS peer_process
     , p.ann_at  AS peer_announced_at
     , p.ann_n   AS peer_announcements
     , p.del_at  AS peer_delivered_at
     , p.del_n   AS peer_deliveries
     , p.wri_at  AS peer_written_at
     , p.wri_n   AS peer_writes
     , p.ado_at  AS peer_adopted_at
FROM forged f
JOIN agg p ON p.hash = f.hash AND p.process <> f.process
UNION ALL
-- A block no other node touched: one row, peer columns NULL.
SELECT f.*, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
FROM forged f
WHERE NOT EXISTS (SELECT 1 FROM agg p WHERE p.hash = f.hash AND p.process <> f.process);

COMMENT ON VIEW v_block_timing IS 'tier 2: a block''s whole life per peer (announced, delivered, written, adopted: first time and count), one row per (block, peer); every forged block has a row (previously: block_ranking_timing)';
