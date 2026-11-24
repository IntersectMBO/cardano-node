-- "block_adoptions_with_ancestors": every forged block with every node's
-- adoption of it, TRACED OR INFERRED, one row per (block, adopting node).
--
-- USER CONTENT (user_sql/README.md): not reviewed or tested like sql/.
-- It INFERS adoptions no trace names, which is why it isn't a
-- package view (sql/README.md: NULL, never inferred); v_block_adoptions is the
-- package's traced-only view.
--
-- Two guarantees:
--   1. EVERY forged block is here (the base is Forge.Loop.ForgedBlock, not the
--      adoption events). A block no node ever adopted keeps one row with the
--      adopter columns NULL.
--   2. EVERY adoption is here, traced or not, with HOW (via):
--        AddedToCurrentChain        the block became the node's tip;
--        SwitchedToAFork            the node switched to a fork whose new TIP
--                                   is this block;
--        SwitchedToAFork ancestor   INFERRED: the block entered the node's
--                                   chain in that same switch, as an ancestor
--                                   of the new tip. The event names only the
--                                   tip, so the ancestors are reconstructed by
--                                   walking the tip's prev_hash links back
--                                   while the ancestor has no explicit
--                                   adoption on that node by switch time.
-- Where a block gathers several candidate adoptions on one node, the EARLIEST
-- wins (and its via).
--
-- Columns: hash, slot_no, forged_by, adopted_by (process names), adopted_at
--   (TIMESTAMP_NS), s_after_forge (seconds, 3 decimals), via.
CREATE OR REPLACE VIEW block_adoptions_with_ancestors AS
WITH RECURSIVE
  fb AS MATERIALIZED (
    SELECT process AS forger, hash, prev_hash AS prev, slot_no AS slot, ts AS forged_at
    FROM v_block_forged),
  explicit AS MATERIALIZED (
    SELECT split_part(data->>'$.newtip', '@', 1) AS hash, process, ts
         , CASE ns WHEN 'ChainDB.AddBlockEvent.AddedToCurrentChain' THEN 'AddedToCurrentChain'
                   ELSE 'SwitchedToAFork' END AS via
    FROM raw
    WHERE ns IN ('ChainDB.AddBlockEvent.AddedToCurrentChain', 'ChainDB.AddBlockEvent.SwitchedToAFork')
      AND strpos(data->>'$.newtip', '@') = 65),
  implied(process, ts, hash) AS (
      SELECT e.process, e.ts, f.prev
      FROM explicit e JOIN fb f ON f.hash = e.hash
      WHERE e.via = 'SwitchedToAFork'
    UNION
      SELECT i.process, i.ts, f.prev
      FROM implied i JOIN fb f ON f.hash = i.hash
      WHERE NOT EXISTS (SELECT 1 FROM explicit e
                        WHERE e.hash = i.hash AND e.process = i.process AND e.ts <= i.ts)),
  adoption AS (
    SELECT hash, process, min(ts) AS ts, arg_min(via, ts) AS via
    FROM (    SELECT hash, process, ts, via FROM explicit
    UNION ALL SELECT i.hash, i.process, i.ts, 'SwitchedToAFork ancestor'
              FROM implied i
              WHERE i.hash IN (SELECT hash FROM fb)
                AND NOT EXISTS (SELECT 1 FROM explicit e
                                WHERE e.hash = i.hash AND e.process = i.process AND e.ts <= i.ts))
    GROUP BY hash, process)
SELECT
    b.hash
  , b.slot                                                   AS slot_no
  , b.forger                                                 AS forged_by
  , a.process                                                AS adopted_by
  , a.ts                                                     AS adopted_at
  , round((epoch_ns(a.ts) - epoch_ns(b.forged_at)) / 1e9, 3) AS s_after_forge
  , a.via
FROM fb b
LEFT JOIN adoption a ON a.hash = b.hash;
