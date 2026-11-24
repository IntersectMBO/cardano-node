-- "v_block_writes": the AXIOMATIC view of every WRITE of a ranking
-- block's body into a node's own VolatileDB, one row per trace line — which
-- node put which block on its own disk, and when.
-- IT IS THE FOURTH STAGE, after forged, announced and fetched. A block is
-- forged, its header is announced to peers, its body is fetched by them, and
-- THEN each node writes that body down. This view is that write. It says the
-- node HOLDS the block; it does not say the node chose it. Choosing is adoption
-- and lives in "v_block_adoptions".
-- THE FORGER IS HERE TOO, unlike v_block_deliveries. A node writes
-- the block
-- it forged into its own VolatileDB without fetching anything, so the forger
-- has a row here and none there, for very nearly every forging. The shortfall
-- is the forgings whose own node never wrote them, which is what a discarded
-- forge leaves behind.
-- NOTHING IS WRITTEN THAT WAS NOT FETCHED OR FORGED. A row belongs to a node
-- that fetched the block or forged it; a body gets here by exactly those two
-- paths.
-- IT FOLLOWS THE FETCH ALMOST IMMEDIATELY, typically well under a millisecond
-- after "BlockFetch.Client.CompletedBlockFetch". The two are nearly the same
-- instant, and a reading that needs "when did this node come to hold the body"
-- can use either.
-- A FEW WRITES PRECEDE THEIR OWN FETCH TRACE, which is pipelining rather than a
-- contradiction: the write is started from the arriving body before the fetch
-- client has finished accounting for it. The fetch row still EXISTS in each of
-- those cases; it simply carries the later timestamp. Nothing here reorders
-- them; both are reported as traced.
-- A SECOND ROW IS A FALLING EDGE, NOT A SECOND WRITE. The trace is bracketed
-- like "ChainSync.ServerHeader.Update": a row with risingEdge = 1 opens the
-- write and a row with NO risingEdge closes it. A (node, block) pair has either
-- a SINGLE row, the rising one, or a rising row followed by a falling one a
-- fraction of a millisecond later; never the falling row first, a falling row
-- alone, or more than two rows. So a node never writes the same block twice: it
-- is one write, traced twice where the closing bracket is emitted.
-- TO COUNT WRITES, TAKE rising_edge, which is every write exactly once.
-- Taking the earliest row per (node, block) gives the same answer, since the
-- rising row always comes first. This view collapses nothing: it carries both
-- rows because both were traced, and the reading is the caller's.
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
-- (Tier 1.)
--
-- Fields:
-- --------
-- * ts          When the write was traced. On the rising row this is when
--   (NOT NULL)  the node came to hold the body; on a falling row it is when
--               the write closed.
--
-- * process     The node that wrote it. The forger appears here for its own
--   (NOT NULL)  block.
--
-- * hash        The block written, from data.block.hash.
--   (NOT NULL)
--
-- * slot_no     Its slot, from data.block.slot.
--   (NOT NULL)
--
-- * block_no    Its height, from data.blockNo. The trace states it as a
--   (NOT NULL)  STRING, "24" and not 24; the cast is strict.
--
-- * rising_edge true on the row that OPENS the write, NULL on the row that
--   (NULL)      closes it (the field is absent there). Most writes have only
--               the opening row; see above for the shape. Filter on
--               rising_edge to count each write once.
--
-- Example (in chronological order):
------------------------------------
--
-- at    2026-10-18T18:41:11.296208081Z
-- ns    BlockFetch.Client.CompletedBlockFetch
-- data  { "block":"bb416f.."
--       , "delay":0.296118713
--       , "peer":{"connectionId":"172.31.10.123:30044 13.238.107.224:30008"}
--       , "size":962
--       }
-- (Not read here -- v_block_deliveries's trace, shown because the store
-- below is the same body, 0.1 ms later, on the same node.)
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
--
CREATE OR REPLACE VIEW v_block_writes AS
SELECT
    ts                                         AS ts
  , process                                    AS process
  , data->>'$.block.hash'                      AS hash
  , CAST(data->>'$.block.slot' AS UBIGINT)     AS slot_no
  , CAST(data->>'$.blockNo' AS UBIGINT)        AS block_no
  , CAST(data->>'$.risingEdge' AS BOOLEAN)     AS rising_edge
FROM raw
WHERE ns = 'ChainDB.AddBlockEvent.AddedBlockToVolatileDB';

COMMENT ON VIEW v_block_writes IS 'tier 1: every write of a block body into a node''s VolatileDB, one row per trace line (rising_edge opens it): ts, process, hash, slot_no, block_no (previously: block_ranking_writes)';
