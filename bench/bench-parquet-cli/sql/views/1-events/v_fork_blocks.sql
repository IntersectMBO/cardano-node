-- "v_fork_blocks": every switch a node made TO a block — one row per (node,
-- block) rollback, with when it began. It reads as a filter on any
-- hash-keyed view,
--   SELECT ... FROM v_block_adoptions
--    WHERE hash NOT IN (SELECT hash FROM v_fork_blocks)
-- and as a record in its own right: which nodes rolled back for which
-- block, and when each of them started.
-- A node's adoption of a block is recorded by exactly one of two ChainDB
-- events naming it as the new tip, AddedToCurrentChain or SwitchedToAFork,
-- never both and never twice, so a block being here means that node
-- reached it by rolling back and no other way.
-- Nothing here parses a hash out of a string. SwitchedToAFork states its
-- tip only as the "hash@slot" text of newtip, but the pipeline around it
-- states the same fact in structured fields: a node considering a fork
-- emits TrySwitchToAFork with data.block.hash, and emits
-- StoreButDontChange with the same field when it decides against
-- switching. What remains is exactly the switches it made: per (node,
-- block), the same set as the tips SwitchedToAFork names.
-- v_fork_switches is the same events seen from the other end, with
-- what each switch gave up and why, and no hash.
-- Known exclusion: a block that entered a node only as an ANCESTOR of a
-- switch is named by no event of its own — a switch names only its new
-- tip — so it is not here unless some node switched to it directly. Such
-- a block leaves a StoreButDontChange of its own on that node and never
-- an adoption, which is how one can be found when it matters.
-- Axiomatic means a property of the event as the protocol defines it; the
-- traces are only the evidence, and where no trace evidences a value the
-- cell is NULL, never inferred.
--
-- (Tier 1.)
--
-- Fields:
-- --------
-- * ts       When the node BEGAN the switch: TrySwitchToAFork's own ts,
--   (NOT NULL) the moment it decided this block was the better chain. The
--            switch completes later, when the blocks have been validated
--            and applied: SwitchedToAFork typically fires a fraction of a
--            second after this, and sometimes seconds. That completion
--            is what v_block_adoptions reports as an adoption time; this is
--            the decision, and the two are not interchangeable.
--
-- * process  The node that switched.
--   (NOT NULL)
--
-- * hash     The block it switched TO.
--   (NOT NULL)
--
-- Example (in chronological order):
------------------------------------
--
-- at    2026-10-18T18:50:04.723729699Z
-- ns    ChainDB.AddBlockEvent.TrySwitchToAFork
-- data  { "block":{ "hash":"99be39.."
--                 , "kind":"Point"
--                 , "slot":1125
--                 }
--       }
-- (The node weighing a switch to a fork it did not have.)
--
-- at    2026-10-18T18:58:45.389850537Z
-- ns    ChainDB.AddBlockEvent.StoreButDontChange
-- data  { "block":{ "hash":"eba728.."
--                 , "kind":"Point"
--                 , "slot":1646
--                 }
--       }
-- (A block stored without changing the selection: a fork kept, not
-- taken. Both traces name a block that is off this node's chain.)
--
CREATE OR REPLACE VIEW v_fork_blocks AS
WITH
  tried AS (
    SELECT process, ts, data->>'$.block.hash' AS hash
    FROM raw WHERE ns = 'ChainDB.AddBlockEvent.TrySwitchToAFork'),
  kept AS (
    SELECT process, data->>'$.block.hash' AS hash
    FROM raw WHERE ns = 'ChainDB.AddBlockEvent.StoreButDontChange')
SELECT min(t.ts) AS ts, t.process, t.hash
FROM tried t
ANTI JOIN kept k ON k.process = t.process AND k.hash = t.hash
GROUP BY t.process, t.hash;

COMMENT ON VIEW v_fork_blocks IS 'tier 1: every block a node switched TO by rolling back, one row per (process, block), at the decision (TrySwitchToAFork): ts, process, hash (previously: fork_blocks)';
