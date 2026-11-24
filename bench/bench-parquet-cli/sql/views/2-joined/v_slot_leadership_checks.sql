-- "v_slot_leadership_checks": every slot of every node's life, whether the node
-- entered it or not -- one row per (slot, node), with the slot's own start
-- beside the instant that node entered it, or NULL where it never did.
-- THE SLOTS COME FROM "v_slot", THE ATTENDANCE FROM THE TRACE. A node that
-- skipped a slot traced nothing for it, so a view built only on
-- "Forge.Loop.StartLeadershipCheck" cannot show the skip at all: it would
-- have no row. Joining the trace onto "v_slot" instead gives a row for every
-- slot either way, and a skipped one reads as a NULL ts -- the only way to
-- say WHICH slots a node missed rather than merely how many.
-- THE WINDOW IS EACH NODE'S OWN, its first traced slot to its last, so a node
-- that started late or stopped early is not charged for the slots outside its
-- own life. Nothing is filtered: the window is read off that node's own rows.
-- (process, slot_no) IS THE KEY. A node traces the check at most once per slot,
-- so the join adds no rows and the row count is exactly the sum of the nodes'
-- window lengths.
-- ts IS NEVER BEFORE slot_start: no node enters a slot before the slot
-- begins. That is the standing check that genesis start in meta/run.parquet
-- is right -- a start off by one slot would put every reading a second out,
-- and a node running late would stand out against the others on that slot.
-- NO LATENESS COLUMN. ts minus slot_start is a READING, and a reading is not
-- evidence: this view carries the two instants the run states and leaves the
-- subtraction to whoever wants it. v_node_forge_loop does it.
-- Axiomatic means a property of the event as the protocol defines it; the
-- traces are only the evidence, and where no trace evidences a value the cell
-- is NULL, never inferred. The NULLs here are the point of the view.
--
-- (Tier 2.)
--
-- Fields:
-- --------
-- * slot_no     The slot, from v_slot. Every slot in the node's window
--   (NOT NULL)  appears, traced or not.
--
-- * slot_start  When that slot began, per genesis, so it holds whether or not
--   (NOT NULL)  anything was traced in the slot.
--
-- * process     The node.
--   (NOT NULL)
--
-- * ts          When that node entered that slot
--   (NULL)      (Forge.Loop.StartLeadershipCheck). NULL IS THE MEASUREMENT:
--               it says the node never entered the slot at all.
--
-- Example (in chronological order):
------------------------------------
--
-- at    2026-10-23T20:10:16.000959848Z
-- ns    Forge.Loop.StartLeadershipCheck
-- data  { "kind":"TraceStartLeadershipCheck"
--       , "slot":3832
--       }
-- (One entered slot: process, slot_no and ts. v_slot pairs it with slot 3832's
-- own start of 1792786216000000000 -- so this node entered the slot 959,848
-- ns after it began, a reading this view leaves to whoever wants it. A slot
-- the node never entered has the same slot_no and slot_start and a NULL ts:
-- there is no trace to show for it, which is why the row has to come from
-- v_slot rather than from the trace.)
--
CREATE OR REPLACE VIEW v_slot_leadership_checks AS
WITH
  checks AS (
    SELECT process, slot_no(data) AS slot_no, ts
    FROM raw WHERE ns IN ('Forge.Loop.StartLeadershipCheck', 'Forge.Loop.StartLeadershipCheckPlus')),
  windows AS (
    SELECT process, min(slot_no) AS lo, max(slot_no) AS hi
    FROM checks GROUP BY process)
SELECT s.slot_no, s.slot_start, w.process, c.ts
FROM windows w
JOIN v_slot s ON s.slot_no BETWEEN w.lo AND w.hi
LEFT JOIN checks c ON c.process = w.process AND c.slot_no = s.slot_no;

COMMENT ON VIEW v_slot_leadership_checks IS 'tier 2: every slot of every node''s window, one row per (slot, process): slot_no, slot_start, process, ts (NULL: the node never entered the slot) (previously: slot_leadership_checks)';
