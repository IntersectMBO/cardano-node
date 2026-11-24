-- "chk_missed_slots": every node that missed 1 % or more of the slots in its
-- own window, or 45 or more consecutive slots, one row per node. A check: no row
-- means passed.
--
-- A missed slot is one with no Forge.Loop.StartLeadershipCheck on the node,
-- between its first and last traced slot (v_slot_leadership_checks; a node that
-- started late or stopped early is not charged for the slots outside its own
-- life, chk_node_window states those).
-- TOLERANCE: UNDER 1 % OF THE WINDOW, AND NO RUN OF 45 SLOTS. Garbage collection
-- pauses make every node miss a few slots, in runs of up to about 30 slots; a
-- share alone would hide one long stall, a run length alone a node that keeps
-- missing slots. v_missed_slot_runs has every run.
--
-- Check columns (the same for every check):
-- * check_name   'missed_slots'.
-- * process      The node.
-- * ts           When its longest run of missed slots began, per genesis.
-- * observed     Slots missed of the window, their share, and the longest run.
-- * expected     The tolerance.
CREATE OR REPLACE VIEW chk_missed_slots AS
WITH
  win AS (
    SELECT process, count(*) AS slots, count(*) FILTER (WHERE ts IS NULL) AS missed
    FROM v_slot_leadership_checks
    GROUP BY process),
  longest AS (
    SELECT process, max(slots) AS slots, arg_max(first_slot, slots) AS first_slot,
           arg_max(ts, slots) AS ts
    FROM v_missed_slot_runs
    GROUP BY process)
SELECT
    'missed_slots'                                                        AS check_name
  , w.process
  , l.ts
  , printf('%d of %d slots not entered (%.2f %%), longest run %d slots from slot %d',
           w.missed, w.slots, 100.0 * w.missed / w.slots, l.slots, l.first_slot)
                                                                          AS observed
  , 'under 1 % of the node''s window not entered, and no run of 45 slots or more'
                                                                          AS expected
FROM win w
JOIN longest l USING (process)
WHERE w.missed >= 0.01 * w.slots OR l.slots >= 45;

COMMENT ON VIEW chk_missed_slots IS 'check: one row per node that missed 1 % or more of the slots in its own window, or a run of 45 slots or more; none = passed';
