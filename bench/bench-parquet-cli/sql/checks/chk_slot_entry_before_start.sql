-- "chk_slot_entry_before_start": every slot a node entered before the slot
-- began per genesis, one row each. A check: no row means passed.
--
-- No node enters a slot before it begins, so the entry delay
-- (Forge.Loop.StartLeadershipCheck's ts minus slot_start) is never negative
-- on a run whose genesis timing is right: this is the standing check that the
-- system start and slot length in meta/run.parquet match the run. A
-- start off by one slot puts every node a slot early or late.
--
-- Check columns (the same for every check):
-- * check_name   'slot_entry_before_start'.
-- * process      The node.
-- * ts           When it entered the slot.
-- * observed     The slot and how early, in seconds.
-- * expected     "entry at or after the slot's start per genesis".
CREATE OR REPLACE VIEW chk_slot_entry_before_start AS
SELECT
    'slot_entry_before_start'                                                  AS check_name
  , process
  , ts
  , printf('slot %d entered %.6f s before it began', slot_no,
           (epoch_ns(slot_start) - epoch_ns(ts)) / 1e9)                         AS observed
  , 'entry at or after the slot''s start per genesis (meta/run.parquet)'        AS expected
FROM v_slot_leadership_checks
WHERE ts < slot_start;

COMMENT ON VIEW chk_slot_entry_before_start IS 'check: one row per slot a node entered before the slot began per genesis (genesis timing wrong); none = passed';
