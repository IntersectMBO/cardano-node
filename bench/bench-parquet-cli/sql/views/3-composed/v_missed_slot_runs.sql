-- "v_missed_slot_runs": every run of consecutive slots a node never entered
-- inside its own window, one row per run. Tier 3.
--
-- From v_slot_leadership_checks: a slot with no Forge.Loop.StartLeadershipCheck
-- on a node, between that node's first and last traced slot. A node that started
-- late or stopped early is not charged for the slots outside its own life.
-- CONSECUTIVE MISSED SLOTS ARE ONE ROW: a node that stalled for a minute is one
-- row with its length; v_slot_leadership_checks has the per-slot detail.
--
-- Fields:
-- --------
-- * process      The node.
--   (NOT NULL)
-- * ts           When the run's first slot began, per genesis.
--   (NOT NULL)
-- * first_slot   The run's first slot.
--   (NOT NULL)
-- * last_slot    The run's last slot.
--   (NOT NULL)
-- * slots        Its length in slots.
--   (NOT NULL)
CREATE OR REPLACE VIEW v_missed_slot_runs AS
WITH missed AS (
  SELECT process, slot_no, slot_start,
         slot_no - row_number() OVER (PARTITION BY process ORDER BY slot_no) AS grp
  FROM v_slot_leadership_checks
  WHERE ts IS NULL)
SELECT process, min(slot_start) AS ts, min(slot_no) AS first_slot, max(slot_no) AS last_slot,
       count(*) AS slots
FROM missed
GROUP BY process, grp;

COMMENT ON VIEW v_missed_slot_runs IS 'tier 3: one row per run of consecutive slots a node never entered inside its own window: process, ts, first_slot, last_slot, slots';
