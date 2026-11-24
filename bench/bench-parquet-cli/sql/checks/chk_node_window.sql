-- "chk_node_window": every node whose slot window falls short of the cluster's
-- by more than 60 slots at either end, one row each. A check: no row means
-- passed.
--
-- A node's window is its first to its last slot with a
-- Forge.Loop.StartLeadershipCheck (v_node_forge_loop); the cluster's is the
-- lowest first and the highest last over all nodes. A node that started late,
-- stopped early, or whose log is truncated has a narrower window.
-- TOLERANCE: 60 SLOTS AT EACH END. Nodes of one cluster don't start or stop in
-- the same slot; a few slots to a few tens of slots apart is normal on a cluster
-- run.
--
-- Check columns (the same for every check):
-- * check_name   'node_window'.
-- * process      The node.
-- * ts           NULL (a property of the whole run).
-- * observed     The node's window, and how many slots short it is at each end.
-- * expected     The cluster's window, within the tolerance.
CREATE OR REPLACE VIEW chk_node_window AS
WITH c AS (SELECT min(first_slot) AS lo, max(last_slot) AS hi FROM v_node_forge_loop)
SELECT
    'node_window'                                                               AS check_name
  , n.process
  , CAST(NULL AS TIMESTAMP_NS)                                                  AS ts
  , printf('slots %d-%d (%d late, %d early)', n.first_slot, n.last_slot,
           n.first_slot - c.lo, c.hi - n.last_slot)                             AS observed
  , printf('within 60 slots of %d-%d (the cluster''s window)', c.lo, c.hi)      AS expected
FROM v_node_forge_loop n, c
WHERE n.first_slot - c.lo > 60 OR c.hi - n.last_slot > 60;

COMMENT ON VIEW chk_node_window IS 'check: one row per node whose slot window (first to last slot entered) falls short of the cluster''s by more than 60 slots at either end; none = passed';
