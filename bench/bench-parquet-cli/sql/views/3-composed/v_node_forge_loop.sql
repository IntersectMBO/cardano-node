-- "v_node_forge_loop": the forge loop's heartbeat, one row per node --
-- v_slot_leadership_checks summarised. Every node enters every slot and traces
-- "Forge.Loop.StartLeadershipCheck" when it does, so this view says, per
-- node, how many of its slots it entered, how many it never entered at all,
-- and how late it was on the ones it did.
-- BUILT ON v_slot_leadership_checks, which has the row-per-(slot, node) detail
-- including the slots nobody traced. "missed" here is that view's NULLs
-- counted, not arithmetic on a span: count(*) - count(ts). The lateness is
-- subtracted here, not there: that view carries the two instants, this one
-- reads them. Go to it for WHICH slots a node missed and whether the misses
-- ran consecutively.
-- THE WINDOW IS THE NODE'S OWN, first_slot to last_slot, so a node that
-- started late or stopped early is not charged for the slots outside its own
-- life. Nothing is filtered: the window is read off the node's own rows.
-- THE IDEAL START IS STATED, NOT INFERRED. Slot starts come from the
-- genesis timing in meta/run.parquet (system start and slot length), so the
-- delay below is a lateness against a known instant. A run converted
-- without genesis timing has none, and this view fails loudly rather than
-- guessing a baseline.
-- THE DELAY IS NEVER NEGATIVE: no node enters a slot before the slot
-- begins, which is the standing check that the genesis start is right.
-- Axiomatic means a property of the event as the protocol defines it; the
-- traces are only the evidence, and where no trace evidences a value the
-- cell is NULL, never inferred.
--
-- (Tier 3.)
--
-- Fields:
-- --------
-- * process        The node.
--   (NOT NULL)
--
-- * first_at       The earliest and latest timestamps this node traced a
-- * last_at        check at. NOT necessarily the checks for first_slot and
--   (NOT NULL)     last_slot: a badly late check can land after the next
--                  slot's, so the extremes of time and the extremes of slot
--                  can be different events.
--
-- * first_slot     The lowest and highest slot this node traced a check for:
-- * last_slot      the window everything below is computed over.
--   (NOT NULL)
--
-- * slots          How many slots that window spans,
--   (NOT NULL)     last_slot - first_slot + 1.
--
-- * checks         How many of them the node actually traced.
--   (NOT NULL)
--
-- * missed         slots - checks: the slots inside the node's own window
--   (NOT NULL)     that it never entered. 0 is a node that entered every slot
--                  it lived through.
--
-- * delay_min_s    The lateness of the check against its slot's genesis
-- * delay_median_s start, in SECONDS, over that node's checks. The median is
-- * delay_avg_s    the nearest-rank pick (the element at rank floor(n/2) + 1
-- * delay_max_s    of the sorted delays). Read median against avg: a long
--   (NOT NULL)     tail drags the mean well above the middle.
--
-- Example (in chronological order):
------------------------------------
--
-- at    2026-10-23T20:10:16.000959848Z
-- ns    Forge.Loop.StartLeadershipCheck
-- data  { "kind":"TraceStartLeadershipCheck"
--       , "slot":3832
--       }
-- (One entered slot, as v_slot_leadership_checks carries it: that view pairs
-- this with slot 3832's own start, and gives a NULL-ts row for each slot
-- the node never entered. The 959,848 ns of lateness is subtracted here;
-- the misses are counted here.)
--
CREATE OR REPLACE VIEW v_node_forge_loop AS
WITH d AS (
  SELECT process, slot_no, ts, (epoch_ns(ts) - epoch_ns(slot_start)) / 1e9 AS delay
  FROM v_slot_leadership_checks)
SELECT
    process
  , min(ts)                                         AS first_at
  , max(ts)                                         AS last_at
  , min(slot_no)                                    AS first_slot
  , max(slot_no)                                    AS last_slot
  , count(*)                                        AS slots
  , count(ts)                                       AS checks
  , count(*) - count(ts)                            AS missed
  , min(delay)                                      AS delay_min_s
  , list_sort(list(delay) FILTER (WHERE delay IS NOT NULL))
      [CAST(floor(count(delay) * 0.5) AS BIGINT) + 1] AS delay_median_s
  , avg(delay)                                      AS delay_avg_s
  , max(delay)                                      AS delay_max_s
FROM d
GROUP BY process;

COMMENT ON VIEW v_node_forge_loop IS 'tier 3: the forge loop''s heartbeat per node: window (first/last slot), slots entered and missed, entry delay min/median/avg/max in seconds (previously: node_forge_loop)';
