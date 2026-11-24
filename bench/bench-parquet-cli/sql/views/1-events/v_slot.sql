-- "v_slot": the run's slots as GENESIS defines them, one row each -- a slot
-- number and the instant it began, and nothing else. Tier 1.
--
-- GENESIS ARITHMETIC, NOT A TRACE. Slot S began at system_start + S *
-- slot_length (slot_start(S), session.sql). The traces only bound the range:
-- from the first to the last slot any node traced
-- Forge.Loop.StartLeadershipCheck for (every node traces it every slot; older
-- nodes trace Forge.Loop.StartLeadershipCheckPlus instead, read the same).
-- Views that need one slot's start call slot_start() directly; this view is for
-- queries that need the whole grid, e.g. to find slots nobody entered.
--
-- For analysis queries only; locli builds its slot timelines per host from
-- evidence and never from such a frame.
--
-- Fields:
-- --------
-- * slot_no      The slot number.
--   (NOT NULL)
--
-- * slot_start   When it began, per genesis.
--   (NOT NULL)
CREATE OR REPLACE VIEW v_slot AS
WITH span AS (
  SELECT min(slot_no(data)) AS lo, max(slot_no(data)) AS hi
  FROM raw WHERE ns IN ('Forge.Loop.StartLeadershipCheck', 'Forge.Loop.StartLeadershipCheckPlus')
)
SELECT
    CAST(r.range AS UBIGINT)  AS slot_no
  , slot_start(r.range)       AS slot_start
FROM span, range(CAST(span.lo AS BIGINT), CAST(span.hi AS BIGINT) + 1) r;

COMMENT ON VIEW v_slot IS 'tier 1: the run''s slots per genesis, one row each: slot_no, slot_start (previously: slot)';
