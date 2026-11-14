-- Curated view "slot", ported from Federico's slot.sql (Leios analysis).
--
-- The run's slots as genesis defines them, one row each: the slot number and the
-- instant it began. Genesis arithmetic, not a trace: slot S began at
-- system_start + S * slot_length. The range is the run's: from the first to the
-- last slot any process traced Forge.Loop.StartLeadershipCheck for (every node
-- traces it every slot); that is the only use of the traces here.
--
-- For analysis queries only; locli builds its slot timelines per host from
-- evidence and never from such a frame.
--
-- Columns: slot_no (UBIGINT), slot_start (TIMESTAMP_NS).
CREATE OR REPLACE VIEW slot AS
WITH span AS (
  SELECT min(slot_no(data)) AS lo, max(slot_no(data)) AS hi
  FROM raw WHERE ns = 'Forge.Loop.StartLeadershipCheck'
)
SELECT
    CAST(r.range AS UBIGINT)                                           AS slot_no
  , make_timestamp_ns(  epoch_ns(m.system_start)
                      + r.range * CAST(m.slot_length_s * 1e9 AS BIGINT)) AS slot_start
FROM span, range(CAST(span.lo AS BIGINT), CAST(span.hi AS BIGINT) + 1) r, run_meta m;
