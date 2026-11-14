-- Curated view: heap and live data per slot, per node (grounds for a plot:
-- slot on x, memory on y, one series per process).
--
--   SELECT * FROM heap_per_slot;                                        -- all nodes
--   SELECT * FROM heap_per_slot WHERE process = 'node-3';
--   SELECT * FROM heap_per_slot WHERE process IN ('node-0', 'node-7') AND slot BETWEEN 1000 AND 2000;
--
-- One row per (process, slot) with a Resources sample; slot = slot_at(sample time),
-- as locli places Resources (impliedSlot); samples before system start are dropped.
-- Nodes sample about once per slot; where a slot has two samples, the last one
-- counts; slots without a sample (mostly during the pauses of trace_pauses) have no
-- row. Live is what the RTS reported at the sample (updated after every GC, not
-- only major ones). A WHERE on process or slot is pushed down to the Parquet files.
--
-- Columns: process, slot (BIGINT), heap_bytes, live_bytes, rss_bytes (UBIGINT).
CREATE OR REPLACE VIEW heap_per_slot AS
SELECT
    process
  , slot_at("at")                       AS slot
  , arg_max(res_heap(data), "at")       AS heap_bytes
  , arg_max(res_live(data), "at")       AS live_bytes
  , arg_max(res_rss(data),  "at")       AS rss_bytes
FROM raw
WHERE ns = 'Resources'
  AND slot_at("at") >= 0
GROUP BY ALL;
