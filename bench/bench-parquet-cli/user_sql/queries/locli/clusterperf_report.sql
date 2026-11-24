-- "locli_clusterperf_report(...)": locli's Resource Usage table
-- (clusterperf.report.typ), one row per metric: locli_clusterperf with
-- section := 'report'. Needs blockprop.sql and clusterperf.sql; parameters
-- and definitions are in clusterperf.sql.
--
--   SELECT * FROM locli_clusterperf_report();
--
-- Metrics: mpResourceCDFs.* (CentiCpu, CentiGC, CentiMut, GcsMajor, GcsMinor, RSS,
-- Heap, Live, Alloc, NetRd, NetWr, FsRd, FsWr), cdfStarts, cdfSpanLensCpuEpoch,
-- cdfSpanLensCpuOther.
-- User content (user_sql/README.md): not reviewed like sql/.
CREATE OR REPLACE MACRO locli_clusterperf_report(filters := NULL, epoch_length := NULL) AS TABLE
FROM locli_clusterperf(filters := filters, epoch_length := epoch_length,
                       section := 'report');
