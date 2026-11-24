-- "locli_clusterperf_snapshots(...)": locli's Ledger Snapshots table
-- (clusterperf.snapshots.typ), one row per metric: locli_clusterperf with
-- section := 'snapshots'. Needs blockprop.sql and clusterperf.sql; parameters
-- and definitions are in clusterperf.sql.
--
--   SELECT * FROM locli_clusterperf_snapshots();
--
-- Metrics: cdfSnapTaken, cdfSnapDuration, cdfSpanLensCpuSnap, cdfLiveMajGcDuring,
-- cdfLiveMajGcOutside.
-- User content (user_sql/README.md): not reviewed like sql/.
CREATE OR REPLACE MACRO locli_clusterperf_snapshots(filters := NULL, epoch_length := NULL) AS TABLE
FROM locli_clusterperf(filters := filters, epoch_length := epoch_length,
                       section := 'snapshots');
