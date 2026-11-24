-- "locli_clusterperf_txprocessing(...)": locli's Transaction Processing table
-- (clusterperf.txprocessing.typ), one row per metric: locli_clusterperf with
-- section := 'txprocessing'. Needs blockprop.sql and clusterperf.sql; parameters
-- and definitions are in clusterperf.sql.
--
--   SELECT * FROM locli_clusterperf_txprocessing();
--
-- Metrics: cdfTxsProcessed, cdfTxsAccepted, cdfTxsRejected, cdfTxsRejectRatio,
-- cdfMempoolTxs, cdfCpuPerTx, cdfGcPerTx, cdfMutPerTx.
-- User content (user_sql/README.md): not reviewed like sql/.
CREATE OR REPLACE MACRO locli_clusterperf_txprocessing(filters := NULL, epoch_length := NULL) AS TABLE
FROM locli_clusterperf(filters := filters, epoch_length := epoch_length,
                       section := 'txprocessing');
