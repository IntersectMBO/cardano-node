-- "locli_blockprop_endtoend(...)": locli's end-to-end block propagation, the
-- bpPropagation CDFs of analysis/blockprop.json (cdf0.50 ... cdf1.00), one
-- row per centile. Needs blockprop.sql; filters as described there.
--
--   SELECT * FROM locli_blockprop_endtoend(filters := locli_filter(unitary := true, min_adoptions := 5));
--
-- PURPOSE: cross-checking locli (see blockprop.sql). User content
-- (user_sql/README.md): not reviewed like sql/.
--
-- For each accepted main-chain block, the times from its slot start to each
-- node's first adoption (every node with an entry for the block, the forger
-- included, valid observation or not); the p-centile of those (the sorted
-- value at index min(floor(n * p), n - 1)) is the block's sample of the
-- cdf<p> CDF, p in 0.50, 0.80, 0.90, 0.92, 0.94, 0.96, 0.98, 1.00.
--
-- Fields: as locli_blockprop_forger; metric is "bpPropagation.cdf0.50" etc.
CREATE OR REPLACE MACRO locli_blockprop_endtoend(filters := NULL) AS TABLE
WITH
  c AS MATERIALIZED (SELECT * FROM locli_bp_chain(filters := filters) WHERE accepted),
  a AS (
    SELECT c.hash, list_sort(list((epoch_ns(e.t_adopt) - epoch_ns(c.slot_start)) / 1e9)) AS v
    FROM c JOIN locli_bp_entries() e ON e.hash = c.hash AND e.role <> 'error' AND e.t_adopt IS NOT NULL
    GROUP BY c.hash),
  p AS (
    SELECT hash, pc, CASE WHEN len(v) = 0 THEN 0
                          ELSE v[least(floor(len(v) * pc)::BIGINT, len(v) - 1) + 1] END AS x
    FROM a, (SELECT unnest([0.5, 0.8, 0.9, 0.92, 0.94, 0.96, 0.98, 1.0]) AS pc))
SELECT printf('bpPropagation.cdf%.2f', pc) AS metric, cdf.size AS size, cdf.average AS average, cdf.median AS median,
       cdf.stddev AS stddev, cdf.min AS min, cdf.max AS max, cdf.centiles AS centiles
FROM (SELECT pc, locli_cdf(list(x)) AS cdf FROM p GROUP BY pc)
ORDER BY pc;
