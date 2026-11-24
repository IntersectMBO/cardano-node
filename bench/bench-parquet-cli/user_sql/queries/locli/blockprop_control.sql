-- "locli_blockprop_control(...)": locli's block propagation control CDFs of
-- analysis/blockprop.json (block size, battles, blocks per host and the
-- ratios), one row per CDF. Needs blockprop.sql; filters as described there.
--
--   SELECT * FROM locli_blockprop_control(filters := locli_filter(unitary := true, min_adoptions := 5));
--
-- PURPOSE: cross-checking locli (see blockprop.sql). User content
-- (user_sql/README.md): not reviewed like sql/.
--
--   cdfBlockSize            accepted main-chain blocks: size (Forge.Loop.AdoptedBlock)
--   cdfSlotBattle           main-chain blocks passing the SLOT filters only:
--                           other blocks forged for the same slot
--   cdfHeightBattle         the same blocks: other blocks at the same height,
--                           forged for another slot
--   cdfBlocksPerHost        per node: blocks it forged
--   cdfBlocksFilteredRatio  per node: its accepted / its main-chain blocks
--                           (nodes without main-chain blocks left out)
--   cdfBlocksChainedRatio   per node: its main-chain / its forged blocks
--                           (nodes that forged nothing left out)
-- Plus the main chain's extent (bpDomainBlocks, bpDomainSlots): rows
-- "domain.blocks_raw" / "domain.blocks_filtered" (size: the block count; min,
-- max: the first and last block number) and "domain.slots_raw" /
-- "domain.slots_filtered" (min, max: the first and last block's slot; size:
-- last minus first slot, which is what locli counts for slots).
--
-- Fields: as locli_blockprop_forger.
CREATE OR REPLACE MACRO locli_blockprop_control(filters := NULL) AS TABLE
WITH
  ch AS MATERIALIZED (FROM locli_bp_chain(filters := filters)),
  hosts AS (
    SELECT p.process,
           count(e.hash)                                     AS total,
           count(e.hash) FILTER (WHERE ch.hash IS NOT NULL)  AS chained,
           count(e.hash) FILTER (WHERE ch.accepted)          AS filtered
    FROM (SELECT DISTINCT process FROM raw) p
    LEFT JOIN locli_bp_entries() e ON e.process = p.process AND e.role = 'forger'
    LEFT JOIN ch ON ch.hash = e.hash
    GROUP BY p.process)
SELECT r.metric AS metric,
       CASE WHEN r.metric LIKE 'domain.slots%' THEN (r.cdf.max - r.cdf.min)::BIGINT ELSE r.cdf.size END AS size, r.cdf.average AS average, r.cdf.median AS median, r.cdf.stddev AS stddev, r.cdf.min AS min, r.cdf.max AS max, r.cdf.centiles AS centiles
FROM (
    SELECT unnest([
        {'metric': 'cdfBlockSize', 'cdf': locli_cdf(list(size) FILTER (WHERE accepted))},
        {'metric': 'cdfSlotBattle', 'cdf': locli_cdf(list(slot_battle::DOUBLE) FILTER (WHERE slot_accepted))},
        {'metric': 'cdfHeightBattle', 'cdf': locli_cdf(list(height_battle::DOUBLE) FILTER (WHERE slot_accepted))}
      ]) AS r
    FROM ch
  UNION ALL
    SELECT unnest([
        {'metric': 'cdfBlocksPerHost', 'cdf': locli_cdf(list(total::DOUBLE))},
        {'metric': 'cdfBlocksFilteredRatio', 'cdf': locli_cdf(list(filtered / chained) FILTER (WHERE chained > 0))},
        {'metric': 'cdfBlocksChainedRatio', 'cdf': locli_cdf(list(chained / total) FILTER (WHERE total > 0))}
      ]) AS r
    FROM hosts
  UNION ALL
    SELECT unnest([
        {'metric': 'domain.blocks_raw',      'cdf': locli_cdf(list(block_no::DOUBLE))},
        {'metric': 'domain.blocks_filtered', 'cdf': locli_cdf(list(block_no::DOUBLE) FILTER (WHERE accepted))},
        {'metric': 'domain.slots_raw',       'cdf': locli_cdf(list(slot_no::DOUBLE))},
        {'metric': 'domain.slots_filtered',  'cdf': locli_cdf(list(slot_no::DOUBLE) FILTER (WHERE accepted))}
      ]) AS r
    FROM ch);
