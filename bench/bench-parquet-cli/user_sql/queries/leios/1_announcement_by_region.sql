-- "1. Announcement, by region": 1_announcement_total split by WHO FORGED, one
-- row per node log, every forging counted and nothing filtered.
--
-- USER CONTENT (user_sql/README.md): not reviewed or tested like sql/; the
-- Leios observables aren't final, so it may break with a node change.
--
-- "By region" is BY NODE here: one row per node. The region is an assumption
-- about the deployment: a node's region is its index mod 3 (0 = EU, 1 = US,
-- 2 = AP). That mapping is NOT applied in this query.
--
-- Columns: process, blocks_forged, announce_an_eb, announce_an_eb_pct.
SELECT
    process
  , count(*)                                                        AS blocks_forged
  , count_if(announced_eb_hash IS NOT NULL)                         AS announce_an_eb
  , round(100.0 * count_if(announced_eb_hash IS NOT NULL) / count(*), 1)
                                                                    AS announce_an_eb_pct
FROM v_block_forged
GROUP BY process
ORDER BY process;
