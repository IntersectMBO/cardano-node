-- "2. Certification, by region": 2_certification_total split by WHO ANNOUNCED,
-- one row per node log, over the same population -- the blocks the whole
-- cluster took.
--
-- USER CONTENT (user_sql/README.md): not reviewed or tested like sql/; the
-- Leios observables aren't final, so it may break with a node change.
--
-- "By region" is BY NODE here: one row per node. The region is an assumption
-- about the deployment: a node's region is its index mod 3 (0 = EU, 1 = US,
-- 2 = AP). That mapping is NOT applied in this query.
--
-- Columns: process, ebs_announced, certificate_carried, carried_pct.
WITH
  cluster AS (SELECT (SELECT count(*) FROM files WHERE process LIKE 'node-%') - 1 AS peers_required),
  qualifying AS MATERIALIZED (
    SELECT hash, prev_hash, process, announced_eb_hash, certified_eb_hash
    FROM v_chain_blocks
    WHERE peer_adoptions = (SELECT peers_required FROM cluster)),
  announced AS MATERIALIZED (
    SELECT hash AS rb, process, announced_eb_hash AS eb
    FROM qualifying WHERE announced_eb_hash IS NOT NULL)
SELECT
    a.process
  , count(DISTINCT a.eb)                                           AS ebs_announced
  , count(DISTINCT CASE WHEN k.hash IS NOT NULL THEN a.eb END)     AS certificate_carried
  , round(100.0 * count(DISTINCT CASE WHEN k.hash IS NOT NULL THEN a.eb END)
                / count(DISTINCT a.eb), 1)                         AS carried_pct
FROM announced a
LEFT JOIN qualifying k ON k.prev_hash = a.rb AND k.certified_eb_hash = a.eb
GROUP BY a.process
ORDER BY a.process;
