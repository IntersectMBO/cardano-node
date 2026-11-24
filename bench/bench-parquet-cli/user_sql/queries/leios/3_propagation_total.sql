-- "3. Endorser-block propagation, total": from an endorser block's own forging
-- to each OTHER node's first acquisition of its body. One row: how much was
-- delivered, and the distribution of the peer delay.
--
-- USER CONTENT (user_sql/README.md): not reviewed or tested like sql/; the
-- Leios observables aren't final, so it may break with a node change.
--
-- reached_all_nodes counts EBs delivered to every node BUT ITS FORGER
-- (cluster_size - 1 takers): it reads wire deliveries (v_eb_deliveries),
-- which a forger never has.
--
-- Columns: ebs_delivered, acquisitions, reached_all_nodes, cluster_size,
-- peer_samples, p50, p90, p99, max, mean (ms).
WITH
  cluster AS (SELECT (SELECT count(*) FROM files WHERE process LIKE 'node-%') AS nodes),
  forging AS MATERIALIZED (
    SELECT hash, min(ts) AS ts, any_value(process) AS forger
    FROM v_eb_forged GROUP BY hash),
  acquired AS MATERIALIZED (
    SELECT hash, process, min(ts) AS ts
    FROM v_eb_deliveries GROUP BY hash, process),
  acquisition AS MATERIALIZED (
    SELECT a.hash AS eb, a.process AS taker, g.forger, (epoch_ns(a.ts) - epoch_ns(g.ts)) / 1e6 AS ms
    FROM acquired a JOIN forging g ON g.hash = a.hash),
  ranked AS (
    SELECT ms, row_number() OVER (ORDER BY ms) AS rn, count(*) OVER () AS n
    FROM acquisition WHERE taker <> forger)
SELECT
    (SELECT count(DISTINCT eb) FROM acquisition)        AS ebs_delivered
  , (SELECT count(*) FROM acquisition)                  AS acquisitions
  , (SELECT count(*) FROM (
       SELECT eb FROM acquisition GROUP BY eb
       HAVING count(DISTINCT taker) = (SELECT nodes FROM cluster) - 1))
                                                        AS reached_all_nodes
  , (SELECT nodes FROM cluster)                         AS cluster_size
  , n                                                   AS peer_samples
  , max(CASE WHEN rn = least(CAST(floor(n * 0.50) AS BIGINT), n - 1) + 1 THEN ms END) AS p50
  , max(CASE WHEN rn = least(CAST(floor(n * 0.90) AS BIGINT), n - 1) + 1 THEN ms END) AS p90
  , max(CASE WHEN rn = least(CAST(floor(n * 0.99) AS BIGINT), n - 1) + 1 THEN ms END) AS p99
  , max(ms)                                             AS max
  , round(avg(ms), 3)                                   AS mean
FROM ranked
GROUP BY n;
