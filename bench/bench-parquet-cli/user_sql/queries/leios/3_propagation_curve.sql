-- "3. Endorser-block propagation, as a curve": for each endorser block, the
-- delay by which it had reached a given SHARE of the peers that ever acquired
-- it. One row per share, the distribution of those per-block delays.
--
-- USER CONTENT (user_sql/README.md): not reviewed or tested like sql/; the
-- Leios observables aren't final, so it may break with a node change.
--
-- Columns: share, samples, p50, p90, p99, max, mean (ms).
WITH
  forging AS MATERIALIZED (
    SELECT hash, min(ts) AS ts, any_value(process) AS forger
    FROM v_eb_forged GROUP BY hash),
  acquired AS MATERIALIZED (
    SELECT hash, process, min(ts) AS ts
    FROM v_eb_deliveries GROUP BY hash, process),
  peer AS MATERIALIZED (
    SELECT eb, ms
         , row_number() OVER (PARTITION BY eb ORDER BY ms) AS rn
         , count(*)     OVER (PARTITION BY eb)             AS takers
    FROM (SELECT a.hash AS eb, (epoch_ns(a.ts) - epoch_ns(g.ts)) / 1e6 AS ms
          FROM acquired a JOIN forging g ON g.hash = a.hash
          WHERE a.process <> g.forger)),
  shares(share) AS (VALUES (0.5), (0.8), (1.0)),
  reached AS MATERIALIZED (
    -- The peer at rank ceil(share * takers): the delay by which that share of
    -- the block's takers had it.
    SELECT s.share, p.ms
    FROM peer p JOIN shares s ON p.rn = CAST(ceil(s.share * p.takers) AS BIGINT)),
  ranked AS (
    SELECT share, ms
         , row_number() OVER (PARTITION BY share ORDER BY ms) AS rn
         , count(*)     OVER (PARTITION BY share)             AS n
    FROM reached)
SELECT
    share
  , n                  AS samples
  , max(CASE WHEN rn = least(CAST(floor(n * 0.50) AS BIGINT), n - 1) + 1 THEN ms END) AS p50
  , max(CASE WHEN rn = least(CAST(floor(n * 0.90) AS BIGINT), n - 1) + 1 THEN ms END) AS p90
  , max(CASE WHEN rn = least(CAST(floor(n * 0.99) AS BIGINT), n - 1) + 1 THEN ms END) AS p99
  , max(ms)            AS max
  , round(avg(ms), 3)  AS mean
FROM ranked
GROUP BY share, n
ORDER BY share;
