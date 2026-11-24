-- "votes_funnel": of the endorser blocks announced in the analysed stretch, how
-- many reached quorum, how many had a certificate carried, and for those that
-- did not, what every voter did instead. One row per stage, in the order the
-- stages happen.
--
-- USER CONTENT (user_sql/README.md): not reviewed or tested like sql/; the
-- Leios observables aren't final, so it may break with a node change.
--
-- Columns: ord, stage, unit, count, pct_of_announced, pct_of_group, group_is,
-- votes_min, votes_median, votes_max.
WITH
  ann AS MATERIALIZED (
    SELECT DISTINCT announced_eb_hash AS eb, hash AS rb, slot_no AS rb_slot
    FROM v_block_forged WHERE announced_eb_hash IS NOT NULL),
  ebs AS MATERIALIZED (SELECT DISTINCT eb FROM ann),
  voters AS (SELECT (SELECT count(*) FROM files WHERE process LIKE 'node-%') AS n),
  voted AS MATERIALIZED (
    SELECT process, data->>'$.vote.rbHash' AS rb, CAST(data->>'$.vote.voterId' AS UBIGINT) AS voter
    FROM raw WHERE ns = 'Consensus.LeiosKernel.Voted'),
  not_voted AS MATERIALIZED (
    SELECT process, data->>'$.ebHash' AS eb, data->>'$.reason' AS reason
    FROM raw WHERE ns = 'Consensus.LeiosKernel.NotVoted'),
  quorum AS MATERIALIZED (
    SELECT DISTINCT a.eb
    FROM raw c JOIN ann a ON a.rb = (c.data->>'$.rbHash')
    WHERE c.ns = 'Consensus.LeiosKernel.Certified'),
  carried AS MATERIALIZED (
    SELECT DISTINCT certified_eb_hash AS eb
    FROM v_chain_blocks
    WHERE certified_eb_hash IS NOT NULL AND certified_eb_hash IN (SELECT eb FROM ebs)),
  backing AS MATERIALIZED (
    SELECT e.eb, count(DISTINCT v.voter) AS votes
    FROM ebs e
    LEFT JOIN ann a   ON a.eb = e.eb
    LEFT JOIN voted v ON v.rb = a.rb
    GROUP BY e.eb),
  noq AS MATERIALIZED (SELECT eb FROM ebs WHERE eb NOT IN (SELECT eb FROM quorum)),
  decided AS MATERIALIZED (
      SELECT 'voted for it anyway' AS why, count(*) AS n
      FROM (SELECT DISTINCT v.process, a.eb
            FROM voted v JOIN ann a ON a.rb = v.rb
            WHERE a.eb IN (SELECT eb FROM noq))
    UNION ALL
      SELECT 'declined: ' || reason, count(*)
      FROM (SELECT DISTINCT process, eb, reason FROM not_voted WHERE eb IN (SELECT eb FROM noq))
      GROUP BY reason),
  deciding AS MATERIALIZED (
      SELECT DISTINCT v.process, a.eb
      FROM voted v JOIN ann a ON a.rb = v.rb
      WHERE a.eb IN (SELECT eb FROM noq)
    UNION
      SELECT process, eb FROM not_voted WHERE eb IN (SELECT eb FROM noq)),
  tot AS (
    SELECT
        (SELECT count(*) FROM ebs)                 AS announced
      , (SELECT count(*) FROM quorum)              AS has_quorum
      , (SELECT count(*) FROM carried)             AS carried
      , (SELECT count(*) FROM noq)                 AS no_quorum
      , (SELECT n FROM voters)                     AS voters
      , (SELECT count(DISTINCT data->>'$.ebHash') FROM raw
          WHERE ns = 'Consensus.LeiosKernel.VoteScheduled'
            AND (data->>'$.ebHash') IN (SELECT eb FROM ebs)) AS scheduled
      , (SELECT coalesce(sum(n), 0) FROM decided)  AS decisions
      , (SELECT count(*) FROM deciding)            AS decided_pairs),
  grouped AS MATERIALIZED (
    SELECT CASE WHEN b.eb IN (SELECT eb FROM quorum) THEN 'q' ELSE 'n' END AS grp, b.votes
    FROM backing b),
  span AS (
    SELECT grp, votes
         , row_number() OVER (PARTITION BY grp ORDER BY votes) AS rn
         , count(*)     OVER (PARTITION BY grp)                AS n
    FROM grouped),
  spread AS (
    SELECT grp, min(votes) AS lo, max(CASE WHEN rn = (n + 1) // 2 THEN votes END) AS mid, max(votes) AS hi
    FROM span GROUP BY grp)
SELECT *
FROM (
    SELECT 1 AS ord, 'Endorser blocks announced' AS stage, 'endorser blocks' AS unit
         , announced AS count, 100.0 AS pct_of_announced, NULL::DOUBLE AS pct_of_group
         , NULL::VARCHAR AS group_is, NULL::BIGINT AS votes_min, NULL::BIGINT AS votes_median, NULL::BIGINT AS votes_max
    FROM tot
  UNION ALL
    SELECT 2, 'A voting window opened', 'endorser blocks', scheduled
         , round(100.0 * scheduled / announced, 1), NULL, NULL, NULL, NULL, NULL
    FROM tot
  UNION ALL
    SELECT 3, 'Reached quorum', 'endorser blocks', has_quorum
         , round(100.0 * has_quorum / announced, 1), NULL, NULL
         , (SELECT lo FROM spread WHERE grp = 'q'), (SELECT mid FROM spread WHERE grp = 'q'), (SELECT hi FROM spread WHERE grp = 'q')
    FROM tot
  UNION ALL
    SELECT 4, 'Certificate carried by a later block', 'endorser blocks', carried
         , round(100.0 * carried / announced, 1), round(100.0 * carried / has_quorum, 1), 'reached quorum', NULL, NULL, NULL
    FROM tot
  UNION ALL
    SELECT 5, 'Quorum reached, never carried', 'endorser blocks', has_quorum - carried
         , round(100.0 * (has_quorum - carried) / announced, 1), round(100.0 * (has_quorum - carried) / has_quorum, 1)
         , 'reached quorum', NULL, NULL, NULL
    FROM tot
  UNION ALL
    SELECT 6, 'Never reached quorum', 'endorser blocks', no_quorum
         , round(100.0 * no_quorum / announced, 1), NULL, NULL
         , (SELECT lo FROM spread WHERE grp = 'n'), (SELECT mid FROM spread WHERE grp = 'n'), (SELECT hi FROM spread WHERE grp = 'n')
    FROM tot
  UNION ALL
    SELECT 7, 'voters ' || d.why, 'logged decisions', d.n, NULL
         , round(100.0 * d.n / (SELECT sum(n) FROM decided), 1), 'logged decisions on no-quorum EBs', NULL, NULL, NULL
    FROM decided d)
ORDER BY ord, stage;
