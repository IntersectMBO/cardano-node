-- "1. Announcement, against every block forged": of the ranking blocks this run
-- forged, how many announced an endorser block. One row, the section's Total
-- figures.
--
-- USER CONTENT (user_sql/README.md): not reviewed or tested like sql/; the
-- Leios observables aren't final, so it may break with a node change.
--
-- Columns: blocks_forged, announce_an_eb, announce_an_eb_pct, announce_nothing,
-- distinct_ebs_announced.
SELECT
    count(*)                                                        AS blocks_forged
  , count_if(announced_eb_hash IS NOT NULL)                         AS announce_an_eb
  , round(100.0 * count_if(announced_eb_hash IS NOT NULL) / count(*), 1)
                                                                    AS announce_an_eb_pct
  , count_if(announced_eb_hash IS NULL)                             AS announce_nothing
  , count(DISTINCT announced_eb_hash)                               AS distinct_ebs_announced
FROM v_block_forged;
