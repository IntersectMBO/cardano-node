-- "2. Certification, against the announcements every node adopted": of the
-- endorser blocks announced by a block the WHOLE CLUSTER took, how many had
-- their certificate carried by a block the whole cluster also took. One row,
-- the section's Total figures.
--
-- USER CONTENT (user_sql/README.md): not reviewed or tested like sql/; the
-- Leios observables aren't final, so it may break with a node change.
--
-- Columns: walkable_blocks, qualifying_blocks, peers_required, ebs_announced,
-- certificate_carried, carried_pct, never_carried.
WITH
  cluster AS (SELECT (SELECT count(*) FROM files WHERE process LIKE 'node-%') - 1 AS peers_required),
  walk AS MATERIALIZED (SELECT * FROM v_chain_blocks),
  qualifying AS MATERIALIZED (
    SELECT hash, prev_hash, announced_eb_hash, certified_eb_hash
    FROM walk
    WHERE peer_adoptions = (SELECT peers_required FROM cluster)),
  announced AS MATERIALIZED (
    SELECT hash AS rb, announced_eb_hash AS eb
    FROM qualifying WHERE announced_eb_hash IS NOT NULL),
  carried AS MATERIALIZED (
    SELECT DISTINCT a.eb
    FROM announced a
    JOIN qualifying k ON k.prev_hash = a.rb AND k.certified_eb_hash = a.eb)
SELECT
    (SELECT count(*) FROM walk)                   AS walkable_blocks
  , (SELECT count(*) FROM qualifying)             AS qualifying_blocks
  , (SELECT peers_required FROM cluster)          AS peers_required
  , (SELECT count(DISTINCT eb) FROM announced)    AS ebs_announced
  , (SELECT count(*) FROM carried)                AS certificate_carried
  , round(100.0 * (SELECT count(*) FROM carried)
                / (SELECT count(DISTINCT eb) FROM announced), 1)
                                                  AS carried_pct
  , (SELECT count(DISTINCT eb) FROM announced)
    - (SELECT count(*) FROM carried)              AS never_carried;
