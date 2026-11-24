-- "Endorser-block diffusion against the announcing block's chain fate": one row
-- per (endorser block, announcing ranking block), carrying how far the EB got
-- and how far that block got, side by side.
--
-- USER CONTENT (user_sql/README.md): not reviewed or tested like sql/; the
-- Leios observables aren't final, so it may break with a node change.
--
-- Columns: eb, eb_slot_no, forgings, forger_process, peers_reached, takers, rb,
-- rb_block_no, rb_slot_no, rb_adoptions.
WITH
  cluster AS (SELECT (SELECT count(*) FROM files WHERE process LIKE 'node-%') - 1 AS takers),
  forged AS MATERIALIZED (
    SELECT hash, any_value(process) AS process
         , CASE WHEN count(*) = 1 THEN min(slot_no) END AS slot_no
         , count(*) AS forgings
    FROM v_eb_forged GROUP BY hash),
  reach AS MATERIALIZED (
    SELECT hash, count(DISTINCT peer_process) AS peers_reached
    FROM v_eb_deliveries_by_peer GROUP BY hash),
  announcing AS MATERIALIZED (
    SELECT announced_eb_hash AS eb, hash AS rb, block_no AS rb_block_no
         , slot_no AS rb_slot_no, peer_adoptions AS rb_adoptions
    FROM v_chain_blocks WHERE announced_eb_hash IS NOT NULL)
SELECT
    f.hash                        AS eb
  , f.slot_no                     AS eb_slot_no
  , f.forgings
  , f.process                     AS forger_process
  , r.peers_reached
  , (SELECT takers FROM cluster)  AS takers
  , a.rb
  , a.rb_block_no
  , a.rb_slot_no
  , a.rb_adoptions
FROM forged f
LEFT JOIN reach      r ON r.hash = f.hash
LEFT JOIN announcing a ON a.eb   = f.hash
ORDER BY a.rb_block_no, a.rb_slot_no, f.hash;
