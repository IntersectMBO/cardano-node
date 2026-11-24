-- "chk_missing_ancestry": every place where the forged blocks' prev_hash chain
-- breaks, one row per forged block whose predecessor no log traced forging. A
-- check: no row means passed.
--
-- ONLY THE BREAK IS REPORTED, not everything above it: the blocks built on top
-- of a missing one are unreachable too (v_chain_blocks leaves them all out), but
-- they add no information. The run's first block (the lowest block_no) is exempt:
-- its predecessor is genesis, which nobody forges.
--
-- RELIES ON FORGER-SIDE TRACES ONLY: Forge.Loop.ForgedBlock (via v_block_forged).
-- A missing or truncated node log breaks the chain wherever another node built
-- on one of its blocks. chk_unknown_forger reports the same cause from the
-- receiving side and also catches blocks nobody built on (fork tips, the run's
-- last blocks); this check still works when the receiving-side traces aren't
-- there.
--
-- Check columns (the same for every check):
-- * check_name   'missing_ancestry'.
-- * process      The node that forged the block whose predecessor is missing.
-- * ts           When it forged it.
-- * observed     The block, its height and slot, and the missing predecessor.
-- * expected     "the predecessor's forging in some node log".
CREATE OR REPLACE VIEW chk_missing_ancestry AS
SELECT
    'missing_ancestry'                                                          AS check_name
  , f.process
  , f.ts
  , printf('block %s (block_no %d, slot %d) builds on %s, forged in no node log',
           f.hash, f.block_no, f.slot_no, f.prev_hash)                          AS observed
  , 'the predecessor''s forging in some node log'                               AS expected
FROM v_block_forged f
ANTI JOIN v_block_forged p ON p.hash = f.prev_hash
WHERE f.block_no > (SELECT min(block_no) FROM v_block_forged);

COMMENT ON VIEW chk_missing_ancestry IS 'check: one row per forged block whose predecessor no node log traced forging (where the prev_hash chain breaks); none = passed';
