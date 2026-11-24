-- "chk_unknown_forger": every block some node saw that no node log traced
-- forging, one row per block. A check: no row means passed.
--
-- Seen = the block's hash in ChainSync.Client.DownloadedHeader (header) or
-- BlockFetch.Client.CompletedBlockFetch (body), on any node. Forged =
-- v_block_forged. A block seen but never forged was forged by a node whose log
-- is missing or truncated (or by a node outside the run), so every block of a
-- missing node log shows up here. This is what locli's chain rebuild fails on;
-- the check names the blocks earlier and plainly.
--
-- RELIES ON RECEIVING-SIDE TRACES (ChainSync.Client.DownloadedHeader,
-- BlockFetch.Client.CompletedBlockFetch) besides Forge.Loop.ForgedBlock. Without
-- them nothing is seen and this check passes vacuously; chk_missing_ancestry
-- needs only the forger side.
--
-- Check columns (the same for every check):
-- * check_name   'unknown_forger'.
-- * process      The node that saw the block first.
-- * ts           When it first saw it.
-- * observed     The block and how many nodes saw it.
-- * expected     "a forging of the block in some node log".
CREATE OR REPLACE VIEW chk_unknown_forger AS
WITH seen AS (
  SELECT data->>'$.block' AS hash, process, ts
  FROM raw
  WHERE ns IN ('ChainSync.Client.DownloadedHeader', 'BlockFetch.Client.CompletedBlockFetch'))
SELECT
    'unknown_forger'                                                AS check_name
  , arg_min(s.process, s.ts)                                        AS process
  , min(s.ts)                                                       AS ts
  , printf('block %s seen by %d nodes, forged in no node log', s.hash, count(DISTINCT s.process))
                                                                    AS observed
  , 'a forging of the block in some node log'                       AS expected
FROM seen s
ANTI JOIN v_block_forged f ON f.hash = s.hash
GROUP BY s.hash;

COMMENT ON VIEW chk_unknown_forger IS 'check: one row per block seen (header or body) that no node log traced forging; none = passed';
