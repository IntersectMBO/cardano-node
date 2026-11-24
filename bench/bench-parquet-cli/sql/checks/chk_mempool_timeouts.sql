-- "chk_mempool_timeouts": every mempool timeout of the run, one row each. A
-- check: no row means passed.
--
-- A timeout is the node rejecting a tx (soft) or tearing down the connection
-- it came from (hard) because validating it took too long; see
-- v_mempool_timeout_hard. Healthy benchmark runs have none.
--
-- Check columns (the same for every check):
-- * check_name   'mempool_timeouts'.
-- * process      The node.
-- * ts           When.
-- * observed     What happened, e.g. "hard timeout after 3.90 s".
-- * expected     "no mempool timeout".
CREATE OR REPLACE VIEW chk_mempool_timeouts AS
SELECT
    'mempool_timeouts'                                                        AS check_name
  , process
  , ts
  , 'hard timeout after ' || coalesce(printf('%.2f s', duration_s), 'an unreadable duration')
                                                                              AS observed
  , 'no mempool timeout'                                                      AS expected
FROM v_mempool_timeout_hard
UNION ALL
SELECT
    'mempool_timeouts'
  , process
  , ts
  , printf('soft timeout after %.2f s (tx %s)', duration_s, txid)
  , 'no mempool timeout'
FROM v_mempool_timeout_soft;

COMMENT ON VIEW chk_mempool_timeouts IS 'check: one row per mempool timeout (hard or soft); none = passed';
