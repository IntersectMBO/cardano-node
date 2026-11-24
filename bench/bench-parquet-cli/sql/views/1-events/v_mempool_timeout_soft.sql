-- "v_mempool_timeout_soft": every SOFT mempool timeout, one row per rejected tx.
-- Tier 1. What soft and hard mean, and why: see v_mempool_timeout_hard.
--
-- A soft timeout is a Mempool.RejectedTx whose errdetails is
-- {"MempoolRejectedByTimeoutSoft": <secs>}; every other rejection has the plain
-- string "MempoolRejectedByLedger". The duration is a JSON number, cast strictly.
--
-- Fields:
-- --------
-- * process        The node whose mempool rejected the tx.
--   (NOT NULL)
--
-- * ts             When.
--   (NOT NULL)
--
-- * txid           The rejected tx.
--
-- * duration_s     The observed validation time, seconds.
--   (NOT NULL)
--
-- * mempool_txs    The mempool when the tx was rejected.
-- * mempool_bytes
CREATE OR REPLACE VIEW v_mempool_timeout_soft AS
SELECT
    process
  , ts
  , data->>'$.tx.txid'                                                       AS txid
  , CAST(data->>'$.errdetails.MempoolRejectedByTimeoutSoft' AS DOUBLE)       AS duration_s
  , mempool_txs(data)                                                        AS mempool_txs
  , mempool_bytes(data)                                                      AS mempool_bytes
FROM raw
WHERE ns = 'Mempool.RejectedTx'
  AND (data->'$.errdetails.MempoolRejectedByTimeoutSoft') IS NOT NULL;

COMMENT ON VIEW v_mempool_timeout_soft IS 'tier 1: soft mempool timeouts (tx rejected), one row per tx: process, ts, txid, duration_s, mempool_txs, mempool_bytes';
