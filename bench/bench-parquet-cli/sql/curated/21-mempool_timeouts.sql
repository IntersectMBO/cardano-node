-- Curated views: mempool timeouts, one relation per kind (one row per event).
--
-- A tx whose validation in the mempool takes too long (ouroboros-consensus,
-- Mempool/Update.hs, addTx):
--   * soft: validation finished, but took longer than mempoolTimeoutSoft (any
--     client), or was cut off at mempoolTimeoutHard for a local client. The tx
--     is rejected and traced as Mempool.RejectedTx with errdetails
--     {"MempoolRejectedByTimeoutSoft": <secs>}; errdetails is the plain string
--     "MempoolRejectedByLedger" for every other rejection. From Conway on.
--   * hard: validation cut off at mempoolTimeoutHard for a remote peer (or
--     before Conway): addTx throws ExnMempoolTimeout and the node tears down the
--     connection the tx came from. The mempool traces nothing; only the network
--     layer does, with the exception's show text (including the whole tx).
-- Both carry the observed validation time, not the configured limit.
--
-- One hard timeout is traced in a cascade of four namespaces as the connection
-- goes down (Net.Mux.Remote.ExceptionExit, Net.InboundGovernor.Remote.
-- ResponderErrored and .MuxErrored, Net.ConnectionManager.Remote.
-- ConnectionHandler.Error, over ~0.1-8 s). Only the first, ExceptionExit, is
-- read here; counting all four would count every timeout four times.
--
-- The show text is no contract. A hard timeout is recognised by the exception's
-- name at the start of the text only; its duration is read where the text has
-- the form "ExnMempoolTimeout <seconds>s ...", and is NULL otherwise
-- (mempool_timeouts counts those as unknown_durations). The soft duration is a
-- JSON number and is cast strictly, as everywhere else.
--
-- Columns, mempool_timeout_hard: process, at, peer (remote address:port),
--   duration_s (DOUBLE, NULL when not readable).
-- Columns, mempool_timeout_soft: process, at, txid, duration_s (DOUBLE),
--   mempool_txs, mempool_bytes (UBIGINT, the mempool when the tx was rejected).
CREATE OR REPLACE VIEW mempool_timeout_hard AS
SELECT
    process
  , "at"
  , split_part(data->>'$.bearer.connectionId', ' ', 2)                       AS peer
  , TRY_CAST(nullif(regexp_extract(data->>'$.event.exception',
                                    '^ExnMempoolTimeout ([0-9.]+)s ', 1), '') AS DOUBLE)
                                                                             AS duration_s
FROM raw
WHERE ns = 'Net.Mux.Remote.ExceptionExit'
  AND (data->>'$.event.exception') LIKE 'ExnMempoolTimeout%';

CREATE OR REPLACE VIEW mempool_timeout_soft AS
SELECT
    process
  , "at"
  , data->>'$.tx.txid'                                                       AS txid
  , CAST(data->>'$.errdetails.MempoolRejectedByTimeoutSoft' AS DOUBLE)       AS duration_s
  , mempool_txs(data)                                                        AS mempool_txs
  , mempool_bytes(data)                                                      AS mempool_bytes
FROM raw
WHERE ns = 'Mempool.RejectedTx'
  AND (data->'$.errdetails.MempoolRejectedByTimeoutSoft') IS NOT NULL;
