-- "v_mempool_timeout_hard": every HARD mempool timeout, one row per event. Tier 1.
-- Its sibling v_mempool_timeout_soft has the soft ones; chk_mempool_timeouts
-- reports both as violations, v_mempool_timeouts counts them per node.
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
-- ConnectionHandler.Error, spread over up to seconds). Only the first, ExceptionExit, is
-- read here; counting all four would count every timeout four times.
--
-- The show text is no contract. A hard timeout is recognised by the exception's
-- name at the start of the text only; its duration is read where the text has
-- the form "ExnMempoolTimeout <seconds>s ...", and is NULL otherwise
-- (v_mempool_timeouts counts those as unknown_durations). The soft duration is a
-- JSON number and is cast strictly, as everywhere else.
--
-- Fields:
-- --------
-- * process      The node that tore down the connection.
--   (NOT NULL)
--
-- * ts           When (Net.Mux.Remote.ExceptionExit).
--   (NOT NULL)
--
-- * peer         The remote address:port the tx came from.
--
-- * duration_s   The observed validation time, seconds; NULL when the show
--   (NULL)       text doesn't have the expected form.
CREATE OR REPLACE VIEW v_mempool_timeout_hard AS
SELECT
    process
  , ts
  , split_part(data->>'$.bearer.connectionId', ' ', 2)                       AS peer
  , TRY_CAST(nullif(regexp_extract(data->>'$.event.exception',
                                    '^ExnMempoolTimeout ([0-9.]+)s ', 1), '') AS DOUBLE)
                                                                             AS duration_s
FROM raw
WHERE ns = 'Net.Mux.Remote.ExceptionExit'
  AND (data->>'$.event.exception') LIKE 'ExnMempoolTimeout%';

COMMENT ON VIEW v_mempool_timeout_hard IS 'tier 1: hard mempool timeouts (connection torn down), one row per event: process, ts, peer, duration_s';
