-- "v_block_deliveries": the AXIOMATIC view of EVERY DELIVERY of a ranking
-- block's body over the wire, one row per delivery, which node received which
-- block and when. That is all a row is. No role and no filter: the forger's own
-- fetch, if it ever makes one, sits here like any peer's. Who is the forger and
-- who is a peer is in "v_block_deliveries_by_peer", which is built on
-- this
-- view. Axiomatic means a property of the event as the protocol defines it; the
-- traces are only the evidence, and where no trace evidences a value the cell
-- is NULL, never inferred.
-- EVERY DELIVERY IS A ROW, re-fetches included. The trace FIRES AGAIN when a
-- node is handed a body it already holds and this view does not collapse them,
-- because each is a transfer that happened and nothing here is filtered. Which
-- of a pair's fetches counts as THE acquisition is a reading of these events,
-- not a fact about them.
-- ACQUIRING IS NOT ADOPTING. Holding the bytes is not putting the block on a
-- chain: a node fetches bodies it goes on to reject, and a block that lost its
-- height battle was still fetched by everyone who asked for it. Chain selection
-- is in "v_block_adoptions", and neither view contains the other.
-- IT IS THE RANKING-BLOCK TWIN OF "v_eb_deliveries", the same
-- event for
-- the other kind of block: the node now holds the thing. For an endorser block
-- that is the whole story, because an EB is never chained; for a ranking block
-- it is one step of several, and "v_block_adoptions_timing" carries the
-- rest.
-- THE ONLY HOLDER WITH NO FETCH IS THE FORGER. A node never fetches the block
-- it built, so every forging has no row here for its own forger.
-- EVERY OTHER ADOPTION HAS ITS FETCH. A peer adopts a block only after a
-- completed fetch of it, and no fetch lands after the adoption it supplied. An
-- adoption with no fetch is therefore not expected; if one appears, read it as
-- a dropped trace rather than as a node that acquired a body some other way.
-- THE "ALREADY HOLDING IT" CASE NEEDS A SECOND ADOPTION, WHICH DOES NOT HAPPEN.
-- A node holding a body does fetch nothing, but to show up as an adoption
-- without a fetch it would have to adopt the same block twice; no (node, block)
-- pair is adopted more than once, rollbacks included. The repetition is all on
-- the fetch side: many pairs are fetched MORE than once.
-- IN PRINCIPLE THE TRACE STILL CANNOT TELL non-possession from a body already
-- held, and nothing here infers which; the population where it would matter
-- is empty. This view states the fetches that completed, no more.
--
-- (Tier 1.)
--
-- Fields:
-- --------
-- * ts         When this delivery completed. A (node, block) pair can have
--   (NOT NULL) more than one row; the earliest is the first arrival.
--
-- * process    The node that received it.
--   (NOT NULL)
--
-- * hash       The block received.
--   (NOT NULL)
--
-- * bytes      The body's size, as the trace states it ("data.size").
--   (NOT NULL)
--
-- * delay      How long the fetch took, in SECONDS, as the trace states it
--   (NOT NULL) ("data.delay"). The endorser-block twin has NO counterpart.
--              It is the node's own measurement of the transfer, not a
--              difference this view computes; time differences between
--              traces are usually the better measure.
--
-- * conn_id    The CONNECTION it arrived over: the same row's
--   (NOT NULL) "peer.connectionId", verbatim. Two space-separated
--              endpoints, "localAddr:port remoteAddr:port", the local one
--              first. Addresses, not node names.
--
-- Example (in chronological order):
------------------------------------
--
-- at    2026-10-18T20:46:03.457988Z
-- ns    BlockFetch.Client.CompletedBlockFetch
-- data  { "kind":"CompletedBlockFetch"
--       , "block":"829dcc9e.."
--       , "size":963
--       , "delay":0.457860492
--       , "peer":{"connectionId":"172.31.36.108:30000 52.28.215.68:30048"}
--       }
--
CREATE OR REPLACE VIEW v_block_deliveries AS
SELECT
    ts                                  AS ts
  , process                             AS process
  , data->>'$.block'                    AS hash
  , CAST(data->>'$.size' AS UBIGINT)    AS bytes
  , CAST(data->>'$.delay' AS DOUBLE)    AS delay
  , data->>'$.peer.connectionId'        AS conn_id
FROM raw
WHERE ns = 'BlockFetch.Client.CompletedBlockFetch';

COMMENT ON VIEW v_block_deliveries IS 'tier 1: every block body delivery (BlockFetch client), one row per completed fetch, re-fetches included: ts, process, hash, bytes, delay, conn_id (previously: block_ranking_deliveries)';
