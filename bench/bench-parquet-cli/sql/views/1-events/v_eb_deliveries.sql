-- "v_eb_deliveries": the AXIOMATIC view of EVERY DELIVERY of an
-- endorser
-- block's body over the wire, one row per delivery, which node received which
-- EB from which peer and when. That is all a row is.
--
-- Tier 1, Leios family: EXPERIMENTAL until the Leios observables are final.
--
-- IT IS THE WIRE, NOT THE KERNEL. Trace is "LeiosFetch.Remote.Receive.Block",
-- the LeiosFetch mini-protocol handing a MsgLeiosBlock to this node: the bytes
-- arrived. A different moment than "Consensus.LeiosKernel.BlockAcquired", which
-- is the kernel declaring the body AVAILABLE once it has processed it. The
-- receive comes first. Neither is wrong; they
-- measure different things, and propagation is the wire.
-- NOR IS BlockAcquired THE TRANSACTIONS. It is the EB's (tx hash, size) LIST
-- written durably, plus the node registering a job pool for the referenced
-- transactions it still lacks -- which are typically still outstanding at
-- that moment. The closure completing is a third trace,
-- "Consensus.LeiosKernel.BlockTxsAcquired", typically an order of magnitude
-- later again. user_sql/queries/leios/fetched_to_acquired states the whole ladder.
-- THE FORGER IS NOT HERE AT ALL, and that is the point of using this trace. A
-- node never fetches an endorser block it built. The kernel's BlockAcquired
-- DOES fire on the forger, once per EB, so a view built on it would need a
-- forger-shaped special case; this one does not.
-- IT IS THE RANKING-BLOCK TWIN OF v_block_deliveries, the same event for
-- the other kind of block: the body arrived over the network. That is what
-- makes the two comparable; an EB's kernel-availability has no ranking-block
-- counterpart.
-- Axiomatic means a property of the event as the protocol defines it; the
-- traces are only the evidence, and where no trace evidences a value the
-- cell is NULL, never inferred.
-- EVERY DELIVERY IS A ROW, repeats included. A node can be handed a body it
-- already holds, and each such delivery is a transfer that happened; this
-- view does not collapse them. Which delivery counts as THE arrival is a
-- reading of these events, not a fact about them: the first one is, and
-- v_eb_timing is where that reading is made.
-- v_eb_deliveries_by_peer does NOT make it -- it carries every
-- delivery, exactly as this view does.
-- A HASH IS NOT A KEY FOR ENDORSER BLOCKS, (hash, slot_no) IS, AND THIS VIEW
-- HAS NO slot_no. The wire message names only the EB hash, so when one hash
-- was forged more than once this view cannot say which forging was delivered,
-- and nothing here should be read as doing so. Re-forges are rare, and a
-- re-forge is the SAME node forging twice from a mempool that had
-- not changed, because an endorser block IS its (tx hash,
-- size) list and re-forging an unchanged full mempool reproduces it exactly.
-- v_eb_announcements carries data.msg.ebSlot and so can tell the two
-- apart; v_eb_forged states the forgings themselves.
--
--
-- Fields:
-- --------
-- * ts        When this delivery completed. A (node, EB) pair can have more
--   (NOT NULL) than one row.
--
-- * process   The node that received it.
--   (NOT NULL)
--
-- * hash      The endorser block received: the hash of its (tx hash, size)
--   (NOT NULL)  list, as the message names it.
--
-- * bytes     The body's size on the wire, as the message declares it
--   (NOT NULL)  ("data.msg.ebBytesSize").
--
-- * conn_id   The CONNECTION it arrived over: the same row's
--   (NOT NULL)  peer.connectionId, verbatim, nothing cut off it. Two
--               space-separated endpoints, "localAddr:port remoteAddr:port",
--               the local one first. Addresses, not node names.
--
-- Example (in chronological order):
-- --------
--
-- at    2026-10-18T20:25:23.774472Z
-- ns    LeiosFetch.Remote.Receive.Block
-- data  { "kind":"Recv"
--       , "msg":{ "kind":"MsgLeiosBlock"
--               , "ebHash":"d11b0e82.."
--               , "ebBytesSize":25718
--               }
--       , "peer":{"connectionId":"172.31.36.108:30000 13.238.162.49:30002"}
--       }
--
CREATE OR REPLACE VIEW v_eb_deliveries AS
SELECT
    ts                                           AS ts
  , process                                      AS process
  , data->>'$.msg.ebHash'                        AS hash
  , CAST(data->>'$.msg.ebBytesSize' AS UBIGINT)  AS bytes
  , data->>'$.peer.connectionId'                 AS conn_id
FROM raw
WHERE ns = 'LeiosFetch.Remote.Receive.Block';

COMMENT ON VIEW v_eb_deliveries IS 'tier 1, Leios (experimental): every endorser-block body delivery over the wire (LeiosFetch receive), one row per delivery, repeats included: ts, process, hash, bytes, conn_id (previously: block_endorser_deliveries)';
