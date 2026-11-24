-- "v_block_announcements": the AXIOMATIC view of every ANNOUNCEMENT of a
-- ranking block's header to a peer, one row per trace line, which node told
-- which connection about which block, and when.
-- IT IS A DIFFUSION VIEW, NOT A DECLARATION. It says a header was pushed at
-- a peer; it says nothing about what the block contains.
-- EVERY NODE ANNOUNCES EVERY BLOCK TO ITS PEERS, MANY TIMES; a block's own
-- forger does so exactly as a relaying node does. Every row is a send to
-- ANOTHER node: no connectionId has the same address at both ends. THAT IS WHY
-- THE ROW COUNT RUNS FAR ABOVE THE NUMBER OF BLOCKS. Anyone counting
-- announcements wants to decide first WHICH of the three multiplicities they
-- mean: the trace pair, the connection, or the node. Take rising_edge for
-- one row per announcement; GROUP BY process, hash for one row per announcing
-- node.
-- THIS IS THE TWIN OF "v_eb_announcements", which reads
-- "LeiosNotify.Remote.Send.BlockAnnouncement", the same thing on the Leios
-- side, a node telling a peer that an endorser block exists. Read them
-- together, but DO NOT COMPARE ROW COUNTS. The multiplicity is the SAME in
-- both. What DIFFERS is how many rows ONE SEND TO ONE PEER leaves behind.
-- Here a single send is traced TWICE, the risingEdge = 1 row and then the
-- row without it; there it is traced ONCE,
-- because that trace has no such field. So this view has 2 rows per (node,
-- block, connection) and that view 1 per (node, EB, slot, connection) -- the
-- same "every connection, once", counted two different ways.
-- A RANKING BLOCK HAS NO DECLARATION EVENT. A ranking block IS its own
-- announcement, so there is nothing for its forger to declare separately. An
-- endorser block does have one, "Consensus.LeiosKernel.BlockAnnounced", which
-- is where the announcing ranking block's hash is stated; it is read in
-- v_block_forged.announced_eb_hash and is not an announcement view.
-- Axiomatic means a property of the event as the protocol defines it; the
-- traces are only the evidence, and where no trace evidences a value the cell
-- is NULL, never inferred.
-- ROLLBACKS ARE NOT HERE. "ChainSync.ServerHeader.Update" is either an add or
-- a rollback and never both: the rows with no data.addBlock are EXACTLY the
-- rows with a data.rollBackTo. A
-- rollback names no block to announce, so it has no row in a view keyed on a
-- block hash. v_fork_switches and v_fork_blocks are where a node's rollbacks are.
--
-- NO slot_start HERE. A slot start is the instant a slot was SCHEDULED
-- for, and the row below is an announcement, not a forging: its slot is the
-- BLOCK's identity, carried so the row can be read against the forging,
-- not a clock for when it was announced. v_block_forged and "v_eb_forged"
-- carry slot_start because their ts IS the forging the slot scheduled;
-- join one of them on hash to measure against it.
--
-- (Tier 1.)
--
-- Fields:
-- --------
-- * ts             When this announcement was traced.
--   (NOT NULL)
--
-- * process        The node that announced it.
--   (NOT NULL)
--
-- * hash           The block announced.
--   (NOT NULL)
--
-- * slot_no        The block's slot, both parsed out of "data.addBlock" (a
--   (NOT NULL)     string with a 64-character hash followed by an '@' and the
--                  slot number). A value in any unexpected shape is skipped
--                  loudly-by-absence rather than cut at the wrong place.
--
-- * conn_id        The CONNECTION it was announced over: the same row's
--   (NOT NULL)     "peer.connectionId", verbatim. Two space-separated
--                  endpoints, "localAddr:port remoteAddr:port", the local one
--                  first. Addresses, not node names.
--
-- * rising_edge    true on the FIRST row of a send, NULL on the second (the
--   (NULL)         field is absent there). The ChainSync server brackets each
--                  send: the rising row, then a row without the field about
--                  half a millisecond later. Per (block, node, connection)
--                  the two always pair, never a lone row of either kind.
--                  It is two-phase TRACING, not two announcements, so
--                  filter on rising_edge (or rising_edge IS NULL) to count
--                  each announcement exactly once.
--
-- * blocking_read  true when the peer was waiting on a blocking read, false
--   (NOT NULL)     when it was not (non-blocking rows are usually a
--                  minority).
--
-- * tip_hash       The announcer's own chain tip when it announced: hash,
-- * tip_block_no   height and slot. The tip is the announcer's state, not the
-- * tip_slot       announced block's. tip_block_no is -1 while the tip is
--   (NOT NULL)     genesis.
--
-- Example (in chronological order):
------------------------------------
--
-- at    2026-10-18T18:38:31.011190Z
-- ns    ChainSync.ServerHeader.Update
-- data  { "kind":"ChainSyncServer.Update"
--       , "addBlock":"60cee8a7..@432"
--       , "risingEdge":1
--       , "blockingRead":1
--       , "rollBackTo":null
--       , "tip":{ "block":"9f3d89fc.."
--               , "blockNo":20
--               , "slot":385
--               }
--       , "peer":{"connectionId":"172.31.36.108:30000 52.28.215.68:30048"}
--       }
--
-- A peer.connectionId on every row, because every row IS a send, exactly as
-- in v_eb_announcements. The field that view has and this one
-- lacks is the announced block's size: a ranking-block header does not
-- state it.
--
CREATE OR REPLACE VIEW v_block_announcements AS
SELECT
    ts                                                       AS ts
  , process                                                  AS process
  , split_part(data->>'$.addBlock', '@', 1)                  AS hash
  , CAST(split_part(data->>'$.addBlock', '@', 2) AS UBIGINT) AS slot_no
  , data->>'$.peer.connectionId'                             AS conn_id
  , CAST(data->>'$.risingEdge' AS BOOLEAN)                   AS rising_edge
  , CAST(data->>'$.blockingRead' AS BOOLEAN)                 AS blocking_read
  , data->>'$.tip.block'                                     AS tip_hash
  , CAST(data->>'$.tip.blockNo' AS BIGINT)                   AS tip_block_no
  , CAST(data->>'$.tip.slot' AS UBIGINT)                     AS tip_slot
FROM raw
WHERE ns = 'ChainSync.ServerHeader.Update'
  -- An add, not a rollback, in the expected "<64-char hash>@<slot>" shape.
  AND strpos(data->>'$.addBlock', '@') = 65;

COMMENT ON VIEW v_block_announcements IS 'tier 1: every block header announcement to a peer (ChainSync server), one row per trace line (two per send: rising_edge): ts, process, hash, slot_no, conn_id, tip (previously: block_ranking_announcements)';
