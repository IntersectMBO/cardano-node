-- "v_block_adoptions_timing": the PIPELINE behind every adoption
-- v_block_adoptions_by_peer reports — one row per (block, peer), the same
-- rows as that view, with each step the peer took to get the block before it
-- could adopt it. The adoption itself, and who adopted what, is that view's
-- business; this one adds only the steps leading up to it, so a query that
-- wants "who and when" never pays for the pipeline.
-- Built on v_block_adoptions_by_peer: the (block, forger, peer) rows and
-- peer_adopted_at come from there verbatim, never from a second reading of
-- the ChainDB trace. Everything else is a first-event-per-(node, block)
-- read of the mini-protocol traces, which no axiom states.
-- THE STEPS ARE A PEER'S, IN ORDER: it hears the header exists, asks for the
-- body, the body arrives, it writes it to its volatile DB, it adopts it.
-- Any of them can be NULL on its own: a node that already had the body asks
-- for nothing, a node whose store trace is missing still adopted. A NULL is
-- a missing trace and never a claim that the step did not happen, and
-- nothing is inferred from one.
-- Every timestamp is named for whose step it is; there is no bare `ts`,
-- because a row carries several.
--
-- (Tier 3.)
--
-- Fields:
-- --------
-- * hash                       The block.
--   (NOT NULL)
--
-- * forger_process             The node that forged it, and
--   (NOT NULL)
--
-- * peer_process               the other node whose pipeline this row
--   (NULL)                     describes, both as v_block_adoptions_by_peer
--                              states them. NULL only on the single row of a
--                              block no peer adopted, as is every peer_
--                              column beside it.
--
-- * forger_stored_at           When the forger wrote its own block into its
--   (NULL)                     volatile DB (ChainDB.AddBlockEvent
--                              .AddedBlockToVolatileDB, earliest). NULL when
--                              the forger logged no such write. That happens:
--                              a block can be popped from the ChainDB queue
--                              and go on to be adopted with no write traced
--                              for it, so the write did happen and only its
--                              trace is missing; nothing is inferred for those
--                              rows. It is rare on the ordinary path and
--                              common on the rollback one, so expect most of
--                              the NULLs on blocks that v_fork_blocks lists.
--
-- * peer_first_noticed_at      When that peer first learned the block exists:
--   (NULL)                     its earliest ChainSync.Client.DownloadedHeader
--                              naming the block. One header is one block's
--                              header, so at this point the node knows the
--                              block exists and where it sits, and has no body
--                              yet. The header arrives once per upstream
--                              connection, several times per block, so this
--                              is whichever connection delivered it first.
--                              NULL when the peer downloaded no header for the
--                              block.
--
-- * peer_first_noticed_conn_id The CONNECTION that header arrived over: the
--   (NULL)                     same row's peer.connectionId, verbatim. Two
--                              space-separated endpoints, "localAddr:port
--                              remoteAddr:port": the local one first (this
--                              node's own listening socket), then the node it
--                              heard the header from. Addresses, not node
--                              names: the traces state no mapping between the
--                              two (a listening socket can be a private
--                              address while peers reach it on a public one),
--                              and nothing is inferred. NULL with
--                              peer_first_noticed_at.
--
-- * peer_first_requested_at    When that peer asked for the body
--   (NULL)                     (BlockFetch.Client.SendFetchRequest, earliest).
--                              The request names a range by its head and a
--                              length; with length 1, head names exactly the
--                              block asked for. Longer ranges exist but are
--                              rare, and are read here by their head only.
--                              NULL when the peer sent no
--                              request: a node that already has the block
--                              never asks.
--
-- * peer_first_requested_conn_id
--   (NULL)                     The connection it asked over, read as
--                              peer_first_noticed_conn_id is. NULL with
--                              peer_first_requested_at.
--
-- * peer_fetched_at            When that peer completed the body fetch
--   (NULL)                     (BlockFetch.Client.CompletedBlockFetch,
--                              earliest). Requests and completions should
--                              cover the same (node, block) pairs, so a
--                              request with no completion, or the reverse, is
--                              an anomaly worth looking at. NULL when the peer
--                              logged no completed fetch.
--                              That trace also carries a `delay` field,
--                              deliberately NOT used here and not what the
--                              name suggests: it is the block's AGE AT
--                              COMPLETION, counted from its slot start, not
--                              the time the fetch took (fetched - requested).
--
-- * peer_fetched_conn_id       The connection that body arrived over, read as
--   (NULL)                     peer_first_noticed_conn_id is. The two commonly
--                              differ: a node hears a header from one peer and
--                              fetches the body from another. NULL with
--                              peer_fetched_at.
--
-- * peer_stored_at             When that peer wrote the block into its
--   (NULL)                     volatile DB (earliest). NULL when it logged no
--                              such write, which has two known shapes: the
--                              block was already in its volatile DB
--                              (IgnoreBlockAlreadyInVolatileDB names those) or
--                              no store trace exists at all. Nothing is
--                              inferred for either.
--
-- * peer_adopted_at            When that peer put it on its chain, from
--   (NULL)                     v_block_adoptions_by_peer verbatim: the step all
--                              the others lead to, repeated here so the
--                              pipeline reads end to end in one row.
--
-- Example (in chronological order):
------------------------------------
--
-- at    2026-10-18T18:41:11.293780745Z
-- ns    ChainSync.Client.DownloadedHeader
-- data  { "block":"bb416f.."
--       , "blockNo":24
--       , "peer":{"connectionId":"172.31.10.123:30044 13.238.107.224:30008"}
--       , "slot":592
--       }
-- (peer_first_noticed_at / _conn_id)
--
-- at    2026-10-18T18:41:11.294799050Z
-- ns    BlockFetch.Client.SendFetchRequest
-- data  { "head":"bb416f.."
--       , "length":1
--       , "peer":{"connectionId":"172.31.10.123:30044 13.238.107.224:30008"}
--       }
-- (peer_first_requested_at / _conn_id; data.head names the range head.)
--
-- at    2026-10-18T18:41:11.296208081Z
-- ns    BlockFetch.Client.CompletedBlockFetch
-- data  { "block":"bb416f.."
--       , "delay":0.296118713
--       , "peer":{"connectionId":"172.31.10.123:30044 13.238.107.224:30008"}
--       , "size":962
--       }
-- (peer_fetched_at / peer_fetched_conn_id)
--
-- at    2026-10-18T18:41:11.296308614Z
-- ns    ChainDB.AddBlockEvent.AddedBlockToVolatileDB
-- data  { "block":{ "hash":"bb416f.."
--                 , "kind":"Point"
--                 , "slot":592
--                 }
--       , "blockNo":"24"
--       , "risingEdge":1
--       }
-- (peer_stored_at)
--
-- at    2026-10-18T18:41:11.365516629Z
-- ns    ChainDB.AddBlockEvent.AddedToCurrentChain
-- data  { "newSuffixSelectView":
--         { "blockNo":24
--         , "issueNo":0
--         , "issuerHash":"6484e5.."
--         , "kind":"PraosTiebreakerView"
--         , "slotNo":592
--         , "tieBreakVRF":"9a0ee6.."
--         , "weightBoost":0
--         }
--       , "newtip":"bb416f..@592"
--       }
-- (peer_adopted_at -- the last rung, via v_block_adoptions.)
--
CREATE OR REPLACE VIEW v_block_adoptions_timing AS
WITH
  -- Each step's earliest trace per (node, block), with the connection of that
  -- earliest trace.
  noticed AS (
    SELECT process, data->>'$.block' AS hash, min(ts) AS ts,
           arg_min(data->>'$.peer.connectionId', ts) AS conn_id
    FROM raw WHERE ns = 'ChainSync.Client.DownloadedHeader' GROUP BY ALL),
  requested AS (
    SELECT process, data->>'$.head' AS hash, min(ts) AS ts,
           arg_min(data->>'$.peer.connectionId', ts) AS conn_id
    FROM raw WHERE ns = 'BlockFetch.Client.SendFetchRequest' GROUP BY ALL),
  fetched AS (
    SELECT process, data->>'$.block' AS hash, min(ts) AS ts,
           arg_min(data->>'$.peer.connectionId', ts) AS conn_id
    FROM raw WHERE ns = 'BlockFetch.Client.CompletedBlockFetch' GROUP BY ALL),
  stored AS (
    SELECT process, data->>'$.block.hash' AS hash, min(ts) AS ts
    FROM raw WHERE ns = 'ChainDB.AddBlockEvent.AddedBlockToVolatileDB' GROUP BY ALL)
SELECT
    a.hash             AS hash
  , a.forger_process   AS forger_process
  , fst.ts             AS forger_stored_at
  , a.peer_process     AS peer_process
  , no.ts              AS peer_first_noticed_at
  , no.conn_id         AS peer_first_noticed_conn_id
  , rq.ts              AS peer_first_requested_at
  , rq.conn_id         AS peer_first_requested_conn_id
  , fe.ts              AS peer_fetched_at
  , fe.conn_id         AS peer_fetched_conn_id
  , st.ts              AS peer_stored_at
  , a.peer_adopted_at  AS peer_adopted_at
FROM v_block_adoptions_by_peer a
LEFT JOIN stored    fst ON fst.process = a.forger_process AND fst.hash = a.hash
LEFT JOIN noticed   no  ON no.process  = a.peer_process   AND no.hash  = a.hash
LEFT JOIN requested rq  ON rq.process  = a.peer_process   AND rq.hash  = a.hash
LEFT JOIN fetched   fe  ON fe.process  = a.peer_process   AND fe.hash  = a.hash
LEFT JOIN stored    st  ON st.process  = a.peer_process   AND st.hash  = a.hash;

COMMENT ON VIEW v_block_adoptions_timing IS 'tier 3: the pipeline behind every peer adoption (noticed, requested, fetched, stored, adopted, with connections), one row per (block, peer) of v_block_adoptions_by_peer (previously: block_adoptions_timing)';
