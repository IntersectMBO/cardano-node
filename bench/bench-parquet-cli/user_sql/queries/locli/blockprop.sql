-- locli's block propagation (blockprop), rebuilt from a PQDIR: the shared base of
-- the locli_blockprop_* macros (blockprop_control.sql, blockprop_endtoend.sql,
-- blockprop_forger.sql, blockprop_peers.sql), and the filter (locli_filter) and
-- CDF (locli_cdf) of all macros in this directory.
--
-- PURPOSE: CROSS-CHECKING LOCLI, not replacing it. The same run analysed by
-- locli (analysis/blockprop.json) and by these macros must give the same
-- numbers; a difference is a bug on one side or a definition that drifted.
-- locli_blockprop_diff() (diff_blockprop.sql) does the comparison.
-- User content (user_sql/README.md): not reviewed like sql/.
--
--   bench-parquet query PQDIR --load user_sql/queries/locli
--   SELECT * FROM locli_blockprop_forger();
--
-- THE DEFINITIONS ARE LOCLI'S (Cardano.Analysis.BlockProp), not the package
-- views': locli reads its own, narrower set of traces, and that is what is
-- reproduced here. Per node, in timestamp order, locli folds these traces into
-- one entry per block hash:
--   notice    ChainSync.Client.DownloadedHeader          (data.block)
--   request   BlockFetch.Client.SendFetchRequest          (data.head, data.length)
--   fetch     BlockFetch.Client.CompletedBlockFetch       (data.block)
--   forge     Forge.Loop.ForgedBlock                      (data.block)
--   adopt     ChainDB.AddBlockEvent.AddedToCurrentChain   (data.newtip, chainLengthDelta, default 1)
--             Forge.Loop.AdoptedBlock                     (data.blockHash, blockSize; delta 1)
--   announce  ChainSync.ServerHeader.Update: rising edge (risingEdge true), not
--             a non-blocking read (blockingRead false), no rollBackTo
--   send      BlockFetch.Server.SendBlock                 (data.block)
-- Each event counts the first time it happens for a block on a node. An
-- entry is created only by a notice (an observer) or a forge (the forger);
-- any other event first, a second forge, or a request or fetch on the forger
-- makes the node's entry for that block an error, which locli then leaves out.
--
-- THE FORGE LOOP is read the way locli folds it: at each ForgedBlock, the
-- latest StartLeadershipCheck, BlockContext, LedgerState, LedgerView,
-- NodeIsLeader, ForgeTickedLedgerState and ForgingMempoolSnapshot traced by
-- that node at or before it (any slot).
--
-- THE MAIN CHAIN is locli's: from the highest block any node knows, one block
-- back (the tip's propagation is still in flight), then along blockPrev of the
-- forges down to block 0. Where several blocks share the highest number, the
-- one with the greatest hash is taken (locli's pick among equals depends on
-- its map order); this matters only when they have different predecessors.
--
-- FILTERS: one struct, built by locli_filter() and passed to every macro as
-- filters := ...; each field is one of locli's chain filters, absolute values
-- only:
--   first_slot / last_slot   SlotGEq / SlotLEq
--   size_min / size_max      BSizeGEq / BSizeLEq: block size in bytes, inclusive
--   min_adoptions := N       BMinimumAdoptions N: at least N other nodes noticed,
--                            requested, fetched and adopted the block
--   unitary := true          BUnitaryChainDelta: the forger's adoption had chain
--                            length delta 1
-- A field left out lets every block pass: locli_filter() is the empty filter.
-- Without filters, the DEFAULT is locli_filter(min_adoptions := nodes - 1),
-- nodes being the session's node-* processes.
--   SELECT * FROM locli_blockprop_forger();                              -- the default
--   SELECT * FROM locli_blockprop_forger(filters := locli_filter());     -- every block
--   SELECT * FROM locli_blockprop_forger(filters := locli_filter(first_slot := 16000,
--                                        size_min := 81101, min_adoptions := 51));
-- locli's relative filters as absolute values (B the genesis maxBlockBodySize,
-- E the genesis epoch length, run_epoch_length()): BFullnessGEq f is
-- size_min := floor(B * f) + 1 (locli: size > floor(B * f)); BFullnessLEq f is
-- size_max := floor(B * f) - 1 (locli: size < floor(B * f)); EpochGEq e is
-- first_slot := e * E. Not supported: BNonNegatives, EpSlotGEq/LEq,
-- EpochSafeIntGEq/LEq.
-- Genesis timing comes from meta/run.parquet (slot_start()).

-- The filter (see FILTERS above): every argument left out lets every block pass.
CREATE OR REPLACE MACRO locli_filter(first_slot := NULL, last_slot := NULL, size_min := NULL,
                                     size_max := NULL, min_adoptions := NULL, unitary := false) AS
  { 'first_slot':    CAST(first_slot AS BIGINT), 'last_slot': CAST(last_slot AS BIGINT)
  , 'size_min':      CAST(size_min AS BIGINT),   'size_max':  CAST(size_max AS BIGINT)
  , 'min_adoptions': CAST(min_adoptions AS BIGINT)
  , 'unitary':       CAST(unitary AS BOOLEAN) };

-- filters, or the default when NULL: min_adoptions := the session's nodes - 1.
CREATE OR REPLACE MACRO locli_filter_or_default(filters) AS
  coalesce(filters, locli_filter(min_adoptions := (SELECT count(*) - 1 FROM files WHERE process LIKE 'node-%')));

-- Every block-propagation event locli reads, one row per trace.
CREATE OR REPLACE MACRO locli_bp_events() AS TABLE
SELECT process, ts, seq, kind, left(hash, 64) AS hash, len, size, block_no, slot_no, prev_hash
FROM (
  SELECT process, ts, seq,
    CASE ns
      WHEN 'ChainSync.Client.DownloadedHeader'         THEN 'notice'
      WHEN 'BlockFetch.Client.SendFetchRequest'        THEN 'request'
      WHEN 'BlockFetch.Client.CompletedBlockFetch'     THEN 'fetch'
      WHEN 'Forge.Loop.ForgedBlock'                    THEN 'forge'
      WHEN 'ChainDB.AddBlockEvent.AddedToCurrentChain' THEN 'adopt'
      WHEN 'Forge.Loop.AdoptedBlock'                   THEN 'adopt'
      WHEN 'ChainSync.ServerHeader.Update'             THEN 'announce'
      WHEN 'BlockFetch.Server.SendBlock'               THEN 'send'
    END AS kind,
    CASE ns
      WHEN 'BlockFetch.Client.SendFetchRequest'        THEN data->>'$.head'
      WHEN 'ChainDB.AddBlockEvent.AddedToCurrentChain' THEN data->>'$.newtip'
      WHEN 'Forge.Loop.AdoptedBlock'                   THEN data->>'$.blockHash'
      WHEN 'ChainSync.ServerHeader.Update'             THEN coalesce(data->>'$.addBlock', data->>'$.block')
      ELSE data->>'$.block'
    END AS hash,
    CASE ns
      WHEN 'BlockFetch.Client.SendFetchRequest'        THEN CAST(data->>'$.length' AS BIGINT)
      WHEN 'ChainDB.AddBlockEvent.AddedToCurrentChain' THEN coalesce(CAST(data->>'$.chainLengthDelta' AS BIGINT), 1)
      WHEN 'Forge.Loop.AdoptedBlock'                   THEN 1
    END AS len,
    CASE WHEN ns = 'Forge.Loop.AdoptedBlock' THEN CAST(data->>'$.blockSize' AS BIGINT) END AS size,
    CASE WHEN ns IN ('ChainSync.Client.DownloadedHeader', 'Forge.Loop.ForgedBlock')
         THEN CAST(data->>'$.blockNo' AS BIGINT) END AS block_no,
    CASE WHEN ns IN ('ChainSync.Client.DownloadedHeader', 'Forge.Loop.ForgedBlock')
         THEN CAST(data->>'$.slot' AS BIGINT) END AS slot_no,
    CASE WHEN ns = 'Forge.Loop.ForgedBlock' THEN data->>'$.blockPrev' END AS prev_hash
  FROM raw
  WHERE ns IN ( 'ChainSync.Client.DownloadedHeader', 'BlockFetch.Client.SendFetchRequest'
              , 'BlockFetch.Client.CompletedBlockFetch', 'Forge.Loop.ForgedBlock'
              , 'ChainDB.AddBlockEvent.AddedToCurrentChain', 'Forge.Loop.AdoptedBlock'
              , 'BlockFetch.Server.SendBlock')
     OR (    ns = 'ChainSync.ServerHeader.Update'
         AND (data->'$.risingEdge') = 'true'::JSON
         AND coalesce((data->'$.blockingRead') <> 'false'::JSON, true)
         AND NOT json_exists(data, '$.rollBackTo')))
WHERE hash IS NOT NULL;

-- locli's per-node entry for each block: one row per (process, hash), with the
-- first instant of each event, the role (forger, observer, error) and the
-- chain length delta.
CREATE OR REPLACE MACRO locli_bp_entries() AS TABLE
WITH e AS MATERIALIZED (FROM locli_bp_events())
SELECT
    process, hash
  , CASE
      WHEN arg_min(kind, (ts, seq)) = 'forge'
       AND count(*) FILTER (WHERE kind = 'forge') = 1
       AND count(*) FILTER (WHERE kind IN ('request', 'fetch')) = 0 THEN 'forger'
      WHEN arg_min(kind, (ts, seq)) = 'notice'
       AND count(*) FILTER (WHERE kind = 'forge') = 0                THEN 'observer'
      ELSE 'error'
    END                                                  AS role
  , coalesce(any_value(block_no) FILTER (WHERE kind = 'forge'),
             arg_min(block_no, (ts, seq)) FILTER (WHERE kind = 'notice')) AS block_no
  , coalesce(any_value(slot_no) FILTER (WHERE kind = 'forge'),
             arg_min(slot_no, (ts, seq)) FILTER (WHERE kind = 'notice'))  AS slot_no
  , any_value(prev_hash) FILTER (WHERE kind = 'forge')   AS prev_hash
  , min(ts) FILTER (WHERE kind = 'notice')               AS t_notice
  , min(ts) FILTER (WHERE kind = 'request')              AS t_request
  , min(ts) FILTER (WHERE kind = 'fetch')                AS t_fetch
  , min(ts) FILTER (WHERE kind = 'forge')                AS t_forge
  , min(ts) FILTER (WHERE kind = 'adopt')                AS t_adopt
  , min(ts) FILTER (WHERE kind = 'announce')             AS t_announce
  , min(ts) FILTER (WHERE kind = 'send')                 AS t_send
  , arg_min(size, (ts, seq)) FILTER (WHERE kind = 'adopt' AND size IS NOT NULL) AS size
  -- Forger: the delta of its first adoption (0 when it never adopted).
  -- Observer: the larger of its first request's length and first adoption's delta.
  , arg_min(len, (ts, seq)) FILTER (WHERE kind = 'adopt')   AS adopt_len
  , arg_min(len, (ts, seq)) FILTER (WHERE kind = 'request') AS request_len
FROM e
GROUP BY process, hash;

-- locli's main chain, one row per block, with the forger's timings (locli's
-- BlockForge, seconds), the block's slot and height battles, and the filter
-- verdicts (accepted = all filters pass, slot_accepted = the slot filters pass).
CREATE OR REPLACE MACRO locli_bp_chain(filters := NULL) AS TABLE
WITH RECURSIVE
  flt AS (SELECT locli_filter_or_default(filters) AS v),
  ent AS MATERIALIZED (FROM locli_bp_entries()),
  forges AS MATERIALIZED (
    SELECT process, hash, prev_hash, block_no, slot_no, t_forge, t_adopt, t_announce, t_send,
           size, coalesce(adopt_len, 0) AS chain_delta
    FROM ent WHERE role = 'forger'),
  tip AS (
    SELECT f.prev_hash AS hash
    FROM forges f
    WHERE f.hash = (SELECT max(hash) FROM ent
                    WHERE role <> 'error'
                      AND block_no = (SELECT max(block_no) FROM ent WHERE role <> 'error'))),
  chain(hash) AS (
      SELECT hash FROM tip
    UNION ALL
      SELECT f.prev_hash FROM forges f JOIN chain c ON f.hash = c.hash WHERE f.block_no > 0),
  loop AS MATERIALIZED (
    SELECT process, ts, ns FROM raw
    WHERE ns IN ( 'Forge.Loop.StartLeadershipCheck', 'Forge.Loop.BlockContext'
                , 'Forge.Loop.LedgerState', 'Forge.Loop.LedgerView', 'Forge.Loop.NodeIsLeader'
                , 'Forge.Loop.ForgeTickedLedgerState', 'Forge.Loop.ForgingMempoolSnapshot')),
  st AS (SELECT process, ts FROM loop WHERE ns = 'Forge.Loop.StartLeadershipCheck'),
  bc AS (SELECT process, ts FROM loop WHERE ns = 'Forge.Loop.BlockContext'),
  ls AS (SELECT process, ts FROM loop WHERE ns = 'Forge.Loop.LedgerState'),
  lv AS (SELECT process, ts FROM loop WHERE ns = 'Forge.Loop.LedgerView'),
  ld AS (SELECT process, ts FROM loop WHERE ns = 'Forge.Loop.NodeIsLeader'),
  tk AS (SELECT process, ts FROM loop WHERE ns = 'Forge.Loop.ForgeTickedLedgerState'),
  ms AS (SELECT process, ts FROM loop WHERE ns = 'Forge.Loop.ForgingMempoolSnapshot'),
  fl AS (
    SELECT f.*, slot_start(f.slot_no) AS slot_start,
           st.ts AS t_start, bc.ts AS t_blkctx, ls.ts AS t_lgrstate, lv.ts AS t_lgrview,
           ld.ts AS t_lead, tk.ts AS t_ticked, ms.ts AS t_memsnap
    FROM forges f SEMI JOIN chain c ON c.hash = f.hash
    ASOF LEFT JOIN st ON st.process = f.process AND st.ts <= f.t_forge
    ASOF LEFT JOIN bc ON bc.process = f.process AND bc.ts <= f.t_forge
    ASOF LEFT JOIN ls ON ls.process = f.process AND ls.ts <= f.t_forge
    ASOF LEFT JOIN lv ON lv.process = f.process AND lv.ts <= f.t_forge
    ASOF LEFT JOIN ld ON ld.process = f.process AND ld.ts <= f.t_forge
    ASOF LEFT JOIN tk ON tk.process = f.process AND tk.ts <= f.t_forge
    ASOF LEFT JOIN ms ON ms.process = f.process AND ms.ts <= f.t_forge),
  -- Observations (locli's BlockObservation): observer entries with notice,
  -- request and fetch.
  obs AS (
    SELECT o.hash, count(*) FILTER (WHERE o.t_adopt IS NOT NULL) AS adoptions
    FROM ent o
    WHERE o.role = 'observer' AND o.t_notice IS NOT NULL AND o.t_request IS NOT NULL AND o.t_fetch IS NOT NULL
    GROUP BY o.hash),
  heights AS (SELECT DISTINCT block_no, hash FROM ent WHERE role <> 'error'),
  -- Rivals: other blocks at the same height (any node's entry) or forged for
  -- the same slot; a slot battle when the rival was forged for the same slot,
  -- else a height battle.
  cand AS (
      SELECT c.hash, h.hash AS other FROM fl c JOIN heights h ON h.block_no = c.block_no
    UNION
      SELECT c.hash, f.hash FROM fl c JOIN forges f ON f.slot_no = c.slot_no),
  rivals AS (
    SELECT x.hash,
           count(*) FILTER (WHERE f.slot_no = c.slot_no)                AS slot_battle,
           count(*) FILTER (WHERE f.slot_no IS DISTINCT FROM c.slot_no) AS height_battle
    FROM cand x
    JOIN fl c ON c.hash = x.hash
    LEFT JOIN (SELECT DISTINCT hash, slot_no FROM forges) f ON f.hash = x.other
    WHERE x.other <> x.hash
    GROUP BY x.hash)
SELECT
    c.process AS forger, c.hash, c.prev_hash, c.block_no, c.slot_no, c.slot_start, c.size, c.chain_delta
  , coalesce(o.adoptions, 0)                                              AS adoptions
  , coalesce(r.slot_battle, 0)                                            AS slot_battle
  , coalesce(r.height_battle, 0)                                          AS height_battle
  , c.t_forge
  -- locli's BlockForge timings, seconds:
  , (epoch_ns(c.t_start) - epoch_ns(c.slot_start)) / 1e9                  AS forger_start
  , (epoch_ns(c.t_blkctx) - epoch_ns(c.t_start)) / 1e9                    AS forger_blkctx
  , (epoch_ns(c.t_lgrstate) - epoch_ns(c.t_blkctx)) / 1e9                 AS forger_lgrstate
  , (epoch_ns(c.t_lgrview) - epoch_ns(c.t_lgrstate)) / 1e9                AS forger_lgrview
  , (epoch_ns(c.t_lead) - epoch_ns(coalesce(c.t_lgrview, c.t_start))) / 1e9 AS forger_lead
  , (epoch_ns(c.t_ticked) - epoch_ns(c.t_lead)) / 1e9                     AS forger_ticked
  , (epoch_ns(c.t_memsnap) - epoch_ns(c.t_ticked)) / 1e9                  AS forger_memsnap
  , (epoch_ns(c.t_forge) - epoch_ns(coalesce(c.t_memsnap, c.t_lead))) / 1e9 AS forger_forge
  , (epoch_ns(c.t_announce) - epoch_ns(c.t_forge)) / 1e9                  AS forger_announce
  , (epoch_ns(c.t_announce) - epoch_ns(c.slot_start)) / 1e9               AS forger_announce_cum
  , (epoch_ns(c.t_send) - epoch_ns(c.t_forge)) / 1e9                      AS forger_send
  , (epoch_ns(c.t_adopt) - epoch_ns(c.t_forge)) / 1e9                     AS forger_adoption
  , (flt.v.first_slot IS NULL OR c.slot_no >= flt.v.first_slot)
    AND (flt.v.last_slot IS NULL OR c.slot_no <= flt.v.last_slot)         AS slot_accepted
  , (flt.v.first_slot IS NULL OR c.slot_no >= flt.v.first_slot)
    AND (flt.v.last_slot IS NULL OR c.slot_no <= flt.v.last_slot)
    AND (NOT flt.v.unitary OR c.chain_delta = 1)
    AND coalesce(o.adoptions, 0) >= coalesce(flt.v.min_adoptions, 0)
    AND (flt.v.size_min IS NULL OR c.size >= flt.v.size_min)
    AND (flt.v.size_max IS NULL OR c.size <= flt.v.size_max)              AS accepted
FROM fl c
CROSS JOIN flt
LEFT JOIN obs o ON o.hash = c.hash
LEFT JOIN rivals r ON r.hash = c.hash;

-- locli's valid observations of the main chain's blocks (BlockObservation with
-- isValidBlockObservation: notice, request, fetch and adoption present and not
-- negative, chain length delta 1), seconds.
CREATE OR REPLACE MACRO locli_bp_observations(blocks) AS TABLE
SELECT
    b.hash, o.process AS observer
  , (epoch_ns(o.t_notice)   - epoch_ns(b.slot_start)) / 1e9 AS noticed
  , (epoch_ns(o.t_request)  - epoch_ns(o.t_notice))   / 1e9 AS requested
  , (epoch_ns(o.t_fetch)    - epoch_ns(o.t_request))  / 1e9 AS fetched
  , (epoch_ns(o.t_announce) - epoch_ns(o.t_fetch))    / 1e9 AS announced
  , (epoch_ns(o.t_send)     - epoch_ns(o.t_fetch))    / 1e9 AS sending
  , (epoch_ns(o.t_adopt)    - epoch_ns(o.t_fetch))    / 1e9 AS adopted
FROM query_table(blocks) b
JOIN locli_bp_entries() o ON o.hash = b.hash AND o.role = 'observer'
WHERE o.t_notice IS NOT NULL AND o.t_request IS NOT NULL AND o.t_fetch IS NOT NULL
  AND o.t_adopt IS NOT NULL
  AND o.t_notice  >= b.slot_start AND o.t_request >= o.t_notice
  AND o.t_fetch   >= o.t_request  AND o.t_adopt   >= o.t_fetch
  AND greatest(coalesce(o.request_len, 0), coalesce(o.adopt_len, 0)) = 1;

-- locli's CDF of a list of samples (Data.CDF.cdfZ over stdCentiles): size,
-- average, median, sample standard deviation, range and the standard
-- centiles, each the sorted sample at index min(floor(n * c), n - 1). An empty
-- list gives zeros, as in locli.
CREATE OR REPLACE MACRO locli_cdf(xs) AS (
  WITH s AS (SELECT list_sort(xs) AS v, len(xs) AS n)
  SELECT {
    'size':    n,
    'average': CASE WHEN n = 0 THEN 0 ELSE list_avg(v) END,
    'median':  CASE WHEN n = 0 THEN 0 ELSE v[least(floor(n * 0.5)::BIGINT, n - 1) + 1] END,
    'stddev':  CASE WHEN n < 2 THEN 0 ELSE list_aggregate(v, 'stddev_samp') END,
    'min':     CASE WHEN n = 0 THEN 0 ELSE v[1] END,
    'max':     CASE WHEN n = 0 THEN 0 ELSE v[n] END,
    'centiles': [ {'centile': c, 'value': CASE WHEN n = 0 THEN 0
                                               ELSE v[least(floor(n * c)::BIGINT, n - 1) + 1] END}
                  FOR c IN [0.0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.75, 0.8, 0.85,
                            0.875, 0.9, 0.925, 0.95, 0.97, 0.98, 0.99, 0.995, 0.997, 0.998, 0.999,
                            0.9995, 0.9997, 0.9998, 0.9999, 1.0] ]
  } FROM s);
