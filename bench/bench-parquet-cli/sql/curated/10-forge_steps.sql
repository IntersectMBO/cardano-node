-- Curated view: one row per slot a node led, with the forge loop's steps.
--
-- Rows:    (process, slot) for every Forge.Loop.NodeIsLeader. A dense pool can
--          lead a slot more than once; this view assumes one leadership per
--          process and slot and stops the query with an error otherwise.
-- Columns: instants of each step (TIMESTAMP_NS); NULL where the node didn't get
--          that far (e.g. no AdoptedBlock when the forged block wasn't adopted).
-- Drift absorbed here:
--   * namespace renames (Forge.Loop.TickedLedgerState -> ForgeTickedLedgerState,
--     Forge.Loop.MempoolSnapshot -> ForgingMempoolSnapshot): both spellings read.
--   * nothing relies on the deprecated "kind" field.
-- Reads the raw layer, not the per-namespace views, so absent namespaces are fine.
CREATE OR REPLACE VIEW forge_steps AS
WITH ev AS (
  SELECT process, ns, ts, data
  FROM raw
  WHERE ns IN ( 'Forge.Loop.StartLeadershipCheck'
              , 'Forge.Loop.NodeIsLeader'
              , 'Forge.Loop.TickedLedgerState',  'Forge.Loop.ForgeTickedLedgerState'
              , 'Forge.Loop.MempoolSnapshot',    'Forge.Loop.ForgingMempoolSnapshot'
              , 'Forge.Loop.ForgedBlock'
              , 'Forge.Loop.AdoptedBlock' )
)
SELECT
    l.process
  , l.slot
  , s.ts    AS started
  , l.ts    AS leader
  , t.ts    AS ticked
  , m.ts    AS mempool_snapshot
  , f.ts    AS forged
  , a.ts    AS adopted
  , f.data->>'$.block'                       AS block
  , CAST(f.data->>'$.blockNo'   AS UBIGINT)  AS block_no
  , CAST(a.data->>'$.blockSize' AS UBIGINT)  AS block_bytes
FROM      (SELECT process, slot_no(data) AS slot, ts
                , CASE WHEN count(*) OVER (PARTITION BY process, slot_no(data)) > 1
                       THEN error('forge_steps: ' || process || ' led slot ' || slot_no(data) || ' more than once')
                  END AS _unique
           FROM ev WHERE ns = 'Forge.Loop.NodeIsLeader') l
LEFT JOIN (SELECT process, slot_no(data) AS slot, min(ts) AS ts FROM ev
           WHERE ns = 'Forge.Loop.StartLeadershipCheck' GROUP BY ALL) s USING (process, slot)
LEFT JOIN (SELECT process, slot_no(data) AS slot, ts FROM ev
           WHERE ns IN ('Forge.Loop.TickedLedgerState', 'Forge.Loop.ForgeTickedLedgerState')) t USING (process, slot)
LEFT JOIN (SELECT process, slot_no(data) AS slot, ts FROM ev
           WHERE ns IN ('Forge.Loop.MempoolSnapshot', 'Forge.Loop.ForgingMempoolSnapshot')) m USING (process, slot)
LEFT JOIN (SELECT process, slot_no(data) AS slot, ts, data FROM ev WHERE ns = 'Forge.Loop.ForgedBlock') f USING (process, slot)
LEFT JOIN (SELECT process, slot_no(data) AS slot, ts, data FROM ev WHERE ns = 'Forge.Loop.AdoptedBlock') a USING (process, slot);
