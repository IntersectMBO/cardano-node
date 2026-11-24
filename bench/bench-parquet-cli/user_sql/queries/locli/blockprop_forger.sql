-- "locli_blockprop_forger(...)": locli's forger-side block propagation, the
-- cdfForger* CDFs of analysis/blockprop.json, one row per CDF. Needs
-- blockprop.sql; filters as described there.
--
--   SELECT * FROM locli_blockprop_forger(filters := locli_filter(unitary := true, min_adoptions := 5));
--
-- PURPOSE: cross-checking locli (see blockprop.sql). User content
-- (user_sql/README.md): not reviewed like sql/.
--
-- ONE SAMPLE PER ACCEPTED MAIN-CHAIN BLOCK, from its forger's events (locli's
-- BlockForge), seconds; a step whose traces are missing gives no sample:
--   cdfForgerStart        slot start -> StartLeadershipCheck
--   cdfForgerBlkCtx       StartLeadershipCheck -> BlockContext
--   cdfForgerLgrState     BlockContext -> LedgerState
--   cdfForgerLgrView      LedgerState -> LedgerView
--   cdfForgerLead         LedgerView (else StartLeadershipCheck) -> NodeIsLeader
--   cdfForgerTicked       NodeIsLeader -> ForgeTickedLedgerState
--   cdfForgerMemSnap      ForgeTickedLedgerState -> ForgingMempoolSnapshot
--   cdfForgerForge        ForgingMempoolSnapshot (else NodeIsLeader) -> ForgedBlock
--   cdfForgerAnnounce     ForgedBlock -> first announcement (ServerHeader.Update)
--   cdfForgerAnnounceCum  slot start -> first announcement
--   cdfForgerSend         ForgedBlock -> first BlockFetch.Server.SendBlock
--   cdfForgerAdoption     ForgedBlock -> first adoption (AddedToCurrentChain or AdoptedBlock)
--
-- Fields:
-- --------
-- * metric    The blockprop.json field.
-- * size, average, median, stddev, min, max   As locli's CDF (cdfSize,
--             cdfAverage, cdfMedian, cdfStddev, cdfRange).
-- * centiles  The standard centiles: list of {centile, value} (cdfSamples).
CREATE OR REPLACE MACRO locli_blockprop_forger(filters := NULL) AS TABLE
WITH c AS MATERIALIZED (SELECT * FROM locli_bp_chain(filters := filters) WHERE accepted)
SELECT r.metric AS metric, r.cdf.size AS size, r.cdf.average AS average, r.cdf.median AS median, r.cdf.stddev AS stddev, r.cdf.min AS min, r.cdf.max AS max, r.cdf.centiles AS centiles
FROM (
  SELECT unnest([
      {'metric': 'cdfForgerStart',       'cdf': locli_cdf(list(forger_start)        FILTER (WHERE forger_start        IS NOT NULL))},
      {'metric': 'cdfForgerBlkCtx',      'cdf': locli_cdf(list(forger_blkctx)       FILTER (WHERE forger_blkctx       IS NOT NULL))},
      {'metric': 'cdfForgerLgrState',    'cdf': locli_cdf(list(forger_lgrstate)     FILTER (WHERE forger_lgrstate     IS NOT NULL))},
      {'metric': 'cdfForgerLgrView',     'cdf': locli_cdf(list(forger_lgrview)      FILTER (WHERE forger_lgrview      IS NOT NULL))},
      {'metric': 'cdfForgerLead',        'cdf': locli_cdf(list(forger_lead)         FILTER (WHERE forger_lead         IS NOT NULL))},
      {'metric': 'cdfForgerTicked',      'cdf': locli_cdf(list(forger_ticked)       FILTER (WHERE forger_ticked       IS NOT NULL))},
      {'metric': 'cdfForgerMemSnap',     'cdf': locli_cdf(list(forger_memsnap)      FILTER (WHERE forger_memsnap      IS NOT NULL))},
      {'metric': 'cdfForgerForge',       'cdf': locli_cdf(list(forger_forge)        FILTER (WHERE forger_forge        IS NOT NULL))},
      {'metric': 'cdfForgerAnnounce',    'cdf': locli_cdf(list(forger_announce)     FILTER (WHERE forger_announce     IS NOT NULL))},
      {'metric': 'cdfForgerAnnounceCum', 'cdf': locli_cdf(list(forger_announce_cum) FILTER (WHERE forger_announce_cum IS NOT NULL))},
      {'metric': 'cdfForgerSend',        'cdf': locli_cdf(list(forger_send)         FILTER (WHERE forger_send         IS NOT NULL))},
      {'metric': 'cdfForgerAdoption',    'cdf': locli_cdf(list(forger_adoption)     FILTER (WHERE forger_adoption     IS NOT NULL))}
    ]) AS r
  FROM c);
