-- "v_eb_validated": every "Consensus.LeiosKernel.EbValidated" the
-- kernel traced, one row per validation, with the block's own facts beside
-- it. It is the only trace that says how much work an endorser block COST a
-- node to validate, split into the transactions it had to apply in full and
-- the ones its cache let it skip.
--
-- Tier 2, Leios family: EXPERIMENTAL until the Leios observables are final.
--
-- THE TRACE FIRES WHEN THE BLOCK'S TRANSACTIONS HAVE ALL APPLIED and the
-- node is about to vote for it. A block whose closure never completed, or
-- that the node never got to vote on, has no row from that node.
-- applied IS THE WORK, reapplied IS THE SAVING. applied counts the
-- transactions this node validated in full; reapplied counts the ones the
-- LeiosTxCache had already validated, so a cheap re-apply sufficed.
-- applied + reapplied = refs ON EVERY ROW, so the two partition the
-- block's references: every referenced transaction was either applied or
-- reapplied, none counted twice.
-- THIS IS NOT v_eb_bodyhits' validated COLUMN. That one is taken
-- at the body insert and counts what was ALREADY validated then, a prior
-- state; this is taken at validation and counts what happened. The two do
-- not agree, and the one here is the one that answers "how many
-- transactions did this block make this node validate".
-- reapplied IS USUALLY AT LEAST THE NODE'S mempool_hits for the same block:
-- whatever the mempool supplied was already validated, and anything that
-- arrived between the insert and the validation adds to it. It is not a
-- guarantee: a row can have reapplied below mempool_hits.
-- THE FORGER HAS A ROW HERE, unlike v_eb_bodyhits. A node must
-- validate a block to vote for it, its own included, so process equals
-- forger_process on those rows rather than being excluded. Every row that is
-- not a forger's own has a v_eb_bodyhits row to match it.
-- A (BLOCK, NODE) PAIR CAN HAVE SEVERAL ROWS, and so can a (block, slot,
-- node) triple: a node can validate the same forging more than once. Each of
-- those happened, so each is a row, and nothing here picks a first.
-- A BLOCK NOBODY VALIDATED STILL HAS A ROW, the validation columns NULL, so
-- every forged endorser block is accounted for. Those are not an artefact:
-- they are the blocks that never reached a vote.
-- Axiomatic means a property of the event as the protocol defines it; the
-- traces are only the evidence, and where no trace evidences a value the cell
-- is NULL, never inferred.
--
-- NO slot_start HERE. A slot start is the instant a slot was SCHEDULED
-- for, and the row below is a validation, not a forging: its slot is the
-- BLOCK's identity, carried so the row can be read against the forging,
-- not a clock for when it was validated. v_block_forged and v_eb_forged
-- carry slot_start because their ts IS the forging the slot scheduled;
-- join one of them on hash to measure against it.
--
--
-- Fields:
-- --------
-- * hash            The endorser block.
--   (NOT NULL)
--
-- * slot_no         Its slot: the trace's own ebSlot on a validation row; on
--   (NULL)          the row of a block nobody validated, the forging's slot,
--                   NULL when it was forged more than once.
--
-- * forgings        How many times this hash was forged.
--   (NOT NULL)
--
-- * forger_process  The node that forged it.
--   (NOT NULL)
--
-- * process         The node that validated it (the forger included: every
--   (NULL)          voter validates, its own block too). NULL on the row of a
--                   block nobody validated.
--
-- * ts              When the validation was traced. NULL as process.
--   (NULL)
--
-- * refs            How many transactions the block references (v_eb_forged's
--   (NOT NULL)      tx_refs_count).
--
-- * applied         How many of them this validation applied afresh, and
-- * reapplied       how many it re-applied (already validated, e.g. from the
--   (NULL)          mempool). NULL as process.
--
-- Example (in chronological order):
-- --------
--
-- at    2026-10-23T20:10:16.275038341Z
-- ns    Consensus.LeiosKernel.BlockForged
-- data  { "closureSize":25718
--       , "ebSize":25718
--       , "hash":"7f9895.."
--       , "mempoolRestSize":0
--       , "numTxs":695
--       , "slot":3832
--       }
-- (hash, slot_no, forger_process, forgings and refs, through v_eb_forged.)
--
-- at    2026-10-23T20:10:19.698016145Z
-- ns    Consensus.LeiosKernel.EbValidated
-- data  { "applied":649
--       , "ebHash":"7f9895.."
--       , "ebSlot":3832
--       , "reapplied":46
--       }
-- (process, ts, applied, reapplied. 649 + 46 = 695, the partition. This is the
-- same block and the same node as v_eb_bodyhits' example, 3.3 s
-- later: there the node held 46 of the references in its mempool and was
-- missing 649. Here it fully validated exactly those 649 and the cache
-- spared it exactly those 46 -- a clean correspondence, but NOT the rule,
-- because transactions keep arriving between the insert and the
-- validation. The usual relation is the weaker form, reapplied >=
-- mempool_hits. Note also that the same node's
-- bodyhits validated column read 14, nothing like reapplied's 46, which is
-- why that column cannot stand in for this one.)
--
CREATE OR REPLACE VIEW v_eb_validated AS
WITH
  forged AS MATERIALIZED (
    -- One row per EB hash. An EB hash has one forger (a re-forge is the same
    -- node forging an unchanged tx list again); a second forger stops the
    -- query rather than being hidden. The slot only when one forging states it.
    SELECT hash
         , CASE WHEN count(DISTINCT process) = 1 THEN any_value(process)
                ELSE error('EB ' || hash || ' forged by more than one node') END AS process
         , CASE WHEN count(*) = 1 THEN min(slot_no) END                         AS slot_no
         , count(*)                                                             AS forgings
         , CASE WHEN count(DISTINCT tx_refs_count) = 1 THEN any_value(tx_refs_count)
                ELSE error('EB ' || hash || ' forged with different reference counts') END AS refs
    FROM v_eb_forged
    GROUP BY hash),
  validated AS MATERIALIZED (
    -- Every validation as traced, repeats included.
    SELECT data->>'$.ebHash'                        AS hash
         , CAST(data->>'$.ebSlot'    AS UBIGINT)    AS slot_no
         , process                                  AS process
         , ts                                       AS ts
         , CAST(data->>'$.applied'   AS UBIGINT)    AS applied
         , CAST(data->>'$.reapplied' AS UBIGINT)    AS reapplied
    FROM raw
    WHERE ns = 'Consensus.LeiosKernel.EbValidated')
-- Every validation, with the forging's facts attached.
SELECT v.hash, v.slot_no, f.forgings, f.process AS forger_process
     , v.process, v.ts, f.refs, v.applied, v.reapplied
FROM validated v
JOIN forged f ON f.hash = v.hash
UNION ALL
-- The blocks nobody validated: one row each, validation columns NULL.
SELECT f.hash, f.slot_no, f.forgings, f.process, NULL, NULL, f.refs, NULL, NULL
FROM forged f
ANTI JOIN validated v ON v.hash = f.hash;

COMMENT ON VIEW v_eb_validated IS 'tier 2, Leios (experimental): every Consensus.LeiosKernel.EbValidated with the forging''s facts, one row per validation (one with NULL validation for an EB nobody validated): applied, reapplied (previously: block_endorser_validated)';
