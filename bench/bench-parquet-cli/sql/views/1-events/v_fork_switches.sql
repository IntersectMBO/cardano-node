-- "v_fork_switches": the AXIOMATIC view of every FORK SWITCH, one row per
-- ChainDB.AddBlockEvent.SwitchedToAFork — a node rolling its chain back to
-- take a better one. It describes the event in the trace's own terms and
-- needs nothing else: no forging, no adoption, no block hash, so no
-- "hash@slot" string to split. The two select views the trace carries say
-- what the node took and what it gave up, and reason says why selection
-- preferred the one over the other.
-- A block is identified here the way chain selection identifies it, by
-- (slot, issuer): one issuer forges at most one block in a slot. The hash
-- is deliberately absent — the trace states it only inside its newtip
-- string. It joins to the other views by what it does name: the HEIGHT and
-- the SLOT the fork was fought at. Both v_block_forged and
-- v_block_adoptions carry block_no and slot_no, so
--   WHERE block_no NOT IN (SELECT new_block_no FROM v_fork_switches
--                          UNION SELECT old_block_no FROM v_fork_switches)
-- drops everything that happened at a height some node rolled back over —
-- forgings or adoptions, whichever view it is run against. Filtering this
-- way is wider than asking which blocks were themselves rolled back for,
-- and usually what is wanted: a height carries BOTH sides of its battle,
-- the block that won and the rivals that lost, and this drops them
-- together. Add WHERE battle IS NOT NULL to the subquery to drop only the
-- heights that were contested, leaving the ones where a node merely rolled
-- back onto a longer chain (reason Longer).
-- Axiomatic means a property of the event as the protocol defines it; the
-- traces are only the evidence, and where no trace evidences a value the
-- cell is NULL, never inferred.
--
-- NO slot_start HERE. A slot start is the instant a slot was SCHEDULED
-- for, and the row below is a rollback, not a forging: its slot is the
-- BLOCK's identity, carried so the row can be read against the forging,
-- not a clock for when it was switched. v_block_forged and "v_eb_forged"
-- carry slot_start because their ts IS the forging the slot scheduled;
-- join one of them on hash to measure against it.
--
-- (Tier 1.)
--
-- Fields:
-- --------
-- * ts             When the node switched.
--   (NOT NULL)
--
-- * process        The node that switched. Every node that takes a block it
--   (NOT NULL)     had a rival for emits its own event, so one battle appears
--                  here once per node that lived through it.
--
-- * battle         What kind of contest the switch settled, named from the
--   (NULL)         two select views: 'slot' when both sides sat in the very
--                  same slot (two leaders elected for one slot), 'height'
--                  when they sat at the same blockNo in different slots.
--                  NULL when the switch settled no contest at all (see
--                  reason).
--
-- * reason         Why selection preferred the new chain, verbatim
--   (NOT NULL)     (data.reason.reason). VRFTiebreak settles a battle, and
--                  the losing and winning VRF outputs are in the trace
--                  (data.reason.our and .candidate) for anyone auditing the
--                  comparison. Longer means the new chain was simply longer,
--                  an ordinary rollback and not a battle.
--
-- * old_block_no   The height, slot and issuer key hash of the tip the node
-- * old_slot_no    moved FROM (data.oldSuffixSelectView). Comparing
-- * old_issuer     old_issuer with new_issuer says whether the node gave up
--   (NOT NULL)     another node's block or its own.
--
-- * new_block_no   The same for the tip it moved TO
-- * new_slot_no    (data.newSuffixSelectView).
-- * new_issuer
--   (NOT NULL)
--
-- Example (in chronological order):
------------------------------------
--
-- at    2026-10-18T18:50:04.800310350Z
-- ns    ChainDB.AddBlockEvent.SwitchedToAFork
-- data  { "newSuffixSelectView":
--         { "blockNo":51
--         , "issueNo":0
--         , "issuerHash":"5cd731.."
--         , "kind":"PraosTiebreakerView"
--         , "slotNo":1125
--         , "tieBreakVRF":"0626b9.."
--         , "weightBoost":0
--         }
--       , "newtip":"99be39..@1125"
--       , "oldSuffixSelectView":
--         { "blockNo":51
--         , "issueNo":0
--         , "issuerHash":"3f7cef.."
--         , "kind":"PraosTiebreakerView"
--         , "slotNo":1125
--         , "tieBreakVRF":"bd384c.."
--         , "weightBoost":0
--         }
--       , "reason":{ "candidate":"OutputVRF {getOutputVRFByteArr.."
--                  , "era":"Dijkstra"
--                  , "our":"OutputVRF {getOutputVRFByteArr.."
--                  , "reason":"VRFTiebreak"
--                  }
--       }
--
CREATE OR REPLACE VIEW v_fork_switches AS
SELECT
    ts                                                         AS ts
  , process                                                    AS process
  , CASE
      WHEN (data->>'$.reason.reason') <> 'VRFTiebreak'         THEN NULL
      WHEN (data->>'$.oldSuffixSelectView.slotNo')
         = (data->>'$.newSuffixSelectView.slotNo')              THEN 'slot'
      ELSE                                                           'height'
    END                                                        AS battle
  , data->>'$.reason.reason'                                   AS reason
  , CAST(data->>'$.oldSuffixSelectView.blockNo' AS UBIGINT)    AS old_block_no
  , CAST(data->>'$.oldSuffixSelectView.slotNo'  AS UBIGINT)    AS old_slot_no
  , data->>'$.oldSuffixSelectView.issuerHash'                  AS old_issuer
  , CAST(data->>'$.newSuffixSelectView.blockNo' AS UBIGINT)    AS new_block_no
  , CAST(data->>'$.newSuffixSelectView.slotNo'  AS UBIGINT)    AS new_slot_no
  , data->>'$.newSuffixSelectView.issuerHash'                  AS new_issuer
FROM raw
WHERE ns = 'ChainDB.AddBlockEvent.SwitchedToAFork';

COMMENT ON VIEW v_fork_switches IS 'tier 1: every fork switch (SwitchedToAFork), described by height, slot and issuer of both sides: ts, process, battle, reason, old_*, new_* (previously: fork_switches)';
