-- "v_eb_forged": the AXIOMATIC view of every FORGING of an endorser block,
-- one row per "Consensus.LeiosKernel.BlockForged" event. Who forged it, when,
-- and what the forged EB IS.
--
-- Tier 1, Leios family: EXPERIMENTAL until the Leios observables are final.
--
-- Axiomatic means a property of the event or of the artifact as the protocol
-- defines it; the traces are only the evidence, and where no trace evidences a
-- value the cell is NULL, never inferred.
--
-- An endorser block is JUST a list of (tx hash, declared tx size) entries,
-- nothing else; no transaction body is ever part of an EB and its hash is the
-- hash OF THAT LIST.
--
-- An EB forged twice (same hash, same content, two forgers or moments) is two
-- rows, because two forgings happened. No matching through announcements: the
-- `ts` is the forging event's own, nothing else. The announcement linkage to a
-- ranking block lives in "v_block_forged".
--
-- THE HASH IDENTIFIES THE BLOCK. For a RANKING block it identifies the FORGING
-- too: that hash is of a header which already carries the slot and the issuing
-- key, so no two forgings can share it (not two nodes in one slot, and not one
-- node forging twice in a slot under different keys, as a dense pool can make
-- it do). For RBs the hash alone therefore references the block and the forging
-- alike, and a join on it cannot fan out.
-- For an ENDORSER block the hash identifies the EB ONLY. That hash is of its
-- (tx hash, size) list and nothing else (no slot, no key) so the same EB can
-- be forged more than once: by one node whose mempool has not changed, and in
-- principle by two nodes at the same moment whose mempools agree. It is still
-- ONE EB, but each forging is its own event, keyed (process, slot_no, hash).
--
--
-- Fields:
-- --------
-- * ts             When it was forged (Consensus.LeiosKernel.BlockForged).
--   (NOT NULL)
--
-- * process        The node that forged it.
--   (NOT NULL)
--
-- * hash           The EB identifier: hash of its (tx hash, size) list.
--   (NOT NULL)     The value the announcing header and any certificate name.
--
-- * slot_no        The slot the forging trace states (the same slot as the
--   (NOT NULL)     ranking block's Forge.Loop.ForgedBlock).
--
-- * slot_start     When that slot began, per genesis (slot_start()).
--   (NOT NULL)
--
-- * bytes          Size of the endorser block itself, serialized (tx hashes,
--   (NOT NULL)     no bodies): data.ebSize. What LeiosNotify announces as
--                  "ebBodySize" and LeiosFetch moves as the EB body.
--
-- * tx_refs_count  How many (tx hash, size) entries the EB holds: data.numTxs.
--   (NOT NULL)
--
-- Example (in chronological order):
-- --------
--
--   at    2026-10-13T23:24:00.881826336Z
--   ns    Consensus.LeiosKernel.BlockForged
--   data  { "hash":"38bd43.."
--         , "slot":1336
--         , "numTxs":2777
--         , "ebSize":99975, "closureSize":99975
--         , "mempoolRestSize":0
--         }
--
-- TODO: Add "tx_refs_bytes" column.
--       Ask for a fix to `closureSize`, its value is `ebSize` copied.
-- Some math on the example: `numTxs` 2777, so `ebSize` 99975 = 2777 x 36 + 3,
-- the encoded (tx hash, size) reference list, which is what `bytes` reports.
-- closureSize states the same number, although the bodies those 2777
-- references point at are far larger. It is not the closure size; it is
-- ebSize copied.
-- About `mempoolRestSize`: the node traces it as 0; not read here.
CREATE OR REPLACE VIEW v_eb_forged AS
SELECT
    ts                                       AS ts
  , process                                  AS process
  , data->>'$.hash'                          AS hash
  , slot_no(data)                            AS slot_no
  , slot_start(slot_no(data))                AS slot_start
  , CAST(data->>'$.ebSize' AS UBIGINT)       AS bytes
  , CAST(data->>'$.numTxs' AS UBIGINT)       AS tx_refs_count
FROM raw
WHERE ns = 'Consensus.LeiosKernel.BlockForged';

COMMENT ON VIEW v_eb_forged IS 'tier 1, Leios (experimental): every endorser-block forging, one row per Consensus.LeiosKernel.BlockForged: ts, process, hash, slot_no, slot_start, bytes, tx_refs_count (previously: block_endorser)';
