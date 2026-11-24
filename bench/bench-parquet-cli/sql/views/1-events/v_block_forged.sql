-- "v_block_forged": the AXIOMATIC view of every FORGING of a ranking block, one
-- row per "Forge.Loop.ForgedBlock" trace / event. Who forged it, when, and what
-- the forged block IS and CARRIES per the chain and the protocol.
-- Axiomatic means a property of the event or of the artifact as the protocol
-- defines it; the traces are only the evidence, and where no trace evidences a
-- value the cell is NULL, never inferred.
--
-- A block that lost its height battle is still a block; every forging is a row.
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
-- (Tier 1.)
--
-- Fields:
-- --------
-- * ts                 When it was forged ("Forge.Loop.ForgedBlock"'s ts).
--   (NOT NULL)
--
-- * process            The node that forged it.
--   (NOT NULL)
--
-- * hash               The block identifier.
--   (NOT NULL)
--
-- * prev_hash          The predecessor its header names.
--   (NOT NULL)
--
-- * block_no           Its height, from its header.
--   (NOT NULL)
--
-- * slot_no            Its slot, from its header.
--   (NOT NULL)
--
-- * slot_start         When the slot named above began, per genesis
--   (NOT NULL)         (slot_start()); the same for every node.
--
-- * bytes              The whole serialized block's size
--   (NULL)             ("Forge.Loop.AdoptedBlock"). NULL when the forger
--                      discarded the block unadopted ("DidntAdoptBlock"). An
--                      EB body is NOT part of it, only the announcement is.
--                      The adoptions themselves, self and peer, live in
--                      "v_block_adoptions".
--
-- * announced_eb_hash  The endorser block announced in the header (Leios): when
--   (NULL)             the forger has txs to additionally forge an EB, the
--                      header carries its announcement / hash. NULL when the
--                      header announces none, and on every Praos run.
--                      EB forging details are in "v_eb_forged".
--
-- * certified_eb_hash  The endorser block this block certifies, if the EB
--   (NULL)             announced in the previous block reached quorum on time
--                      (Leios). The body carries the certificate itself. NULL
--                      when the block carries no certificate, and on every
--                      Praos run.
--
-- Example (in chronological order):
------------------------------------
--
-- at    2026-10-13T23:24:00.004740982Z
-- ns    Consensus.LeiosKernel.BlockCertified
-- data  { "ebHash":"258a89.."
--       , "atSlot":1336
--       , "ebSlot":1303
--       }
--
-- at    2026-10-13T23:24:00.881826336Z
-- ns    Consensus.LeiosKernel.BlockForged
-- data  { "hash":"38bd43.."
--       , "slot":1336
--       , "numTxs":2777
--       , "ebSize":99975, "closureSize":99975
--       , "mempoolRestSize":0
--       }
--
-- at    2026-10-13T23:24:00.891113106Z
-- ns    Consensus.LeiosKernel.BlockAnnounced
-- data  { "rbHash":"0f00dc.."
--       , "ebHash":"38bd43.."
--       , "ebSlot":1336
--       }
-- (Not a wire announcement event, only tracing the relation between EB and RB).
--
-- at    2026-10-13T23:24:00.891159760Z
-- ns    Forge.Loop.ForgedBlock
-- data  { "block":"0f00dc.."
--       , "blockNo":74
--       , "blockPrev":"3c0cb8.."
--       , "slot":1336
--       }
--
-- at    2026-10-13T23:24:01.125825293Z
-- ns    Forge.Loop.AdoptedBlock
-- data  { "blockHash":"0f00dc.."
--       , "blockSize":964
--       , "slot":1336
--       }
--
-- The other case: a forging the forger discarded
-- (sev is Error and the trace names no hash).
-- at    2026-10-14T00:24:47.993817067Z
-- ns    Forge.Loop.DidntAdoptBlock
-- data  {"slot":4983}
--
CREATE OR REPLACE VIEW v_block_forged AS
SELECT
    f.ts                                       AS ts
  , f.process                                  AS process
  , f.data->>'$.block'                         AS hash
  , f.data->>'$.blockPrev'                     AS prev_hash
  , CAST(f.data->>'$.blockNo' AS UBIGINT)      AS block_no
  , slot_no(f.data)                            AS slot_no
  , slot_start(slot_no(f.data))                AS slot_start
  , CAST(a.data->>'$.blockSize' AS UBIGINT)    AS bytes
  , ann.data->>'$.ebHash'                      AS announced_eb_hash
  , cert.data->>'$.ebHash'                     AS certified_eb_hash
FROM      (SELECT * FROM raw WHERE ns = 'Forge.Loop.ForgedBlock') f
-- The self-adoption, if any; traced on the forger's node only.
LEFT JOIN (SELECT * FROM raw WHERE ns = 'Forge.Loop.AdoptedBlock') a
       ON (f.data->>'$.block') = (a.data->>'$.blockHash')
-- The EB the block announces, if any (Leios).
LEFT JOIN (SELECT * FROM raw WHERE ns = 'Consensus.LeiosKernel.BlockAnnounced') ann
       ON (f.data->>'$.block') = (ann.data->>'$.rbHash')
-- The EB the block certifies, if any (Leios). BlockCertified names no block
-- hash, only atSlot, the slot of the block being forged when it fires. A slot
-- alone is ambiguous (several nodes can forge in one slot), but the trace fires
-- on the certifying forger's own node during that forge, and a node forges at
-- most once per slot, so (process, atSlot) identifies exactly one forging.
LEFT JOIN (SELECT * FROM raw WHERE ns = 'Consensus.LeiosKernel.BlockCertified') cert
       ON     f.process       = cert.process
          AND slot_no(f.data) = CAST(cert.data->>'$.atSlot' AS UBIGINT);

COMMENT ON VIEW v_block_forged IS 'tier 1: every block forging, one row per Forge.Loop.ForgedBlock: ts, process, hash, prev_hash, block_no, slot_no, slot_start, bytes; Leios EB columns (previously: block_ranking)';
