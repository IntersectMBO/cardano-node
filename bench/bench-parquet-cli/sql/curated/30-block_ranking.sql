-- Curated view "block_ranking", ported from Federico's block_ranking.sql (Leios).
--
-- One row per forging of a ranking block (Forge.Loop.ForgedBlock): who forged
-- it, when, and what the block is and carries. Where no trace evidences a value,
-- the cell is NULL, never inferred. A block that lost its height battle is still
-- a row.
--
-- Joins, as in the original:
--   * Forge.Loop.AdoptedBlock by block hash (traced by the forger only);
--   * Consensus.LeiosKernel.BlockAnnounced by rbHash: the EB the block announces;
--   * Consensus.LeiosKernel.BlockCertified by (process, atSlot): it names no block
--     hash, only the slot of the block being forged, and fires on the forger's own
--     node during that forge, so (process, slot) pins the forging.
--
-- Columns: at (TIMESTAMP_NS), process, hash, prev_hash, block_no, slot_no,
--   slot_start (TIMESTAMP_NS, from "slot"), bytes (NULL when the forger didn't
--   adopt it), announced_eb_hash, certified_eb_hash.
-- Differences from the original: timestamps are TIMESTAMP_NS instead of integer
-- nanoseconds; "process" (the log's directory) instead of a file index.
CREATE OR REPLACE VIEW block_ranking AS
SELECT
    f."at"                                     AS "at"
  , f.process                                  AS process
  , f.data->>'$.block'                         AS hash
  , f.data->>'$.blockPrev'                     AS prev_hash
  , CAST(f.data->>'$.blockNo' AS UBIGINT)      AS block_no
  , slot_no(f.data)                            AS slot_no
  , sl.slot_start                              AS slot_start
  , CAST(a.data->>'$.blockSize' AS UBIGINT)    AS bytes
  , ann.data->>'$.ebHash'                      AS announced_eb_hash
  , cert.data->>'$.ebHash'                     AS certified_eb_hash
FROM      (SELECT * FROM raw WHERE ns = 'Forge.Loop.ForgedBlock') f
JOIN      slot sl
       ON sl.slot_no = slot_no(f.data)
LEFT JOIN (SELECT * FROM raw WHERE ns = 'Forge.Loop.AdoptedBlock') a
       ON (f.data->>'$.block') = (a.data->>'$.blockHash')
LEFT JOIN (SELECT * FROM raw WHERE ns = 'Consensus.LeiosKernel.BlockAnnounced') ann
       ON (f.data->>'$.block') = (ann.data->>'$.rbHash')
LEFT JOIN (SELECT * FROM raw WHERE ns = 'Consensus.LeiosKernel.BlockCertified') cert
       ON     f.process     = cert.process
          AND slot_no(f.data) = CAST(cert.data->>'$.atSlot' AS UBIGINT);
