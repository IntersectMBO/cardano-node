-- "v_gen_tx_confirmed": which transactions each block included, one row per
-- (block, tx), as the generator saw it. Tier 1, generator family:
-- EXPERIMENTAL until the tx-centrifuge observables are final.
--
-- The generator's recycler releases a tx's backlog when its observer sees the
-- tx CONFIRMED in a block, and that trace names the tx and the block
-- (TxCentrifuge.Recycler.Backlog.Released.Confirmed).
--
-- A TX CONFIRMED THROUGH A CERTIFIED ENDORSER BLOCK IS KNOWN BY HASH ONLY: the
-- hash is what the EB carries, and the generator never fetches the tx bodies
-- (it built every tx itself). So on the EB path the trace has no txId, by
-- design; on the body path it does, and it is the confirmed tx's own id
-- (equal to Builder.NewTx's txId for the same txHash). The id column therefore comes from the builder's own map
-- on both paths. The confirmed tx's identity is txHash. Blocks that certify an
-- endorser block carry no own txs; the EB's txs are confirmed under the
-- CERTIFYING block's height.
--
-- NOT EVERY CHAIN-INCLUDED TX GETS A ROW: a confirm for a key the backlog no
-- longer holds is ignored, so a tx that was earlier orphan-released (it sat in
-- a doomed fork too) never confirms here. A Recycler.Reset would also clear
-- held keys wholesale.
--
-- NO slot_start HERE. The row is a confirmation, not a forging: its slot is
-- the BLOCK's identity, carried so the row can be read against the forging
-- (join v_block_forged on block_no / slot_no), not a clock for when it was
-- confirmed.
--
-- SHAPE: the builder's and the recycler's rows are unioned and grouped once by
-- hash: a hash is built once and confirmed once, and a tx the builder never
-- logged keeps its row with id NULL.
--
-- Reads raw_generator (the generator's own log), never the nodes'.
--
-- Fields:
-- --------
-- * ts          When the observer saw the confirmation, not when the block was
--   (NOT NULL)  forged.
--
-- * id          The confirmed tx's wire identifier, the builder's statement for
--   (NULL)      that hash; NULL would be a confirmed tx the builder never
--               logged.
--
-- * hash        The confirmed tx's hash (txHash, verbatim).
--   (NOT NULL)
--
-- * block_no    The including block's height, per the trace.
--   (NOT NULL)
--
-- * slot_no     The including block's slot, per the trace.
--   (NOT NULL)
--
-- Example:
-- --------
-- ns    TxCentrifuge.Recycler.Backlog.Released.Confirmed
-- data  {"backlog":10944,"blockNo":250,"count":2,"inputs":[...],"pipe":"node-21",
--        "recycler":"node-21","slotNo":4713,"txHash":"47590d.."}
CREATE OR REPLACE VIEW v_gen_tx_confirmed AS
SELECT
    max(ts)        AS ts
  , max(id)        AS id
  , hash
  , max(block_no)  AS block_no
  , max(slot_no)   AS slot_no
FROM
  ( SELECT NULL::TIMESTAMP_NS AS ts, data->>'$.txId' AS id, data->>'$.txHash' AS hash,
           NULL::UBIGINT AS block_no, NULL::UBIGINT AS slot_no
    FROM raw_generator
    WHERE ns IN ('TxCentrifuge.Builder.NewTx')
    UNION ALL
    SELECT ts, NULL, data->>'$.txHash',
           CAST(data->>'$.blockNo' AS UBIGINT), CAST(data->>'$.slotNo' AS UBIGINT)
    FROM raw_generator
    WHERE ns IN ('TxCentrifuge.Recycler.Backlog.Released.Confirmed') )
GROUP BY hash
HAVING max(block_no) IS NOT NULL;

COMMENT ON VIEW v_gen_tx_confirmed IS 'tier 1, generator (experimental): one row per tx confirmed in a block, per the generator: ts, id, hash, block_no, slot_no (previously: tx_confirmed)';
