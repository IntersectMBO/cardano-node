-- "v_gen_tx_sent": every transaction delivery from the generator, one row per
-- tx in each TxCentrifuge.TxSubmission.ReplyTxs, verbatim. Tier 1, generator
-- family: EXPERIMENTAL until the tx-centrifuge observables are final.
--
-- A DELIVERY'S WIRE ENTRY IS {id, size} AND NOTHING ELSE, so that is what a
-- row states. The tx's hash is deliberately NOT here: the wire never carries
-- it, and v_gen_tx_generated is the id -> hash map (join on id).
--
-- Reads raw_generator (the generator's own log), never the nodes'.
--
-- Fields:
-- --------
-- * ts          When the generator delivered it.
--   (NOT NULL)
--
-- * id          The tx's wire identifier (the delivery's entry).
--   (NOT NULL)
--
-- * bytes       The tx's size, from the same {id, size} entry.
--   (NOT NULL)
--
-- * target      The node the generator delivered it to (data.target, verbatim:
--   (NOT NULL)  the generator names its peers, e.g. "node-42").
--
-- Example:
-- --------
-- ns    TxCentrifuge.TxSubmission.ReplyTxs
-- data  {"target":"node-25","txs":[{"id":"8a9657..","size":370},...]}
CREATE OR REPLACE VIEW v_gen_tx_sent AS
SELECT
    ts
  , tx->>'$.id'                      AS id
  , CAST(tx->>'$.size' AS UBIGINT)   AS bytes
  , target
FROM (SELECT ts, data->>'$.target' AS target, unnest(data->'$.txs[*]') AS tx
      FROM raw_generator
      WHERE ns IN ('TxCentrifuge.TxSubmission.ReplyTxs'));

COMMENT ON VIEW v_gen_tx_sent IS 'tier 1, generator (experimental): one row per tx the generator delivered: ts, id, bytes, target (previously: tx_sent)';
