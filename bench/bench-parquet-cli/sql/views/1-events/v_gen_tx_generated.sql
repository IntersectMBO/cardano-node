-- "v_gen_tx_generated": every transaction the generator built, one row per
-- TxCentrifuge.Builder.NewTx, verbatim. Tier 1, generator family:
-- EXPERIMENTAL until the tx-centrifuge observables are final.
--
-- The generator names a tx two ways and both are axiomatic content of the tx:
-- id is the wire identifier (the one tx-submission announces and delivers),
-- hash the transaction hash. The builder states no size; a tx's declared size
-- appears only on the delivery wire, per v_gen_tx_sent.
--
-- Reads raw_generator (the generator's own log), never the nodes'.
--
-- Fields:
-- --------
-- * ts          When the generator built it.
--   (NOT NULL)
--
-- * id          The tx's wire identifier (data.txId).
--   (NOT NULL)
--
-- * hash        The tx's hash (data.txHash).
--   (NOT NULL)
--
-- Example:
-- --------
-- ns    TxCentrifuge.Builder.NewTx
-- data  {"builder":"node-17","destinations":[...],"inputs":[...],
--        "outputs":[...],"txHash":"c145df..","txId":"441e08.."}
CREATE OR REPLACE VIEW v_gen_tx_generated AS
SELECT
    ts
  , data->>'$.txId'    AS id
  , data->>'$.txHash'  AS hash
FROM raw_generator
WHERE ns IN ('TxCentrifuge.Builder.NewTx');

COMMENT ON VIEW v_gen_tx_generated IS 'tier 1, generator (experimental): every tx the generator built: ts, id, hash (previously: tx_generated)';
