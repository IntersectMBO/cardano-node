-- "v_mempool_sizes": the mempool size gauge, per EVENT — no time grouping,
-- every trace as it happened. Every mempool trace states the pool's size
-- AFTER its event; AddedTx and RemoveTxs are the events that change the
-- pool, and their union is the gauge, verbatim.
-- RejectedTx is left out ON PURPOSE: a rejected tx never enters the pool,
-- so its trace reads the gauge but never moves it. Including it would add
-- rows that repeat the previous value and change nothing.
--
-- Leios nodes also trace Mempool.SwitchedToCertifyingView, which states the
-- pool's size after the node removed the txs of a certified EB. It moves the
-- gauge, often by most of the pool's content, so it is part of the union
-- here. Praos runs don't have it.
--
-- (Tier 1.)
--
-- Fields:
-- --------
-- * ts          The event's timestamp.
--   (NOT NULL)
--
-- * process     The node whose mempool this is.
--   (NOT NULL)
--
-- * bytes       The pool's content in bytes, after the event.
--   (NOT NULL)
--
-- * txs         The pool's content in transactions, after the event.
--   (NOT NULL)
--
-- Example (in chronological order):
------------------------------------
--
-- at    2026-10-18T18:39:09.907377259Z
-- ns    Mempool.AddedTx
-- data  { "mempoolSize":{ "bytes":374
--                       , "numTxs":1
--                       }
--       , "tx":{"txid":"702f5a52"}
--       }
--
-- at    2026-10-18T18:40:15.567923904Z
-- ns    Mempool.RemoveTxs
-- data  { "mempoolSize":{ "bytes":608872
--                       , "numTxs":1628
--                       }
--       , "txs":"[{\"tx\":{\"txid\":\"23ffe965\"}},{\".."
--       }
--
CREATE OR REPLACE VIEW v_mempool_sizes AS
SELECT ts, process, mempool_bytes(data) AS bytes, mempool_txs(data) AS txs
FROM raw
WHERE ns IN ('Mempool.AddedTx', 'Mempool.RemoveTxs', 'Mempool.SwitchedToCertifyingView');

COMMENT ON VIEW v_mempool_sizes IS 'tier 1: the mempool size gauge per event (AddedTx, RemoveTxs, Leios SwitchedToCertifyingView), after the event: ts, process, bytes, txs (previously: mempool_sizes)';
