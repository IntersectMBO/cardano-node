-- "4. Ranking-block propagation, the delays themselves": one row per peer, per
-- block — from the block's own FORGING to each milestone that peer reached. The
-- ranking-block twin of 3_propagation_delays, which does the same for endorser
-- blocks.
--
-- USER CONTENT (user_sql/README.md): not reviewed or tested like sql/; the
-- Leios observables aren't final, so it may break with a node change.
--
-- Columns: hash, block_no, slot_no, forger_process, taker_process,
-- to_noticed_ms, to_fetched_ms, to_adopted_ms, forge_offset_ms,
-- from_slot_adopted_ms.
SELECT
    t.hash
  , f.block_no
  , f.slot_no
  , t.forger_process
  , t.peer_process                                AS taker_process
  , (epoch_ns(t.peer_first_noticed_at) - epoch_ns(f.ts)) / 1e6 AS to_noticed_ms
  , (epoch_ns(t.peer_fetched_at) - epoch_ns(f.ts)) / 1e6 AS to_fetched_ms
  , (epoch_ns(t.peer_adopted_at) - epoch_ns(f.ts)) / 1e6 AS to_adopted_ms
  , (epoch_ns(f.ts) - epoch_ns(f.slot_start)) / 1e6 AS forge_offset_ms
  , (epoch_ns(t.peer_adopted_at) - epoch_ns(f.slot_start)) / 1e6 AS from_slot_adopted_ms
FROM v_block_adoptions_timing t
JOIN v_block_forged f ON f.hash = t.hash
WHERE t.peer_process IS NOT NULL
ORDER BY f.block_no, f.slot_no, t.hash, t.peer_process;
