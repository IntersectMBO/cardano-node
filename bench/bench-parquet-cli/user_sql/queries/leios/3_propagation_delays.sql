-- "3. Endorser-block propagation, the delays themselves": one row per peer
-- delivery — which endorser block, from which forger, to which node, how long
-- it took. The samples 3_propagation_total summarises, unaggregated.
--
-- USER CONTENT (user_sql/README.md): not reviewed or tested like sql/; the
-- Leios observables aren't final, so it may break with a node change.
--
-- Columns: eb, forger_process, taker_process, ms.
WITH
  forging AS MATERIALIZED (
    SELECT hash, min(ts) AS ts, any_value(process) AS forger
    FROM v_eb_forged GROUP BY hash),
  acquired AS MATERIALIZED (
    SELECT hash, process, min(ts) AS ts
    FROM v_eb_deliveries GROUP BY hash, process)
SELECT
    a.hash        AS eb
  , g.forger      AS forger_process
  , a.process     AS taker_process
  , (epoch_ns(a.ts) - epoch_ns(g.ts)) / 1e6 AS ms
FROM acquired a
JOIN forging g ON g.hash = a.hash
WHERE a.process <> g.forger
ORDER BY a.hash, a.process;
