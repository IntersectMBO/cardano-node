-- "locli_blockprop_diff(json_path, ...)": the cross-check itself. Compares
-- every CDF of locli_blockprop_forger / _peers / _endtoend / _control with the
-- same CDF in a locli analysis of the same run (analysis/blockprop.json), one
-- row per (metric, statistic). Needs blockprop.sql and the four section
-- files; filters (blockprop.sql) must be the ones that analysis used
-- (summary.json .sumFilters; blockprop.sql has the conversion of locli's
-- relative filters).
--
--   SELECT * FROM locli_blockprop_diff('RUN/analysis/blockprop.json',
--                                      filters := locli_filter(unitary := true, min_adoptions := 5))
--   WHERE NOT ok;
--
-- PURPOSE: cross-checking locli. User content (user_sql/README.md): not
-- reviewed like sql/.
--
-- TOLERANCE: locli stores timestamps as REAL epoch seconds in its per-node
-- DBs, which rounds them to about a quarter of a microsecond; a time
-- difference can therefore be off by up to about half a microsecond. ok is
-- true when the two values differ by at most abs_tol (default 1e-6, seconds
-- for times) or by rel_tol (default 1e-9) of locli's value. Counts must match
-- exactly. The domain rows compare the main chain's block and slot ranges
-- and counts with bpDomainBlocks / bpDomainSlots.
--
-- Fields:
-- --------
-- * metric     As in the four macros ("cdfForgerStart", "bpPropagation.cdf0.50", ...).
-- * statistic  size, average, median, stddev, min, max, or "centile 0.95" etc.
-- * ours       This side's value.
-- * locli      The value in blockprop.json (NULL when it has no such field).
-- * ok         Within tolerance.
CREATE OR REPLACE MACRO locli_blockprop_diff(json_path, filters := NULL, abs_tol := 1e-6, rel_tol := 1e-9) AS TABLE
WITH
  j AS (SELECT content::JSON AS j FROM read_text(json_path)),
  ours AS (
              SELECT * FROM locli_blockprop_forger(filters := filters)
    UNION ALL SELECT * FROM locli_blockprop_peers(filters := filters)
    UNION ALL SELECT * FROM locli_blockprop_endtoend(filters := filters)
    UNION ALL SELECT * FROM locli_blockprop_control(filters := filters)),
  cdfs AS (
    SELECT o.*,
           json_extract(j.j, '/' || regexp_replace(o.metric, '^bpPropagation\.', 'bpPropagation/')) AS c
    FROM ours o, j
    WHERE o.metric NOT LIKE 'domain.%'),
  stats AS (
              SELECT metric, 'size' AS statistic, size::DOUBLE AS ours, (c->>'cdfSize')::DOUBLE AS locli, true AS exact FROM cdfs
    UNION ALL SELECT metric, 'average', average, (c->>'cdfAverage')::DOUBLE, false FROM cdfs
    UNION ALL SELECT metric, 'median',  median,  (c->>'cdfMedian')::DOUBLE, false FROM cdfs
    UNION ALL SELECT metric, 'stddev',  stddev,  (c->>'cdfStddev')::DOUBLE, false FROM cdfs
    UNION ALL SELECT metric, 'min',     min,     (c->>'$.cdfRange[0]')::DOUBLE, false FROM cdfs
    UNION ALL SELECT metric, 'max',     max,     (c->>'$.cdfRange[1]')::DOUBLE, false FROM cdfs
    UNION ALL
      SELECT metric, printf('centile %g', cv.centile), cv.value,
             (SELECT (s->>'$[1]')::DOUBLE FROM (SELECT unnest(json_extract(c, '$.cdfSamples[*]')) AS s)
              WHERE (s->>'$[0]')::DOUBLE = cv.centile), false
      FROM (SELECT metric, c, unnest(centiles) AS cv FROM cdfs)),
  dom AS (
    SELECT o.metric, o.size, o.min, o.max,
           json_extract(j.j, CASE o.metric
             WHEN 'domain.blocks_raw'      THEN '$.bpDomainBlocks'
             WHEN 'domain.blocks_filtered' THEN '$.bpDomainBlocks'
             WHEN 'domain.slots_raw'       THEN '$.bpDomainSlots'
             ELSE '$.bpDomainSlots' END) AS d,
           o.metric LIKE '%_raw' AS raw
    FROM ours o, j WHERE o.metric LIKE 'domain.%'),
  domstats AS (
              SELECT metric, 'size', size::DOUBLE, (CASE WHEN raw THEN d->>'ddRawCount' ELSE d->>'ddFilteredCount' END)::DOUBLE, true FROM dom
    UNION ALL SELECT metric, 'min', min, (CASE WHEN raw THEN d->>'$.ddRaw[0]' ELSE d->>'$.ddFiltered[0]' END)::DOUBLE, true FROM dom
    UNION ALL SELECT metric, 'max', max, (CASE WHEN raw THEN d->>'$.ddRaw[1]' ELSE d->>'$.ddFiltered[1]' END)::DOUBLE, true FROM dom)
SELECT metric, statistic, ours, locli,
       CASE WHEN exact THEN ours = locli
            ELSE abs(ours - locli) <= greatest(abs_tol, rel_tol * abs(locli)) END AS ok
FROM (SELECT * FROM stats UNION ALL SELECT * FROM domstats);
