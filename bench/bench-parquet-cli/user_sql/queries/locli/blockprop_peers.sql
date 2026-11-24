-- "locli_blockprop_peers(...)": locli's peer-side block propagation, the
-- cdfPeer* CDFs of analysis/blockprop.json, one row per CDF. Needs
-- blockprop.sql; filters as described there.
--
--   SELECT * FROM locli_blockprop_peers(filters := locli_filter(unitary := true, min_adoptions := 5));
--
-- PURPOSE: cross-checking locli (see blockprop.sql). User content
-- (user_sql/README.md): not reviewed like sql/.
--
-- SAMPLES FROM VALID OBSERVATIONS of the accepted main-chain blocks (locli's
-- isValidBlockObservation: a node other than the forger noticed, requested,
-- fetched and adopted the block, no step negative, chain length delta 1),
-- seconds:
--   cdfPeerNoticeFirst  per block, the earliest: slot start -> DownloadedHeader
--   cdfPeerFetchFirst   per block, the earliest: slot start -> CompletedBlockFetch
--   cdfPeerRequest      each: DownloadedHeader -> SendFetchRequest
--   cdfPeerFetch        each: SendFetchRequest -> CompletedBlockFetch
--   cdfPeerAnnounce     each, where traced: CompletedBlockFetch -> first announcement
--   cdfPeerSend         each, where traced: CompletedBlockFetch -> first SendBlock
--   cdfPeerAdoption     each: CompletedBlockFetch -> first adoption
--
-- Fields: as locli_blockprop_forger (metric, size, average, median, stddev,
-- min, max, centiles).
CREATE OR REPLACE MACRO locli_blockprop_peers(filters := NULL) AS TABLE
WITH
  c AS MATERIALIZED (SELECT * FROM locli_bp_chain(filters := filters) WHERE accepted),
  o AS MATERIALIZED (FROM locli_bp_observations(c)),
  firsts AS (
    SELECT hash, min(noticed) AS notice_first, min(noticed + requested + fetched) AS fetch_first
    FROM o GROUP BY hash)
SELECT r.metric AS metric, r.cdf.size AS size, r.cdf.average AS average, r.cdf.median AS median, r.cdf.stddev AS stddev, r.cdf.min AS min, r.cdf.max AS max, r.cdf.centiles AS centiles
FROM (
    SELECT unnest([
        {'metric': 'cdfPeerNoticeFirst', 'cdf': locli_cdf(list(notice_first))},
        {'metric': 'cdfPeerFetchFirst',  'cdf': locli_cdf(list(fetch_first))}
      ]) AS r
    FROM firsts
  UNION ALL
    SELECT unnest([
        {'metric': 'cdfPeerRequest',  'cdf': locli_cdf(list(requested))},
        {'metric': 'cdfPeerFetch',    'cdf': locli_cdf(list(fetched))},
        {'metric': 'cdfPeerAnnounce', 'cdf': locli_cdf(list(announced) FILTER (WHERE announced IS NOT NULL))},
        {'metric': 'cdfPeerSend',     'cdf': locli_cdf(list(sending)   FILTER (WHERE sending   IS NOT NULL))},
        {'metric': 'cdfPeerAdoption', 'cdf': locli_cdf(list(adopted))}
      ]) AS r
    FROM o);
