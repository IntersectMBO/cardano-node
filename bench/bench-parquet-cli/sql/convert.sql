-- Conversion of one log to Parquet. Loaded by convert only, after lib.sql
-- (bp_parse uses bp_rebuild_line).

-- The rows of one log, in Parquet layout, unsorted. src: the log's lines, each
-- numbered by the caller as "<n>\x1f<line>" (convert pipes them through
-- /dev/stdin). One pass, no intermediate tables (each would spill for large logs).
CREATE OR REPLACE MACRO bp_parse(process, src) AS TABLE
  WITH
  l AS (SELECT l[1:i-1]::UBIGINT AS seq, l[i+1:] AS orig
        FROM (SELECT l, instr(l, E'\x1f') AS i
              FROM read_csv(src, columns={'l': 'VARCHAR'}, header=false, delim=E'\x1e',
                            quote='', escape='', auto_detect=false, max_line_size=8388608))),
  -- A JSON line: an object with a namespace and a parseable timestamp. Anything
  -- else is a text line, kept verbatim.
  c AS (SELECT seq, orig,
               CASE WHEN left(orig, 1) = '{' AND json_valid(orig)
                    THEN (orig->>'ns') IS NOT NULL AND TRY_CAST(orig->>'at' AS TIMESTAMP_NS) IS NOT NULL
                    ELSE false
               END AS j
        FROM l),
  r AS (SELECT seq, orig,
               CASE WHEN j THEN CAST(orig->>'at' AS TIMESTAMP_NS) END AS ts,
               CASE WHEN j THEN orig->>'ns'     END AS ns,
               CASE WHEN j THEN orig->>'sev'    END AS sev,
               CASE WHEN j THEN orig->>'thread' END AS thread,
               CASE WHEN j THEN orig->>'host'   END AS host,
               -- The payload verbatim; the regex assumes the canonical envelope order.
               -- Otherwise the parsed form (not verbatim): the line then fails the
               -- rebuild check below and is kept whole.
               CASE WHEN j THEN coalesce(nullif(regexp_extract(orig, '"data":(.*),"sev":', 1), ''),
                                         (orig->'data')::VARCHAR) END AS data
        FROM c)
  SELECT process AS process, seq, ts, ns, sev, thread, host, data,
         CASE WHEN ns IS NULL
                OR bp_rebuild_line(ts, ns, data, sev, thread, host) IS DISTINCT FROM orig
              THEN orig
         END AS line
  FROM r;

-- Line counts of one converted log (reads only the small columns).
CREATE OR REPLACE MACRO bp_file_stats(f) AS TABLE
  SELECT count(*)::UBIGINT                                        AS lines_total,
         count(ns)::UBIGINT                                       AS lines_json,
         (count(*) - count(ns))::UBIGINT                          AS lines_text,
         count(*) FILTER (WHERE ns IS NULL
                            AND seq > (SELECT min(seq) FROM read_parquet(f)
                                       WHERE ns IS NOT NULL))::UBIGINT
                                                                  AS lines_text_after_json,
         count(*) FILTER (WHERE ns IS NOT NULL AND line IS NOT NULL)::UBIGINT
                                                                  AS lines_json_verbatim
  FROM read_parquet(f);
