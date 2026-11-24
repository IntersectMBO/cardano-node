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

-- Memory (MiB) and cores for scheduling conversions, from DuckDB's defaults on a
-- fresh connection (portable: Linux, macOS, Windows). Its default memory_limit is
-- 80 % of physical RAM or of a cgroup memory limit, whichever is lower; dividing by
-- 0.8 gives that limit back (the per-slot budget has its own margin). Its default
-- threads follow the cores and a cgroup CPU quota.
CREATE OR REPLACE MACRO bp_resources() AS TABLE
  WITH m AS (SELECT regexp_extract(current_setting('memory_limit'),
                                   '^([0-9.]+) ?([A-Za-z]+)$', ['n', 'u']) AS m)
  SELECT CAST(m.n::DOUBLE * CASE m.u WHEN 'KiB' THEN 1.0 / 1024 WHEN 'MiB' THEN 1
                                     WHEN 'GiB' THEN 1024 WHEN 'TiB' THEN 1048576
                                     ELSE error('memory_limit not understood: ' || current_setting('memory_limit')) END
              / 0.8 AS BIGINT) AS mem_mib,
         current_setting('threads') AS cores
  FROM (SELECT unnest(m) FROM m) m;

-- The run metadata (meta/run.parquet) after convert --add: the recorded row,
-- with each genesis value it lacks taken from the arguments (NULL: not given).
-- An error, before anything is converted, when the run id or the converter
-- version differs from the recorded one, or a given genesis value from a
-- recorded one. Parameters are named new_*: a macro parameter shadows a column
-- of the same name in the body. Folders converted before epoch_length existed
-- read it as NULL.
CREATE OR REPLACE MACRO bp_merge_run_meta(old_file, new_run_id, new_system_start, new_slot_length_s,
                                          new_epoch_length, new_converter_version) AS TABLE
SELECT
    CASE WHEN o.run_id = new_run_id THEN o.run_id
         ELSE error(printf('convert --add: run id %s, recorded: %s', CAST(new_run_id AS VARCHAR), o.run_id)) END
      AS run_id
  , CASE WHEN new_system_start IS NULL OR o.system_start IS NULL
         THEN coalesce(o.system_start, CAST(new_system_start AS TIMESTAMP))
         WHEN o.system_start = new_system_start THEN o.system_start
         ELSE error(printf('convert --add: system start %s, recorded: %s', CAST(new_system_start AS VARCHAR), CAST(o.system_start AS VARCHAR))) END
      AS system_start
  , CASE WHEN new_slot_length_s IS NULL OR o.slot_length_s IS NULL
         THEN coalesce(o.slot_length_s, CAST(new_slot_length_s AS DOUBLE))
         WHEN o.slot_length_s = new_slot_length_s THEN o.slot_length_s
         ELSE error(printf('convert --add: slot length %s, recorded: %s', CAST(new_slot_length_s AS VARCHAR), CAST(o.slot_length_s AS VARCHAR))) END
      AS slot_length_s
  , CASE WHEN new_epoch_length IS NULL OR o.epoch_length IS NULL
         THEN coalesce(o.epoch_length, CAST(new_epoch_length AS BIGINT))
         WHEN o.epoch_length = new_epoch_length THEN o.epoch_length
         ELSE error(printf('convert --add: epoch length %s, recorded: %s', CAST(new_epoch_length AS VARCHAR), CAST(o.epoch_length AS VARCHAR))) END
      AS epoch_length
  , CASE WHEN o.converter_version = new_converter_version THEN o.converter_version
         ELSE error(printf('convert --add: written by converter %s, this is %s (convert again with --force)',
                           o.converter_version, new_converter_version)) END
      AS converter_version
  , o.raw_schema_version
  , o.duckdb_version
  , o.created
FROM (SELECT * FROM (SELECT CAST(NULL AS BIGINT) AS epoch_length LIMIT 0)
      UNION ALL BY NAME
      SELECT * FROM read_parquet(old_file)) o;
