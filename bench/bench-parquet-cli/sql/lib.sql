-- Shared definitions: loaded by conversion, restore, excerpt and query sessions.
-- Conversion and restore use the same bp_rebuild_line, so what conversion proves
-- is exactly what restore produces.

-- The node's timestamp format: RFC 3339, UTC, fraction without trailing zeros,
-- no fraction at all when it is zero (e.g. 2026-11-04T14:30:24.9283082Z).
-- (strftime's %n is broken for TIMESTAMP_NS in DuckDB 1.5, hence by hand.)
CREATE OR REPLACE MACRO bp_fmt_at(t) AS
     strftime(t, '%Y-%m-%dT%H:%M:%S')
  || CASE WHEN epoch_ns(t) % 1000000000 = 0 THEN ''
          ELSE '.' || rtrim(lpad((epoch_ns(t) % 1000000000)::VARCHAR, 9, '0'), '0')
     END
  || 'Z';

-- One JSON log line in the canonical envelope order. NULL if any part is NULL:
-- conversion then keeps the original text in `line`.
CREATE OR REPLACE MACRO bp_rebuild_line(t, ns, data, sev, thread, host) AS
     '{"at":"'    || bp_fmt_at(t)
  || '","ns":'    || to_json(ns)
  || ',"data":'   || data
  || ',"sev":'    || to_json(sev)
  || ',"thread":' || to_json(thread)
  || ',"host":'   || to_json(host)
  || '}';

-- The original line of a stored row.
CREATE OR REPLACE MACRO bp_line(r) AS
  coalesce(r.line, bp_rebuild_line(r."at", r.ns, r.data, r.sev, r.thread, r.host));
