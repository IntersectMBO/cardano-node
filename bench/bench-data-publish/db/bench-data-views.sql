--
-- views for the API
--

-- a base view containing every attribute viable for filtering all published runs
DROP MATERIALIZED VIEW IF EXISTS run CASCADE;
CREATE MATERIALIZED VIEW run AS
    SELECT
        cr.id AS run_id,
        cr.run_profile,
        cr.run_commit,
        cr.run_at,
        ri.meta #>> '{meta,batch}' AS run_name,
		ri.meta #>> '{meta,profile_content,generator,era}' AS run_era,
        CASE 
            WHEN (ri.meta #> '{meta,profile_content,generator,plutusMode}') :: BOOLEAN
			    OR (ri.meta #> '{meta,profile_content,generator,plutusAutoMode}') :: BOOLEAN
			THEN 'plutus'
            ELSE 'value'
        END AS run_workload
    FROM
        cluster_run cr
        JOIN run_info ri ON cr.id = ri.run_id
    WHERE
        cr.run_published = true;

COMMENT ON MATERIALIZED VIEW run IS
    'All runs on the benchmarking cluster';

-- this view facilitates access to the raw JSON data that was imported
CREATE OR REPLACE VIEW run_raw AS
    SELECT
        r.*,
        ri.meta,
        rr.blockprop,
        rr.clusterperf
    FROM 
        run r
        JOIN run_info ri USING (run_id)
        LEFT JOIN run_result rr USING (run_id);

COMMENT ON VIEW run_raw IS
    'Raw JSON data for each benchmarking cluster run, as originally imported';

-- a view over all existing blockprop results from published runs
CREATE OR REPLACE VIEW blockprop AS
    WITH cte AS (
        SELECT
            r.run_id,
            rr.blockprop
        FROM
            run r
            JOIN run_result rr USING (run_id)
        WHERE
            rr.blockprop IS NOT NULL
    ) 
    SELECT
        cte.run_id,
        obj.key AS cdf_name,
        cdf.key AS cdf_prop,
        cdf.value AS cdf_value
    FROM
        cte,
        jsonb_each(cte.blockprop) AS obj,
		jsonb_each(obj.value) as cdf
    WHERE
        obj.value ? 'cdfSize';

-- a list of all known blockprop metrics
CREATE OR REPLACE VIEW blockprop_metric AS
    SELECT
        DISTINCT cdf_name
    FROM
        blockprop
    ORDER BY
        cdf_name;

-- a view over all existing clsuterperf results from published runs
CREATE OR REPLACE VIEW clusterperf AS
    WITH cte AS (
        SELECT
            r.run_id,
            rr.clusterperf
        FROM
            run r
            JOIN run_result rr USING (run_id)
        WHERE
            rr.clusterperf IS NOT NULL
    ) 
    SELECT
        cte.run_id,
        obj.key AS cdf_name,
        cdf.key AS cdf_prop,
        cdf.value AS cdf_value
    FROM
        cte,
        jsonb_each(cte.clusterperf) AS obj,
		jsonb_each(obj.value) as cdf
    WHERE
        obj.value ? 'cdfSize';

-- a list of all known clusterperf metrics
CREATE OR REPLACE VIEW clusterperf_metric AS
    SELECT
        DISTINCT cdf_name
    FROM
        clusterperf
    ORDER BY
        cdf_name;

-- a composition of run attributes and the view on blockprop results
CREATE OR REPLACE VIEW run_blockprop AS
   SELECT
        r.*,
        b.cdf_name,
        b.cdf_prop,
        b.cdf_value
    FROM
        run r
        JOIN blockprop b USING (run_id);

-- a composition of run attributes and the view on clusterperf results
CREATE OR REPLACE VIEW run_clusterperf AS
   SELECT
        r.*,
        c.cdf_name,
        c.cdf_prop,
        c.cdf_value
    FROM
        run r
        JOIN clusterperf c USING (run_id);
