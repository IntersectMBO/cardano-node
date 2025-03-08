################################################################################

SELECT array_to_json(array_agg(rows))
FROM (SELECT * FROM epoch_param ORDER BY epoch_no) AS rows

SELECT array_to_json(array_agg(rows))
FROM (SELECT * FROM cost_model  ORDER BY id)       AS rows

################################################################################

PGHOST=127.0.0.1 PGPORT=5432 PGDATABASE=mainnet PGUSER=mainnet psql -c "COPY( SELECT array_to_json(array_agg(rows)) FROM (SELECT * FROM epoch_param ORDER BY epoch_no) AS rows) TO '/tmp/epoch_param.json';"

PGHOST=127.0.0.1 PGPORT=5432 PGDATABASE=mainnet PGUSER=mainnet psql -c "COPY( SELECT array_to_json(array_agg(rows)) FROM (SELECT * FROM cost_model ORDER BY id) AS rows) TO '/tmp/cost_model.json';"

################################################################################
