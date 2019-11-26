#!/bin/sh

BASEDIR=$(realpath $(dirname $0))

. ${BASEDIR}/configuration/psql-settings.sh

psql -q  <<ENDQUERY

  DROP TABLE IF EXISTS results;

  SELECT block.slot_no, block.slot_leader, block.epoch_no,
          block.block_no, txq.txcount, txq.size, block."time",
          "time" - lag("time", 1) OVER (ORDER BY "time") AS delta
  INTO results
  FROM
    (SELECT tx.block, COUNT(*) as txcount, SUM(tx.size) as size
      FROM tx as tx
      GROUP BY tx.block) as txq
  INNER JOIN block ON txq.block = block.id;

  -- show result table
  SELECT * FROM results;

  -- output number of slots per node
  SELECT slot_leader, COUNT(*) AS "block_count"
  FROM results
  WHERE size > 0
  GROUP BY slot_leader;

  -- output column counts
  SELECT COUNT(iq.slot_leader) AS "count_slot_leaders", NULL AS "block_count"
  FROM (SELECT DISTINCT slot_leader FROM results WHERE size > 0) AS iq
  UNION ALL
  SELECT NULL, COUNT(iq.block_no)
  FROM (SELECT block_no FROM block WHERE tx_count > 0) AS iq;

  SELECT COUNT(iq.tid) AS "tx_count"
  FROM (SELECT id AS tid FROM tx WHERE size > 0) AS iq;

  -- calculate TPS
  SELECT resq.*, (resq."total_trxs" / extract(epoch from resq."delta_t")) AS TPS
  FROM
    (SELECT MAX(subq.min) AS t0, MAX(subq.max) AS t1, MAX("delta_t") AS "delta_t", MAX("total_tx") AS "total_trxs"
     FROM
       (SELECT MIN("time"), MAX("time"), (MAX("time") - MIN("time")) AS "delta_t", NULL AS "total_tx"
       FROM results
       WHERE size > 0

       UNION ALL

       SELECT NULL, NULL, NULL, SUM(innerq.txcount) AS "total_tx"
       FROM
         (SELECT txcount
          FROM results
          WHERE size > 0
          ORDER BY "time"
          OFFSET 1) AS innerq  -- do not count tx in first block
         ) AS subq
    ) AS resq;
  
  -- calculate TPBS
  SELECT resq.*, (resq."total_size" / extract(epoch from resq."delta_t")) AS TBPS
  FROM
    (SELECT MAX(subq.min) AS t0, MAX(subq.max) AS t1, MAX("delta_t") AS "delta_t", MAX("total_size") AS "total_size"
     FROM
       (SELECT MIN("time"), MAX("time"), (MAX("time") - MIN("time")) AS "delta_t", NULL AS "total_size"
       FROM results
       WHERE size > 0

       UNION ALL

       SELECT NULL, NULL, NULL, SUM(innerq.size) AS "total_size"
       FROM
         (SELECT size
          FROM results
          WHERE size > 0
          ORDER BY "time"
          OFFSET 1) AS innerq  -- do not count tx in first block
         ) AS subq
    ) AS resq;
  
ENDQUERY

