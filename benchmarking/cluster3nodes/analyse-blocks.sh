#!/bin/sh

BASEDIR=$(realpath $(dirname $0))

. ${BASEDIR}/configuration/psql-settings.sh

Q='SELECT block.slot_no, block.slot_leader, block.epoch_no,
          block.block_no, txq.txcount, txq.size, block."time" FROM
    (SELECT tx.block, COUNT(*) as txcount, SUM(tx.size) as size
      FROM tx as tx
      GROUP BY tx.block) as txq
    INNER JOIN block ON txq.block = block.id;'

psql -c "$Q"
