#!/bin/sh

if [ -z "${BASEDIR}" -o ! -d "${BASEDIR}" ]; then
  echo "missing \$BASEDIR"
  exit 1
fi

export PGDATABASE=cexplorer
export PGUSER=cexplorer
export PGHOST=localhost
export PGPORT=5432
export PGPASSFILE=${BASEDIR}/configuration/pgpass
