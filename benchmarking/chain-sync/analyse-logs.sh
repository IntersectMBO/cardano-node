#!/bin/sh
set -x
logdir="$1"
if test ! -d "${logdir}"
then echo "ERROR:  log directory not found at '${logdir}'" >&2; exit 1; fi

# get first block copy time
ls -1 ${logdir}/node-0-*.log | head -1
LOGFILE=`ls -1 ${logdir}/node-0-*.log | head -1`
if test ! -f "${LOGFILE}"
then echo "ERROR:  logfile not found: ${LOGFILE}" >&2; exit 1; fi
FIRSTSLOT=`grep -e '.*:cardano.node.ChainDB:.*TraceCopyToImmDBEvent.CopiedBlockToImmDB.*"tip":"[a-z0-9]*@' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*"tip":"[a-z0-9]*@\([0-9]\+\)".*/\1 \2\t\3/p; q;'`

LOGFILE=`ls -1r ${logdir}/node-0-*.log | head -1`
if test ! -f "${LOGFILE}"
then echo "ERROR:  logfile not found: ${LOGFILE}" >&2; exit 1; fi
LASTSLOT=`grep -e '.*:cardano.node.ChainDB:.*TraceCopyToImmDBEvent.CopiedBlockToImmDB.*"tip":"[a-z0-9]*@' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*"tip":"[a-z0-9]*@\([0-9]\+\)".*/\1 \2\t\3/p;' | tail -n 1`

# output git revision
git log | head -1 | cut -d ' ' -f 2
echo $FIRSTSLOT
echo $LASTSLOT

