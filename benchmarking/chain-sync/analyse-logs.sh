#!/bin/sh

# get first block copy time
LOGFILE=`ls -1 state-node-mainnet/node-0-*.log | head -3`
FIRSTSLOT=`grep -e '.*:cardano.node.ChainDB:.*TraceCopyToImmDBEvent.CopiedBlockToImmDB.*"tip":"[a-z0-9]*@' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*"tip":"[a-z0-9]*@\([0-9]\+\)".*/\1 \2\t\3/p; q;'`

LOGFILE=`ls -1r state-node-mainnet/node-0-*.log | head -1`
LASTSLOT=`grep -e '.*:cardano.node.ChainDB:.*TraceCopyToImmDBEvent.CopiedBlockToImmDB.*"tip":"[a-z0-9]*@' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*"tip":"[a-z0-9]*@\([0-9]\+\)".*/\1 \2\t\3/p;' | tail -n 1`

# output git revision
git log | head -1 | cut -d ' ' -f 2
echo $FIRSTSLOT
echo $LASTSLOT

