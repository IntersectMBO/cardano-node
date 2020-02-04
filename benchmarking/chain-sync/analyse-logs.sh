#!/bin/sh

NETWORK=${1:-mainnet}

# get first block copy time
LOGFILE=`ls -1 state-node-${NETWORK}/node-0-*.log | head -3`
FIRSTSLOT=`grep -e '.*:cardano.node.ChainDB:.*TraceCopyToImmDBEvent.CopiedBlockToImmDB.*"tip":"[a-z0-9]*@' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*"tip":"[a-z0-9]*@\([0-9]\+\)".*/"\1 \2";\3/p; q;'`

LOGFILE=`ls -1r state-node-${NETWORK}/node-0-*.log | head -2`
echo $LOGFILE
LASTSLOT=`grep -e '.*:cardano.node.ChainDB:.*TraceCopyToImmDBEvent.CopiedBlockToImmDB.*"tip":"[a-z0-9]*@' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*"tip":"[a-z0-9]*@\([0-9]\+\)".*/"\1 \2";\3/p;' | sort -k 1.2,1.23 | tail -n 1`
LASTRSS=`grep -e '.*cardano.node-metrics:.*Mem.resident =' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*Mem.resident = \([0-9]\+\).*/\1 \2\t\3/p;' | { read a b c; while [ -n "$a" ]; do echo "\"$a $b\";$((c*4096))"; read a b c; done; } | sort -k 1.2,1.23 | tail -n 1`
LASTNETIN=`grep -e '.*cardano.node-metrics:.*Net.IpExt:InOctets =' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*Net.IpExt:InOctets = \([0-9]\+\).*/"\1 \2";\3/p;' | sort -k 1.2,1.23 | tail -n 1`
LASTNETOUT=`grep -e '.*cardano.node-metrics:.*Net.IpExt:OutOctets =' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*Net.IpExt:OutOctets = \([0-9]\+\).*/"\1 \2";\3/p;' | sort -k 1.2,1.23 | tail -n 1`
LASTDISKIN=`grep -e '.*cardano.node-metrics:.*IO.rbytes =' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*IO.rbytes = \([0-9]\+\).*/"\1 \2";\3/p;' | sort -k 1.2,1.23 | tail -n 1`
LASTDISKOUT=`grep -e '.*cardano.node-metrics:.*IO.wbytes =' $LOGFILE | sed -ne 's/^.* \[\([0-9-]\+\) \([0-9:.]\+\) UTC\] .*IO.wbytes = \([0-9]\+\).*/"\1 \2";\3/p;' | sort -k 1.2,1.23 | tail -n 1`

# output git revision
echo -n "commit;"
git log | head -1 | cut -d ' ' -f 2
echo -n "slotfirst;"; echo $FIRSTSLOT
echo -n "slotlast;"; echo $LASTSLOT
echo -n "memorylast;"; echo $LASTRSS
echo -n "netinlast;"; echo $LASTNETIN
echo -n "netoutlast;"; echo $LASTNETOUT
echo -n "diskinlast;"; echo $LASTDISKIN
echo -n "diskoutlast;"; echo $LASTDISKOUT

