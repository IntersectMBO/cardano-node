#!/bin/sh

NETWORK=${1:-mainnet}

# get first block copy time
LOGFILE_JSON=`ls -1 state-node-${NETWORK}/node-0-*.json | head -3`

FIRSTSLOT=`jq --slurp 'map (select (.data.event.kind == "TraceCopyToImmDBEvent.CopiedBlockToImmDB")) | .[0] | "\(.at);\(.data.tip | sub (".*@(?<slot>.*)$"; "\(.slot)"))"' $LOGFILE_JSON | xargs echo | sed 's/T/ /; s/2020/"2020/; s/Z/"/'`

extract_legacy_metric() {
        grep "$1"' =' "$2" | jq '"\(.at);\(.msg)"' | sed 's/Z;'"$1"' = \([0-9]*\)\(\|[^0-9]*\)"$/";\1/; s/T/ /' | sort -k 1.2,1.23 | tail -n 1
}

LOGFILE_JSON=`ls -1r state-node-${NETWORK}/node-0-*.json | head -2`
echo $LOGFILE_JSON

LASTSLOT=`jq 'select (.data.event.kind == "TraceCopyToImmDBEvent.CopiedBlockToImmDB") | "\(.at);\(.data.tip | sub (".*@(?<slot>.*)$"; "\(.slot)"))"' $LOGFILE_JSON | sort -k 1.2,1.23 | tail -n 1 | xargs echo | sed 's/T/ /; s/2020/"2020/; s/Z/"/'`
LASTRSS=$(extract_legacy_metric "Mem.resident" $LOGFILE_JSON)
LASTNETIN=$(extract_legacy_metric "Net.IpExt:InOctets" $LOGFILE_JSON)
LASTNETOUT=$(extract_legacy_metric "Net.IpExt:OutOctets" $LOGFILE_JSON)
LASTDISKIN=$(extract_legacy_metric "IO.rbytes" $LOGFILE_JSON)
LASTDISKOUT=$(extract_legacy_metric "IO.wbytes" $LOGFILE_JSON)

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

