#!/bin/sh

# extract timestamp and slot number
sed -ne 's/.*\([0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\} [.:0-9]\+\) .*{"event":{"kind":"TraceCopyToImmDBEvent.CopiedBlockToImmDB","slot":{"kind":"Tip","tip":".*@\([0-9]\+\)".*/\1 \2/p;' node.log-2019* > immblockevents.csv

# extract timestamp and memory usage
sed -ne 's/.*\([0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\} [.:0-9]\+\) .*Mem.resident = \([0-9]\+\).*/\1 \2/p;' node.log-2019* > memresidentevents.csv

# extract timestamp and network counters
sed -ne 's/.*\([0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\} [.:0-9]\+\) .*Net.IpExt:OutOctets = \([0-9]\+\).*/\1 \2/p;' node.log-2019* > netoutevents.csv
sed -ne 's/.*\([0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\} [.:0-9]\+\) .*Net.IpExt:InOctets = \([0-9]\+\).*/\1 \2/p;' node.log-2019* > netinevents.csv

# extract timestamp and I/O counters
sed -ne 's/.*\([0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\} [.:0-9]\+\) .*IO.wchar = \([0-9]\+\).*/\1 \2/p;' node.log-2019* > iowriteevents.csv
sed -ne 's/.*\([0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\} [.:0-9]\+\) .*IO.rchar = \([0-9]\+\).*/\1 \2/p;' node.log-2019* > ioreadevents.csv

