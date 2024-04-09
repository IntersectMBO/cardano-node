# Set the output to a png file
set terminal png size 1200,800

# The graphic titles
set title   "Heap sizes"
set xlabel  "Seconds (from first Resource trace)"
set ylabel  "Heap size (bytes)"
set format y  "%'.0f"
set y2label "UTxO set size (UTxOs)"
set format y2 "%'.0f"
set ytics nomirror
set y2tics

set datafile separator whitespace

set output 'heap-value.png'
plot                                                     \
  for [IDX=0:51] 'heap-value.txt'        index IDX u 1:2 \
    with steps lt rgb "black" axes x1y1,                 \
  for [IDX=0:51] 'heap-value-utxohd.txt' index IDX u 1:2 \
    with steps lt rgb "red"   axes x1y1,                 \
  for [IDX=0:51] 'utxo-value.txt'        index IDX u 1:2 \
    with steps lt rgb "gray"   axes x1y2,                \
  for [IDX=0:51] 'utxo-value-utxohd.txt' index IDX u 1:2 \
    with steps lt rgb "orange" axes x1y2

set output 'heap-plutus.png'
plot                                                      \
  for [IDX=0:51] 'heap-plutus.txt'        index IDX u 1:2 \
    with steps lt rgb "black"  axes x1y1,                 \
  for [IDX=0:51] 'heap-plutus-utxohd.txt' index IDX u 1:2 \
    with steps lt rgb "red"    axes x1y1,                 \
  for [IDX=0:51] 'utxo-plutus.txt'        index IDX u 1:2 \
    with steps lt rgb "gray"   axes x1y2,                 \
  for [IDX=0:51] 'utxo-plutus-utxohd.txt' index IDX u 1:2 \
    with steps lt rgb "orange" axes x1y2

#do for [ i = 0:51 ] {
#  if (i == 0) {
#    plot   'heap-0.txt' index i using 1:2 with lines lt rgb "black" title sprintf("HD-%d",i)
#    #plot 'heap-1.txt' index i using 1:2 with lines lt rgb "yellow" title sprintf("%d",i)
#  } else {
#    replot 'heap-0.txt' index i using 1:2 with lines lt rgb "violet" title sprintf("HD-%d",i)
#    replot 'heap-1.txt' index i using 1:2 with lines lt rgb "yellow" title sprintf("%d",i)
#  }
#}

