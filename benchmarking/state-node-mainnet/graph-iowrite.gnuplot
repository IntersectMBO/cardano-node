set term png medium size 600,480
set output "iowriteevents.png"
set key above
plot "< { head -1 iowriteevents.csv > ttt; cat iowriteevents.csv >> ttt ; paste ttt iowriteevents.csv ; }"  using ($6-$3) title "delta I/O write bytes"
set output

