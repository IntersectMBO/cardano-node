set term png medium size 600,480
set output "netinevents.png"
set key above
plot "< { head -1 netinevents.csv > ttt; cat netinevents.csv >> ttt ; paste ttt netinevents.csv ; }"  using ($6-$3) title "delta network read bytes"
set output

