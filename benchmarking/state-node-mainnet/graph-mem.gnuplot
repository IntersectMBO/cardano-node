set term png medium size 600,480
set output "memresidentevents.png"
set key above
plot 'memresidentevents.csv'  using (column(3)*4096/1024/1024) title "resident memory (MB)"
set output

