reset
set term pngcairo size 1024,768 lw 1

set yrange [0.02:50]
set ytics nomirror
set logscale y
set xlabel "Centile, fraction of sample population"
set ylabel "Time, s"

set style data yerrorlines

rundir = "../"

cdfI_2(cdf, title, run1ti, run1, run2ti, run2) = \
  "set title '".title."';". \
  "plot '".rundir."/".run1."/analysis/".cdf.".cdf' using 1:2 t '".run1ti."', '". \
           rundir."/".run2."/analysis/".cdf.".cdf' using 1:2 t '".run2ti."'   ". \
           ""

cdfI_3(cdf, title, run1ti, run1, run2ti, run2, run3ti, run3) = \
  "set title '".title."';". \
  "plot '".rundir."/".run1."/analysis/".cdf.".cdf' using 1:2 t '".run1ti."', '". \
           rundir."/".run2."/analysis/".cdf.".cdf' using 1:2 t '".run2ti."', '". \
           rundir."/".run3."/analysis/".cdf.".cdf' using 1:2 t '".run3ti."'   ". \
           ""

cdfI_4(cdf, title, run1ti, run1, run2ti, run2, run3ti, run3, run4ti, run4) = \
  "set title '".title."';". \
  "plot '".rundir."/".run1."/analysis/".cdf.".cdf' using 1:2 t '".run1ti."', '". \
           rundir."/".run2."/analysis/".cdf.".cdf' using 1:2 t '".run2ti."', '". \
           rundir."/".run3."/analysis/".cdf.".cdf' using 1:2 t '".run3ti."', '". \
           rundir."/".run4."/analysis/".cdf.".cdf' using 1:2 t '".run4ti."'   ". \
           ""
