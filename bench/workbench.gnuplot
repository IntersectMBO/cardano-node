reset
set term pngcairo size 1024,768 lw 1

set yrange [*:*]
set ytics nomirror
set logscale y

set style data yerrorlines

rundir = "../"

cdfI_2(cdf, run1, run2) \
  = "plot '".rundir."/".run1."/analysis/".cdf.".cdf' using 1:2, '".rundir."/".run2."/analysis/".cdf.".cdf' using 1:2"

cdfI_3(cdf, run1, run2, run3) = \
  "set title '".cdf."';". \
  "plot '".rundir."/".run1."/analysis/".cdf.".cdf' using 1:2 t '".run1."', '". \
           rundir."/".run2."/analysis/".cdf.".cdf' using 1:2 t '".run2."', '". \
           rundir."/".run3."/analysis/".cdf.".cdf' using 1:2 t '".run3."'   ". \
           ""
