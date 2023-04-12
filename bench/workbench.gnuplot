reset
set term pngcairo size 1024,768 lw 1

set yrange [0:50]
set ytics nomirror
set logscale y
set xlabel "Centile, fraction of sample population"
set ylabel "Time, s"

set style data yerrorlines

rundir = "../.."

cdfI_2(cdf, title, run1ti, run1, run2ti, run2) = \
  "set title '".title."';". \
  "plot '".rundir."/".run1."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run1ti."', '". \
           rundir."/".run2."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run2ti."'   ". \
           ""

cdfI_3(cdf, title, run1ti, run1, run2ti, run2, run3ti, run3) = \
  "set title '".title."';". \
  "plot '".rundir."/".run1."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run1ti."', '". \
           rundir."/".run2."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run2ti."', '". \
           rundir."/".run3."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run3ti."'   ". \
           ""

cdfI_4(cdf, title, run1ti, run1, run2ti, run2, run3ti, run3, run4ti, run4) = \
  "set title '".title."';". \
  "plot '".rundir."/".run1."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run1ti."', '". \
           rundir."/".run2."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run2ti."', '". \
           rundir."/".run3."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run3ti."', '". \
           rundir."/".run4."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run4ti."'   ". \
           ""

cdfI_5(cdf, title, run1ti, run1, run2ti, run2, run3ti, run3, run4ti, run4, run5ti, run5) = \
  "set title '".title."';". \
  "plot '".rundir."/".run1."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run1ti."', '". \
           rundir."/".run2."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run2ti."', '". \
           rundir."/".run3."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run3ti."', '". \
           rundir."/".run4."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run4ti."', '". \
           rundir."/".run5."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run5ti."'   ". \
           ""

# cdfI_6(cdf, title, run1ti, run1, run2ti, run2, run3ti, run3, run4ti, run4, run5ti, run5, run6ti, run6) = \
#   "set title '".title."';". \
#   "plot '".rundir."/".run1."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run1ti."', '". \
#            rundir."/".run2."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run2ti."', '". \
#            rundir."/".run3."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run3ti."', '". \
#            rundir."/".run4."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run4ti."', '". \
#            rundir."/".run5."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run5ti."', '". \
#            rundir."/".run6."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run6ti."'   ". \
#            ""

# cdfI_7(cdf, title, run1ti, run1, run2ti, run2, run3ti, run3, run4ti, run4, run5ti, run5, run6ti, run6, run7ti, run7) = \
#   "set title '".title."';". \
#   "plot '".rundir."/".run1."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run1ti."', '". \
#            rundir."/".run2."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run2ti."', '". \
#            rundir."/".run3."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run3ti."', '". \
#            rundir."/".run4."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run4ti."', '". \
#            rundir."/".run5."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run5ti."', '". \
#            rundir."/".run6."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run6ti."', '". \
#            rundir."/".run7."/analysis/cdf/".cdf.".cdf' using 1:2 t '".run7ti."'   ". \
#            ""
