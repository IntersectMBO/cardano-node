reset
set term pngcairo size 1024,768 lw 1

set title ""

set yrange [*:*]
set ytics nomirror
set logscale y

set style data yerrorlines

cdfI_2(cdf, run1, run2) \
  = sprintf("plot '%s/%s/analysis/%s.cdf' using 1:2, '%s/%s/analysis/%s.cdf' using 1:2", rundir, run1, cdf, rundir, run2, cdf)
cdfI_3(cdf, run1, run2, run3) \
  = sprintf("plot '%s/%s/analysis/%s.cdf' using 1:2, '%s/%s/analysis/%s.cdf' using 1:2, '%s/%s/analysis/%s.cdf' using 1:2", rundir, run1, cdf, rundir, run2, cdf, rundir, run3, cdf)