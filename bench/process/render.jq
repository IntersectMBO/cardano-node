##
## Presentation
##

def decimal_pt: (. * 10 | floor) / 10;
def decimal_pt2: (. * 100 | floor) / 100;
def float_n(width): "\(.)" | .[:width + (if .[0:1] == "-" then 1 else 0 end)];
def downscale(factor): . / factor;

## Expect name of a simple numeric field.
def field(fname; f):
    .[fname] as $val
  | "\($val | f)";

## Expect name of a rich variable.
def var(fname; f):
    .[fname] as $val
  | "\($val.mean | f) | \("\($val.relstddev)" | .[:4])";

def render_config (format; cf; res):
    . as $x
  | (if format != "csv" then [null] else [] end
     +
     (cf
      | map($x[.key]))
     +
     (res
      | map(. as $spec
            | $x[$spec.key] as $val
            | [(($val.mean / ($val.scale // 1))
               | if $spec.round then ceil else . end)
              , ($val.relstddev | tostring | .[:5])])
      | add))
    as $columns

  | ($columns | join(" | "));

def render_table_head (format; cf; res):
    .
  | (if format != "csv" then [null] else [] end
     +
     (cf
      | map(.header))
     +
     (res
      | map([.header, "σ/μ"])
      | add))
    as $columns

  | if format == "github"
    then [([null] + ($columns | map("--")) + [null])
          | join("|")] else [] end
    +
    [ $columns | join(" | ")];

def render_table:
     .format       as $format
   | .config_specs as $config_specs
   | .result_specs as $result_specs
   | render_table_head (.format; .config_specs; .result_specs)
     + (.configs | map (render_config ($format; $config_specs; $result_specs)));

def add_header_footer(commits; run_counts; slot_counts):
     .
   | ([ "Parameters:\n"
      , if run_counts | all(. == run_counts[0])
          then "         Every value is mean of \(run_counts[0]) runs,\n"
         else "         Every value is mean of varying amount of runs (\(run_counts)).\n" end
      , if slot_counts | all(. == slot_counts[0])
          then "         Each run was syncing \(slot_counts[0]) slots, or \(slot_counts[0] / 21600 | floor) epochs over loopback, from a quiescent server.\n"
          else "         Runs were for varying amount of slots (\(slot_counts)).\n" end
      , "\nLegend:\n"
      , "         wall = total_wall_seconds, total cpu = total_cpu_seconds\n"
      , "         total alloc = allocated_bytes, copied = copied_bytes, max live = max_live_bytes\n"
      , "         See https://github.com/ghc/ghc/blob/master/includes/RtsAPI.h for details.\n"
      , "         Each value is followed by σ/μ, i.e. relative stddev (or CoV).\n"
      , "         Δ% is change from baseline.\n"
      , "\n\n"]) + .;
  #| . + ["\n\n\(commits | map("  - \(.branch) / \(.commit) = https://github.com/'${github_user}'/cardano-node/tree/\(.commit)\n") | add)"];

def render(header_footer):
  .commits     as $commits
| .run_counts  as $run_counts
| .slot_counts as $slot_counts
| render_table
| if header_footer == true
    then add_header_footer($commits; $run_counts; $slot_counts)
    else . end
| join("\n");

