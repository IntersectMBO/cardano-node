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

def render_value(spec; val):
  (val.mean / (spec.scale // 1))
  | if spec.round
    then ceil
    else . end
  | tostring;

def render_value_at_index(spec; val; ix):
  (val.raw[ix] / (spec.scale // 1))
  | if spec.round
    then ceil
    else . end
  | tostring;

def render_value_relstddev(spec; val):
  (val.relstddev / (spec.scale // 1) * 100)
  | tostring
  | .[:5];

def render_config_hydra_charts (res):
    . as $x
  | (res
      | map(. as $spec
             | $x[$spec.key] as $val
             # For each value, produce two Hydra metrics:
             #  - (id,     value, unit)
             #  - (id-CoV, CoV,   %)
             | [ $spec.key
               , render_value($spec; $val)
               , $spec.unit
               , $spec.key + "-coeff-variation"
               , render_value_relstddev($spec; $val)
               , "%"
               ]
            | join(" ")))
    as $properties

  | ($properties | join("\n"));

def render_html_config_runs(cf; res; count):
  [ "<table border=1>\n"
  , "<caption>Per-run raw data for config '\(.config)'</caption>"
  , "<tr><th>"
  , ((cf + res)
      | map(if .unit != null
            then [ .header + ", " + .unit ]
            else [ .header ] end)
      | add
      | join("</th><th>"))
  , "</th></tr>\n"
  , . as $x
    |
    ( [range(count)]
    | map( . as $ix
         | cf + res
         | map(. as $spec
                | $x[$spec.key] as $val
                | if .unit != null
                  then render_value_at_index($spec; $val; $ix)
                  else $val | tostring end
                | "<td>" + . + "</td>")
           | add
           | ("<tr>" + . + "</tr>\n"))
    | add)
  , "\n</table>"
  ]
  | add;

def render_html_summary(cf; res):
  [ "<table border=1>\n"
  , "<caption>Summary stats for run</caption>"
  , "<tr><th>"
  , ((cf + res)
      | map(if .unit != null
            then [ .header + ", " + .unit
                 , .header + ", " + "σ/μ"
                 ]
            else [ .header ] end)
      | add
      | join("</th><th>"))
  , "</th></tr>\n"
  , (.configs
    | map( . as $x
         | cf + res
         | map(. as $spec
                | $x[$spec.key] as $val
                | if .unit != null
                  then [ render_value($spec; $val)
                       , "</td><td>"
                       , render_value_relstddev($spec; $val) ]
                  else [ $val | tostring ] end
                | ["<td>"] + . + ["</td>"]
                | add)
           | add
           | ("<tr>" + . + "</tr>\n"))
    | add)
  , "\n</table>"
  ]
  | add;

def render_html:
    .config_specs as $cf
  | .result_specs as $res
  | .runs         as $runs

  | [render_html_summary($cf; $res)]
    + (.configs
        | map( . as $config
               | render_html_config_runs($cf; $res;
                                         $runs
                                          | map(select(.config_name == $config.config))
                                          | length)))
  | join("\n<hr>\n");

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
            | [ render_value($spec; $val)
              , render_value_relstddev($spec; $val)])
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

def render_hydra_charts_for_config(config_name):
  .result_specs as $result_specs
| .configs
| map(select(.config == config_name))
| if . == null or . == []
  then error ("render_hydra_charts_for_config: unknown config \(config_name)")
  else .[0]
  end
| render_config_hydra_charts ($result_specs)
# | join("\n")
;
