##
## Aggregation
##
def mean:     if length == 0
                then 0
                else reduce .[] as $n (0; . + $n) / length end;
def pow2:     . * .;
def variance: . | mean as $mean | map_values(. - $mean | pow2) | mean;
def stddev:   . | (variance ?) | sqrt;

def samples_to_variable (n):
    stddev as $stddev
  | (add / n) as $mean
  | { "mean":      $mean
    , "stddev":    $stddev
    , "relstddev": (if $stddev == null then 0 else $stddev / $mean end)
    , "raw":       .
    };

def varspec_to_variable (objects; nobjects):
    .key  as $key
  | .path as $path
  | objects
  | { "\($key)":
      map (.data
           | getpath($path)
               as $val
           | if $val != null then $val
             else error("Path \($path) unreachable among top level keys: \(keys)")
             end
           )
      | samples_to_variable(nobjects)
    };

def description_from_headliner(x; rest):
  map ( . as $spec
      | (x | getpath($spec.path)) as $head
      | (rest | map (getpath($spec.path) == $head) | all) as $coherence
      | { "\($spec.key)":
          (if $coherence == true
           then $head
           else error("Incoherence on config key: \($spec.key)")
           end) })
  | add;

def aggregate_config_runs_variables (config_specs; result_specs):
    . as $runs
  | .[0] as $headliner
  | length as $nruns
  | result_specs
  | map(varspec_to_variable($runs; $nruns))
  | add
  | (config_specs | description_from_headliner($headliner; $runs[1:]))
    + .;
