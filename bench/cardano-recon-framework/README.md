# Cardano Re(altime) Con(formance) Framework

## Applications

### `cardano-recon` — LTL conformance checker

The application CLI takes as input:
  - A yaml list of strings, where each string is a parseable Linear Temporal Logic formula.
    For further details about the formula language, consult the syntax [reference](docs/ltl-formula-syntax.txt).
    For an example, see [this](examples/cfgs/formulas.yaml).
  - A list of files which contain machine-readable traces. For an example, see [this](examples/extracts/ok-1.txt).
  - An operation _mode_, which can be either _online_ or _offline_. For realtime conformance testing of multiple
    ever-growing log files, run in online mode. For testing on one read-only file run in offline mode.
  - Other configuration options (see `$ --help` for a comprehensive list).

The application traverses the events from the given log files and checks if each given formula is satisfied by them.
If negative, reports as such and lists the events that have been relevant to the formula.

Pass `--grep` for machine-readable output: on a negative outcome only the JSON array of relevant events is written
to stdout (bypassing trace-dispatcher), and nothing is printed on a positive outcome. This makes it straightforward
to pipe results directly into `cardano-recon-grep`.

#### CLI Syntax

```
Usage: cardano-recon FILE --mode <offline|online> --duration INT FILES
                     [--retention INT] [--trace-dispatcher-cfg FILE]
                     [--context FILE] [--dump-metrics BOOL] [--seek-to-end BOOL]
                     [--timeunit <hour|minute|second|millisecond|microsecond>]
                     [--on-missing-key <crash|bottom>] [--grep]

  Check formula satisfiability against a log of trace messages

Available options:
  --mode <offline|online>  mode
  --duration INT           temporal event duration (μs)
  --retention INT          temporal event retention period (ms) (default: 200)
  --trace-dispatcher-cfg FILE
                           trace dispatcher configuration file
  --context FILE           context variables
  --dump-metrics BOOL      enable periodic metric dumps to stdout
                           (default: False)
  --seek-to-end BOOL       seek to the end of the trace file before ingesting it
                           (default: True)
  --timeunit <hour|minute|second|millisecond|microsecond>
                           unit in which numeric arguments of temporal
                           operators in input formulas are measured
                           (default: second)
  --on-missing-key <crash|bottom>
                           behaviour when a formula atom references a missing
                           event property key (default: bottom)
  --grep                   on formula violation print only the JSON array of
                           relevant events; print nothing on satisfaction
  -h,--help                Show this help text
```

### `cardano-recon-grep` — Global Realisation Print

A stateless utility that filters a JSON array of trace messages by interpreting a list of
_continuous_ (temporal-operator-free) formulas against each message individually.
Only messages that satisfy **all** given formulas are written to stdout as a pretty-printed
JSON array.

Inputs:
  - A YAML list of continuous formula strings (same syntax as `cardano-recon`, but temporal
    operators such as `□`, `◇`, `○`, and `|` are not permitted).
    For further details about the formula language, consult the [language overview](docs/formula-languages.txt).
  - A JSON array of `TraceMessage` objects, read from a file (`--traces FILE`) or from stdin
    if `--traces` is omitted (e.g. piped from `cardano-recon` on a negative formula outcome).
  - An optional context variables YAML file for variable substitution in formulas.
  - An `--on-missing-key` policy (default: `bottom`) controlling behaviour when a formula
    references a property absent from a message.

#### CLI Syntax

```
Usage: cardano-recon-grep --formulas FILE [--traces FILE]
                            [--context FILE]
                            [--on-missing-key <crash|bottom>]

  Print log events that realise all given continuous formulas (Global Realisation Print)

Available options:
  --formulas FILE          YAML file with a list of ContinuousFormulas
  --traces FILE            JSON array of TraceMessages to filter (default: stdin)
  --context FILE           context variables YAML file
  --on-missing-key <crash|bottom>
                           behaviour when a formula atom references a missing
                           event property key (default: bottom)
  -h,--help                Show this help text
```

## Build flags
  - _debug_ for enabling verbose tracing of formulas as they evolve (multiple traces per each temporal event).
