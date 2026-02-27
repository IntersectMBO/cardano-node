# Cardano Re(altime) Con(formance) Framework

## What it does

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

## CLI Syntax

```
Usage: cardano-recon FILE --mode <offline|online> --duration INT FILES 
                     [--retention INT] [--trace-dispatcher-cfg FILE] 
                     [--context FILE] [--dump-metrics BOOL] [--seek-to-end BOOL]

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
  -h,--help                Show this help text
```

## Build flags
  - _debug_ for enabling verbose tracing of formulas as they evolve (multiple traces per each temporal event).
  - _crash_on_missing_key_ for making the application crash if a formula atom is evaluated on an event which is missing
    a key the atom is referencing.
