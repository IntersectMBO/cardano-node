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

## Build flags
  - _debug_ for enabling verbose tracing of formulas as they evolve (multiple traces per each temporal event).
  - _crash_on_missing_key_ for making the application crash if a formula atom is evaluated on an event which is missing
    a key the atom is referencing.
