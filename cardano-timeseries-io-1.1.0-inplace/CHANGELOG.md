# Revision history for cardano-timeseries-io

## 1.1.0 -- May 2026

### Breaking changes

* JSON wire format aligned with Prometheus conventions:
  - Response envelope uses `{"status":"success","data":...}` /
    `{"status":"error","errorType":"...","error":"..."}`.
  - `resultType` / `result` field names replace the previous `tag` / `value`.
  - Prometheus result-type strings (`scalar`, `vector`, `matrix`) used
    throughout.
  - Timestamps and durations encoded as Unix seconds (Double).
  - Data-point values encoded as strings (Prometheus convention).
  - `metric` / `values` field names replace `labels` / `data` in
    instant-vector and time-series objects.

### New features

* `Unit` type: unit literal `()`, pair `(a, b)`, and triple `(a, b, c)`
  expressions.
* `List` type: `Nil` / `Cons` values; `metrics` builtin enumerates all
  metric names known to the store.
* `RangeVector` / `Scalar` arithmetic: range vectors can now appear as
  operands in arithmetic expressions with scalars.
* `Duration + Duration` noncanonical arithmetic rule.
* Three additional unambiguous noncanonical arithmetic rules.
* Local name shadowing: inner `let` bindings correctly shadow outer ones.
* Metric-name awareness in the elaborator: unknown identifiers that are not
  in the metric store now produce an `Undefined name` error with a
  "did you mean?" suggestion instead of a type-mismatch later.

### Bug fixes

* `epoch` keyword was incorrectly parsed; now correctly produces
  `Timestamp 0`.
* `sum_over_time` elaboration bug fixed.
* Parser bug fixed (duration suffix bleed into adjacent identifiers).

### Testing

* Comprehensive test suite added: parser, elaborator, and interpreter suites
  covering 362 cases.

## 1.0.0 -- March 2026

* First version.
