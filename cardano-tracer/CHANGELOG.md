# ChangeLog

## 0.2.3 (April 19, 2024)

* The field `rpMaxAgeHours` of `RotationParams` is changed to
  `rpMaxAgeMinutes`. `rpMaxAgeHours` can still be used as a virtual
  field.
* Add `HandleRegistry` to `TracerEnv`, to track handles that have been opened.

## 0.2.2

* Add resource tracing for `cardano-tracer`.
* Add config option: `resourceFreq`.
* Fix benchmark suite.

## 0.2.1

* Updated to `ouroboros-network-0.10`

## 0.2.0

* Updated to `ouroboros-network-0.9.1`

## 0.1.0

Initial version.
