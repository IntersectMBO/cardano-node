# ChangeLog

## 0.2.4 (August 13, 2024)

* `systemd` is enabled by default. To disable it use the cabal
  flag: `-f -systemd`.
* Put RTView behind a feature flag that is disabled by default. To enable RTView,
  use the cabal flag `-f +rtview`. No change to the service configuration.
* EKG monitoring moved from `threepenny-gui` to direct HTML rendering.
* Drop dependency on package `threepenny-gui` (unless RTView is enabled).
* Restructured modules `Cardano.Tracer.Handlers.RTView.Notifications.*`
  to `Cardano.Tracer.Handlers.Notifications.*`.
* All modules related to notification, SSL, and others moved from the RTView
  namespace.

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
