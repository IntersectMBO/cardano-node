# ChangeLog

## 0.3.1 (January 22, 2025)

* Fix race condition when querying `NodeInfo` data point, occasionally resulting in fallback `NodeName`s instead of proper ones.
* Updated to `typed-protocols-0.3`.

## 0.3 (September 26, 2024)

* Fix the creation of empty logs.
* Abondon `snap` webserver in favour of `wai`/`warp` for Prometheus and EKG Monitoring.
* Add dynamic routing to EKG stores of all connected nodes.
* Derive URL compliant routes from connected node names (instead of plain node names).
* Remove the requirement of two distinct ports for the EKG backend (changing `hasEKG` config type).
* Improved OpenMetrics compliance of Prometheus exposition; also addresses [issue#5140][i5140].
* Prometheus help annotations can be provided via the new optional config value `metricsHelp`.
* For optional RTView component only: Disable SSL/https connections. Force `snap-server`
  dependency to build with `-flag -openssl`.
* Add JSON responses when listing connected nodes for both Prometheus and EKG Monitoring.
* Fix: actually send `forHuman` rendering output to journald when specified.
* Add consistency check for redundant port values in the config.

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



[i5140]: https://github.com/IntersectMBO/cardano-node/issues/5140
