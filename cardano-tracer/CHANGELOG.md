# ChangeLog

## NEXT
* RTView: Remove monitoring based on the `NodePeers` datapoint, which has been removed
  since Node 10.6.2; fixes the RTView-enabled build.
* Introduce secure communication via HTTPS for EKG and Prometheus metric servers, enabled
  by a new boolean `epForceSSL` in the tracer configuration, per-endpoint.
  The specified port will either speak HTTP or HTTPS depending on the flag.
    "hasEKG":        {"epHost": "127.0.0.1", "epPort": 3100, "epForceSSL": true}
    "hasPrometheus": {"epHost": "127.0.0.1", "epPort": 3200, "epForceSSL": true}
  `warp-tls` run conditionally based on this value.
* Certificates (TLS certs, keys, and (optional) chains field) specified by optional field
  in the tracer configuration.
     "tlsCertificate":
       { "certificateFile": "/path/to/certificate.pem"
       , "certificateKeyFile": "/path/to/key.pem"
       , "certificateChain": ["/path/to/intermediate1.pem", "/path/to/intermediate2.pem"]
       }

## 0.3.6 (November 2025)
* Implement Prometheus HTTP service discovery (SD) under the URL `/targets`
* Add optional config field `"prometheusLabels": { "<labelname>": "<labelvalue>", ... }` for custom labels to be attached with Prometheus SD
* Use `TracerTrace.forMachine` directly instead of going via derived `TracerTrace.toJSON`; remove unused `TracerTrace` JSON instances
* Use proper 'camelCase' for machine-readable `TracerTrace`
* Proper tracing (vs. dumping to stdout) for `showProblemIfAny` and for forwarding connection interruptions
* Remove redundant `runInLoop` in favour of `trace-dispatcher`'s implementation
* Split up journal handler implementation into internal modules `Systemd` and `NoSystemd` (maintenance)

## 0.3.5 (October, 2025)
* Updated to `ekg-forward-1.0`, `ouroboros-network-0.22.3`, `ouroboros-network-api-0.16` and `ouroboros-network-0.22.3`.
* Updated metric names

## 0.3.4 (July, 2025)
* Forwarding protocol supports connections over TCP socket, in addition to Unix domain sockets.

## 0.3.3 (April, 2025)
* Redesigned `Cardano.Tracer.Handlers.Notifications.Timer` interface with IO-actions instead of TVars.
* Removed `cardano-node' as a dependency from `cardano-tracer'. This necessitated moving `NodeInfo`
  (from `cardano-tracer:Cardano.Node.Startup` to `trace-dispatcher:Cardano.Logging.Types.NodeInfo`), `NodePeers`
  (from `cardano-node:Cardano.Node.Tracing.Peers` to `trace-dispatcher:Cardano.Logging.Types.NodePeers`), and
  `NodeStartupInfo` (from `cardano-tracer:Cardano.Node.Startup` to `cardano-node:Cardano.Node.Tracing.NodeStartupInfo.hs`).

## 0.3.2 (March 2025)

* When requesting forwarded metrics, ask for delta to previous request only. New config option `ekgRequestFull` defaults to `false`; set to `true` to revert this behavior.
* Add up-to-date symlink for each log rotation, pointing to the most recent file (`node.json` or `node.log`, depending on the log format).
* Fix: CLI option `--min-log-severity` is now observed correctly.
* Add `metricsNoSuffix` to configuration, which when set removes suffixes like `_int` from metrics names, making them more similar to those in the old system (optional boolean; default: `false`).
* Remove `metricsComp` metric name remapping from configuration.
* Updated to use `ekg-forward-0.9`.

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
