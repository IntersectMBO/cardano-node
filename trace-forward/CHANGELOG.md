# ChangeLog

## 2.4.0 - Nov 2025

* Refactor `writeToSink` and `readFromSink` to simplify STM usage
* Drop incongruous logic of switching queue capacity from `ForwardSink`, along with related fields and code
* Provide `InitForwardingConfig` config record type for `initForwarding` and `initForwardingDelayed`

## 2.3.1 - Oct 2025

* Updated to `ekg-forward-1.0`, `ouroboros-network-framework-0.19.2` and `typed-protocols-1.0`.

## 2.3.0 - Jul 2025

* Remove unused `forwarderEndpoint` and `acceptorEndpoint` fields from forwarder / acceptor configuration types.
* `trace-forward` now depends on `trace-dispatcher`, and not the other way round.

## 2.2.11 - Mar 2025

* Updated to `ouroboros-network-framework-0.17`

## 2.2.10 - Feb 2025

* Updated to `ouroboros-network-framework-0.16`

## 2.2.9 - Jan 2025

* Updated to `typed-protocols-0.3`.

## 2.2.8 - Oct 2024

* Bump for version bound

## 2.2.7 - Sep 2024

* Remove potentially leaky continuation passing from `TraceObjectForwarder` and `DataPointForwarder`.

## 2.2.2 - Dec 2023

* with overflow callback

## 2.2.1 - Oct 2023

* Updated to `ouroboros-network-api-0.6` and `ouroboros-network-framework-0.10`

## 2.1.0 - Sep 18 2023

* Updated to `ouroboros-network-api-0.5.1` and `ouroboros-network-framework-0.9.0`.

## 2.0.0 - May 2023

* Undocumented changes

## 1.34.5 - November 2022

* No changes

## 0.1.0 - October 2022

* Initial Release
