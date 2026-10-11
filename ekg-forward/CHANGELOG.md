# Changelog

## 1.2 - Mar 2026

* Generalised metrics acceptor to work with an arbitrary store.
* Updated to `ouroboros-network-1.1`.

## 1.1 - Mar 2026

* Updated to `typed-protocols-1.2`.
* Updated to `ouroboros-network-1.0`.

## 1.0 - Sep 2025

* Updated to `typed-protocols-1.0`.
* Updated to `ouroboros-network-framework-0.19`.

## 0.9 - Mar 2025

* New `Deltify` data type that selects only the delta/changes of metrics, keeping a reference to the previously transmitted sample in a back buffer.
* Saving bandwidth: New request constructor `GetUpdatedMetrics` which asks for changed metrics only - with the initial back buffer being empty.
* Saving bandwidth: New boolean field `useDummyForwarder` in `ForwarderConfiguration` to forcibly use a dummy forwarder, i.e. when the consumer is certain it wont require metrics at all.
* New test case for `GetUpdatedMetrics`.
* Updated to `ouroboros-network-framework-0.17`.

## 0.8.1

* Updated to `ouroboros-network-framework-0.16`.

## 0.8

* Updated to `network-mux-0.6` and `ouroboros-network-framework-0.15`

## 0.7 - Oct 2024

* Updated to `typed-protocols-0.3`.

## 0.6.0 - Sep 2024

* Remove potentially leaky continuation passing of `EKGForwarder`.
* Bump dependency version bounds.

## 0.5.0

* Bump dependency version bounds

## 0.4.0

* Updated to `ouroboros-network-framework-0.8`.

## 0.1.0

* Initially created.
