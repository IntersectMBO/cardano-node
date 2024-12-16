# Revision history for mux

## next release

## 0.4.5.3 -- 2024-08

### Breaking changes

### Non-breaking changes

* Make it build with ghc-9.10

## 0.4.5.2 -- 2024-06-07

### Breaking changes

### Non-breaking changes

- Bump io-sim and io-classes

## 0.4.5.1 -- 2024-01-22

### Non-breaking changes

* Use io-sim-1.4.1.0
- Bump `Win32-network` version

## 0.4.5.0 -- 2024-01-22

### Non-breaking changes

* ghc-9.8 support.

## 0.4.3.0 -- 2023-11-16

### Non-breaking changes

* Make sure jobs are removed from the `JobPool`.
* Use `io-sim-1.3.1.0`.

## 0.4.3.0 -- 2023-11-16

### Non-breaking changes

* Use `io-sim-1.3.0.0`.

## 0.4.2.0

### Breaking

### Non-breaking

* `asserts` cabal flag was removed, one can use `ghc-options` directly
* Restructured `step` to prevent an impossible case and eliminate the associated
  `error`.
* Restructured `setupDispatchTable` to prevent an impossible case and eliminate
  the associated error.

## 0.4.1.0

* Use `io-classes-1.2`

## 0.4.0.0 -- 2023-04-28

### Breaking

* Use `io-classes-1.1`.
* Renamed `MuxTraceShutdown` as `MuxTraceStopping`.
* Fixed a typo now the mux stopping exception carries message: `Mux stopping`.


## 0.3.0.0 -- 2023-01-25

* Provide a `MakeBearer` newtype: a function to constructs a `MuxBearer`.
* Fix NodeToNodeV10 support
* Fix invalid Haddock markup
* Support `ghc-9.2`

## 0.2.0.0 -- 2022-11-11

* Bump versions of packages
* Platform independent TCP info trace
