# Revsion history of io-classes

## 1.5.0.0

### Breaking changes

* `MonadST` depends on `PrimMonad`.
* Provide a default implementation of `withLiftST`.
* Added `annotateIO` to `MonadThrow` (only supported for ghc-9.10 or newer).

### Non-breaking change

* Add `writeTMVar` to `MonadSTM`.

## 1.4.1.0

### Non-breaking changes

* New dependency on `primitive` package
* New `stToIO` in `MonadST`, which is simpler to use than the existing
  `withLiftST`, and depends on the `primitive` package's `PrimState` (#141).

## 1.4.0.0

### Breaking changes

* Removed deprecated module `Control.Monad.Class.MonadVar`.

### Non-breaking changes

* Fixed some module haddock typos.

## 1.3.1.0

### Non-breaking changes

* Added `forkFinally` to `MonadFork`.

## 1.3.0.0

- `io-sim-1.3.0.0`.
* Support `ghc-9.8`.

## 1.2.0.0

### Non-breaking changes

* Add new `MonadInspectMVar` class with an `inspectMVar` function for accessing
  an `MVar` in an underlying monad (if applicable). This is mainly useful for
  `io-sim`, since the underlying monad is `ST`. `IO` has no underlying monad, so
  the provided instance for `IO` defaults `inspectMVar` to `tryReadMVar`.
* Add some Haddock documentation to `MonadDelay`

## 1.1.0.0

### Breaking changes

* `Control.Monad.Class.MonadMVar` is now deprecated in favour of
  `Control.Concurrent.Class.MonadMVar`.

### Non breaking changes

* Fixed building haddocks with `ghc-8.10`.

## 1.0.0.1

### Non breaking changes

* Support `ghc-9.6`.

## 1.0.0.0

### Breaking changes

* `MonadMonotonicTime` morphed into `MonadMonotonicTimeNSec` which supports
  `getMonotonicTimeNSec` from "base".  `MonadMonotonicTime` can be found in new
  package `si-timers`.
* A simplified `MonadTimer` which is using time in microseconds encoded as
  `Int` rather than `DiffTime`.  The previous interface can be found in the
  package `si-timers`.
* The non standard timer API is moved from `MonadTimer` to a `MonadTimerFancy`
  type class which can be imported from
  `Control.Monad.Class.MonadTimer.NonStandard` module.

### Non breaking changes

* Added `registerDelayCancellable` to `Control.Monad.Class.MonadTimer` module.

## 0.6.0.0

## 0.5.0.0

* fixed `tryTakeMVarDefault`, `takeMVarDefault`, `putMVarDefault` and
  `isEmptyMVarDefault`.
* Implemented the proper `readMVar` semantics.
* Added `tryReadMVar` to `MonadMVar` type class.
* Moved `MVarDefault` to `io-sim` package.

## 0.4.0.0

* support `ghc-9.4` (except on Windows input-output-hk/io-sim#51)

## 0.3.0.0

* added missing `MArray` instance for `WrappedSTM`
* added monoid instances for `WrappedSTM`
* added `threadStatus` method of `MonadFork` class
* removed `linktTo` and `linkToOnly`
* added `labelTChan`
* added `flushTQueue`
* split `MonadSTM` into submodules in a similar way that the `stm` package is.
* added `TChan`, `TSem` and `TArray`
* added `interruptible` and `allowInterrupt` to `MonadMaskingState` type class.
* added `unGetTQueue` & `unGetTBQueue`
* support `ghc-9.2` & `ghc-9.4`
* added `MonadMVar`

## 0.2.0.0

* First version published on Hackage.
* added `lengthTBQueue` to `MonadSTM`; `io-classes` now depends on `stm-2.5.0.0`.

## 0.1.0.0

* Initial version, not released on Hackage.
