# Revision history of io-sim

## 1.5.0.0

### Breaking changes

- Generalised the type of `traceSelectTraceEvents` & co.

### Non-breaking changes

- Added `writeTMVar` to `MonadSTM` instance for `(IOSim s)`.
- Fixes IOSimPOR test failure (see issue #154).
- Reverted commit 4534b6eae64072a87bd81584f479a123681358a3 which uses
  `unsafePerformIO` instead of ST, to regain lazyness on infinite simulations.
- Added a test to check for lazyness on infinite simulations

## 1.4.1.0

### Non-breaking changes

* QuickCheck monadic combinators: `monadicIOSim`, `monadicIOSim_` and `runIOSimGen` (#140).
* New dependency on `primitive`
* Provides an instance for `PrimMonad`, giving access to most functionality
  from the `primitive` package (#141).
* Prevented STM waking up threads blocked on `threadDelay` (#142).

## 1.4.0.0

### Breaking changes

* Removed `EventTimerUpdated` constructor (not used anymore).

### Non-breaking changes

* `Alternative` & `MonadPlus` instances for `IOSim`.
* Fixed `flushTQueue` implemetation.

## 1.3.1.0

### Non-breaking changes

* Optimised `io-sim` performance (improved memory footprint).
* Fixed a bug in `io-sim-por`: `execAtomically'` should not commit tvars.

## 1.3.0.0

### Breaking changes

* `MainReturn`, `MainException` and the pattern synonyms `TraceMainReturn`,
  `TraceMainException` changed their signature.  They will now also show the main thread id.
* Renamed `ThreadId` to `IOSimThreadId` to avoid a clash with `ThreadId`
  associated type family of `MonadFork`.  It makes it much simpler to paste
  failing `ScheduleControl` in `ghci` or tests.
* `BlockedReason` was modified: `BlockedOnOther` was removed, in favour of `BlockedOnDelay` and `BlockOnThrowTo`.
* The `Failure` type (for example returned by `runSim`) now also contains
  a constructor for internal failures.  This improved error reporting when
  there's a bug in `IOSimPOR`.  Currently it's only used by some of the
  assertions in `IOSimPOR`.

#### Non-breaking changes

* Refactored the internal API to avoid `unsafePerformIO`.
* Fixed bugs which lead to discovery of schedules which are impossible to run.
* Added haddocks, refactored the code base to improve readability.
* Fixed reported `step` in `EventTxWakup`
* Added debugging information schedule, (`explorationDebugLevel` option).
  Mostly useful for debugging `IOSimPOR` itself.  This information will
  contains `Effect`, discovered races and schedules.
* Addded or improved pretty printers for `SimTrace`.  Among other changes,
  a racy `StepId`: `(RacyThreadId [1,2], 2)`, is now pretty printed as `Thread
  {1,2}.2`, a non racy step is printed as `Thread [1,2].2`.
* Fixed trace of calls to the `deschedule` function.
* Exposed `Timeout` type as part of the `newTimeout` API.
* When `explorationDebugLevel` is set, avoid printing the same trace twice.
* Reimplemented `labelTVarIO` and `traceTVarIO` in `ST` monad, which simplifies
  trace of these calls.
* Fixed `traceTVar` for `TVar`'s created with `registerDelay`.
* Added pretty printer for `SimResult`, and other pretty printer improvements.
* Support `ghc-9.8`.

## 1.2.0.0

### Breaking changes

* `selectTraceEvents`, `selectTraceEvents'` catpure time of events.
* Added select function which capture the time of the trace events:
  - `selectTraceEventsDynamicWithTime`
  - `selectTraceEventsDynamicWithTime'`
  - `selectTraceEventsSayWithTime`
  - `selectTraceEventsSayWithTime'`

### Non-breaking changes

* Provide `MonadInspectMVar` instance for `IOSim`.
- Added NFData & NoThunks instances for `ThreadId`

## 1.1.0.0

### Non-breaking changes

* `io-classes-1.1.0.0`

## 1.0.0.1

### Non-breaking changes

* Support `ghc-9.6`.

## 1.0.0.0

### Breaking changes

* Support refactored `MonadTimer`, and new `MonadTimerFancy`, `MonadTimeNSec`
  monad classes.

## 0.6.0.0

### Breaking changes

* Added `TimeoutId` to `EventThreadDelay` and `EventThreadFired` events.

### Non-breaking changes

* Fixed `threadDelay` in presence of asynchronous exceptions (in `IOSim` and `IOSimPOR`) (#80).
* Fixed bug in `IOSim` & `IOSimPOR` which resulted in reusing existing
  `ThreadId`s (#80).

## 0.5.0.0

* `MVar` type family is not injective anymore.
* Removed default implementation of `readMVar` in the `MonadMVar` type class.
* Moved `MVarDefault` to `io-sim` package.

## 0.4.0.0

* support `ghc-9.4` (except on Windows input-output-hk/io-sim#51)
* `MonadCatch` instance for `STM (IOSim s)` monad
* fixed `isEmptyTBQueeuDefault` (thanks to @EMQ-YangM)
* refactored internal implementation of timeouts, changed `SimEventType`
  constructors

## 0.3.0.0

* added `Functor` instance for `SimResult`
* added `MonadFix` instance for `STM (IOSim s)`
* support `ghc-9.2` & `ghc-9.4`

## 0.2.0.0

* First version published on Hackage.
* Depends on `io-classes-0.2.0.0`.

## 0.1.0.0

* Initial version, not released on Hackage.
