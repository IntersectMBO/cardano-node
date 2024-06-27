# IO Monad Class Hierarchy

This package provides a monad class hierarchy which is an interface for both
the [`io-sim`] and [`IO`] monads.  It was developed with the following
constraints in mind:

* be a drop-in replacement for `IO` monad;
* `IO` instances do not alter its original semantics, providing a shallow
  bindings to [`async`], [`base`], [`stm`], and [`exceptions`] packages as well
  as timer API;
* provide zero-cost abstractions.

We provide also non-standard extensions of this API:

* [`strict-stm`]: strict `TVar`'s, and other mutable `STM` variables, with
  support of the [`nothunks`] library;
* [`si-timers`]: timers api:
    - 32-bit safe API using `DiffTime` measured in seconds (rather than time in
      microseconds represented as `Int` as in `base`)
    - cancellable timeouts.

[`strict-stm`] and [`nothunks`] were successfully used in a large code base to
eliminate space leaks and keep that property over long development cycles.

## Exception Class Hierarchy

This package provides an alternative class hierarchy giving access to
exceptions API.  The [`exception`] package class hierarchy is also supported by
[`io-sim`], so you can also use either one.

The `MonadThrow` defined in this package allows working with exceptions without
having explicit access to `catch` or `mask`.  It only provides access to
`throwIO`, `bracket`, `bracket_`, and `finally`.  `MonadCatch` class provides
API which allows working with exceptions, e.g. `catch` or `bracketOnError`, and
`MonadMask` gives access to low-level `mask` and friends.   This division makes
code review process somewhat easier.  Using only `MonadThrow` constraint, the
reviewer can be sure that no low-level exception API is used, which usually
requires more care.  Still `MonadThrow` is general enough to do resource
handling right.

## Time and Timer APIs

The time and timer APIs of this package follows closely the API exposed by
[`base`] and [`time`] packages.  We separately packaged a more convenient API
in [`si-timers`] (ref [SI]), which provides a monoidal action of `DiffTime` on
monotonic time as well as exposes 32-bit safe timer API (on 32-bit systems time
in microseconds represented as an `Int` can only hold timeouts of ~35 minutes).

`Control.Monad.Class.MonadTimer.NonStandard.MonadTimeout` provides a low-level
timeout abstraction.  On systems that support a native timer manager, it's used
to implement its API, which is very efficient even for low-latency timeouts.
On other platforms (e.g. `Windows`), it's good enough for subsecond timeouts
but it's not good enough for fine-grained timeouts (e.g. sub milliseconds) as
it relays on the GHC thread scheduler.  We support `MonadTimeout` on `Linux`,
`MacOS`, `Windows`, and `IOSim` (and unofficially on `GHCJS`).

`MonadDelay` and `MonadTimer` classes provide a well-established interface to
delays & timers.


## Software Transactional Memory API

We provide two interfaces to `stm` API: lazy, included in `io-classes`; and
strict one provided by [`strict-stm`].


## Threads API

We draw a line between `base` API and `async` API.  The former is provided by
[MonadFork](https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadFork.html#t:MonadFork)
the latter by
[MonadAsync](https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadFork.html#t:MonadAsync).
Both are shallow abstractions around APIs exposed by the `base` and `async`
packages.


## Some other APIs

* [MonadEventlog]: provides an API to the [Debug.Trace] event log interface.
* [MonadST]: provides a way to lift `ST`-computations.
* [MonadSay]: dummy debugging interface

## Differences from `base`, `async`, or `exceptions` packages

### Major differences

* `getMonotonicTime` returns `Time` (a newtype wrapper around `DiffTime`)
* `Deadlock` exceptions are not thrown to the main thread (see
  [ref][io-deadlock]), so they cannot be caught. This was a design decision,
  which allows to catch all deadlocks which otherwise could be captured by
  a `catch`.

### Minor differences

Some of the types have more general kind signatures, e.g.

```
type Async :: (Type -> Type) -> Type -> Type
```

The first type of kind `Type -> Type` describes the monad which could be
instantiated to `IO`, `IOSim` or some other monad stacks built with monad
transformers.  The same applies to many other types, e.g. `TVar`, `TMVar`.

The following types although similar to the originals are not the same as the
ones that come from `base`, `async`, or `exceptions` packages:

* `Handler` (origin: `base`)
* `MaskingState` (origin: `base`)
* `Concurrently` (origin: `async`)
* `ExceptionInLinkedThread` (origin: `async`): `io-class`es version does not
  store `Async`
* `ExitCase` (origin: `exceptions`)

## Debuging & Insepction

We provide quite extended debugging & inspection API.  This proved to be
extremely helpful when analysing complex deadlocks or livelocks or writing
complex quickcheck properties of a highly concurrent system.  Some of this is
only possible because we can control the execution environment of [`io-sim`].

* `labelThread` as part of `MonadThread` ([`IO`], [`io-sim`], which is also
  part of `GHC` API, ref [`labelThread`][labelThread-base]);
* `MonadLabelledSTM` which allows to label of various `STM` mutable variables,
  e.g. `TVar`, `MVar`, etc. ([`io-sim`], not provided by `GHC`);
* `MonadInspectSTM` which allows inspecting values of `STM` mutable variables
  when they are committed. ([`io-sim`], not provided by `GHC`).


## Monad Transformers

We provide support for monad transformers (although at this stage it might have
its limitations and so there might be some rough edges.  PRs are welcomed,
[contributing]).

[SI]: https://www.wikiwand.com/en/International_System_of_Units 
[`DiffTime`]: https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Clock.html#t:DiffTime
[`IO`]: https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-IO.html#t:IO
[`async`]: https://hackage.haskell.org/package/async
[`base`]: https://hackage.haskell.org/package/base
[`exceptions`]: https://hackage.haskell.org/package/exceptions
[`io-sim`]: https://hackage.haskell.org/package/io-sim
[`si-timers`]: https://hackage.haskell.org/package/si-timers
[`stm`]: https://hackage.haskell.org/package/stm
[`strict-stm`]: https://hackage.haskell.org/package/strict-stm
[`threadDelay`]: https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadTimer.html#v:threadDela
[`time`]: https://hackage.haskell.org/package/time
[contributing]: https://www.github.com/input-output-hk/io-sim/tree/master/CONTRIBUTING.md
[`nothunks`]: https://hackage.haskell.org/package/nothunks
[labelThread-base]: https://hackage.haskell.org/package/base-4.17.0.0/docs/GHC-Conc-Sync.html#v:labelThread
[io-deadlock]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Exception.html#t:Deadlock

[MonadEventlog]: https://hackage.haskell.org/package/io-sim-classes/docs/Control-Monad-Class-MonadEventlog.html#t:MonadEventlog
[Debug.Trace]: https://hackage.haskell.org/package/base/docs/Debug-Trace.html
[MonadST]: https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadST.html#t:MonadST
[MonadSay]: https://hackage.haskell.org/package/io-classes/docs/Control-Monad-Class-MonadSay.html#t:MonadSay
