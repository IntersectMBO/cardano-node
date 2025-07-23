# [IO Simulator Monad][`io-sim`]

A pure simulator monad built on top of the lazy `ST` monad which supports:

  * optional dynamic race discovery and schedule exploration
  * synchronous and asynchronous exceptions; including: throwing, catching and
    masking synchronous and asynchronous exceptions;
  * concurrency (using simulated threads), with interfaces shaped by the
    `base` and `async` libraries;
  * software transactional memory (`STM`);
  * simulated time;
  * timeouts;
  * dynamically typed traces and event log tracing;
  * lifting any `ST` computations;
  * inspection of `STM` mutable data structures;
  * deadlock detection;
  * `MonadFix` instances for both [`IOSim`] and its corresponding `STM` monad.
  * partial order reduction (see [`IOSimPOR`]).

[`io-sim`] together with [`io-classes`] is a drop-in replacement for the `IO`
monad (with some ramifications).  It was designed to write easily testable
Haskell code (including simulating socket programming or disk IO).  Using
[`io-classes`] and [`si-timers`] libraries one can write code that can run in
both: the real `IO` and the [`IOSim`] monad provided by this package.  One of the
design goals was to keep the API as close as possible to `base`, `exceptions`,
`async`, and `stm` packages.

[`io-sim`] package also provides two interpreters, a standard one and [`IOSimPOR`]
which supports dynamic discovery of race conditions and schedule exploration
with partial order reduction.

[`io-sim`] provides API to explore traces produced by a simulation.  It can
contain arbitrary Haskell terms, a feature that is very useful to build
property-based tests using `QuickCheck`.

The package contains thorough tests, including tests of `STM` against the
original specification (as described in [Composable Memory
Transactions](https://research.microsoft.com/en-us/um/people/simonpj/papers/stm/stm.pdf)
and its `GHC` implementation.  This can be seen in both ways: as a check that
our implementation matches the specification and the `GHC` implementation, but
also the other way around: that `GHC`s `STM` implementation meets the
specification.

[`io-sim`]:  https://hackage.haskell.org/package/io-sim
[`io-classes`]: https://hackage.haskell.org/package/io-classes
[`si-timers`]: https://hackage.haskell.org/package/si-timers
[`IOSimPOR`]: https://github.com/input-output-hk/io-sim/tree/main/io-sim/how-to-use-IOSimPOR.md
[`IOSim`]: https://hackage.haskell.org/package/io-sim/docs/Control-Monad-IOSim.html#t:IOSim
