# [SI] Timers

The `si-timers` package provides delays & timeouts which are safe on 32-bit
systems; cancellable timeouts (see `registerDelayCancellable`); a refined
interface for monotonic `Time`.  `Time` is given with left monoid action of
`DiffTime` (which encodes the notion of time differences).  The
`MonadMonotonicTime`, `MonadDelay` type classes & `MonadTimers` (type synonym)
API provide a consistent interface for working with delays and timeouts.

`si-timers` package also defined a low level `MonadTimout` type class.  On
system with a native timer manager (e.g. `Linux`, `MacOS`, `FreeBSD`), it's
very efficient but for other platforms (e.g.  `Windows`), it might not be the
right API for low latency timeouts needed for example for low level networking
code, because it relies on `GHC`'s `RTS` thread scheduling.

`si-timers` are compatible with `io-sim`.

The `SI` comes from the [International System of Units][SI].

[SI]: https://www.wikiwand.com/en/International_System_of_Units
[`io-sim`]: https://hackage.haskell.org/package/io-sim
