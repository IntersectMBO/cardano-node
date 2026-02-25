# Revision history for nothunks

## 0.2.1.0 -- 2024-02-06

* Exported `mkThunkInfo`.
* Test support of `ghc-9.10`.

## 0.2.1.0 -- 2024-02-06

* Support `wherefrom` with `GHC-9.2` or newer. (Teo Camarasu, [#49](https://github.com/input-output-hk/nothunks/pull/49))

## 0.2.0 -- 2024-01-27

* Use `whereFrom` to get source information, which is avialable when the source
  is compiled with `GHC-9.6` (or newer) and with `-finfo-table-map` (and even
  more accurate when `-fdistinct-constructor-table` is passed).
  For that reason the `ThunkInfo` type has changed.
* `NoThunks` instance for `Data.Tuple.Solo`.
* `NoThunks` instances for `Data.Semigroup` and `Data.Monoid` newtype wrappers.

## 0.1.5 -- 2023-10-29

* `NoThunks ThreadId` instance.
* `NoThunks Identity` instance
* Fix tests on ghc 9.8.
  Andreas Abel <andreas.abel@gu.se>
* Tested with ghc 8.10 to 9.8.

## 0.1.4 -- 2023-03-27

* Made cabal flags manual.
* Support ghc-9.2 to 9.6.
* `ThunkInfo` is a newtype.

## 0.1.3 -- 2021-06-28

* Fix tests on ghc-9.0.1
  Joe Hermaszewski <git@monoid.al>
* Make bytestring, text and vector optional dependencies
  Bodigrim <andrew.lelechenko@gmail.com>

## 0.1.2 -- 2020-12-03

* Add IORef, MVar and TVar instances.
  Oleg Grenrus <oleg.grenrus@iki.fi>

## 0.1.1.0 -- 2020-09-29

* Export `Context` and `GWNoThunks`
* Fix typos in Haddocks
* Improve bounds (and add upper bounds)

## 0.1.0.0 -- 2020-09-09

* Initial public release
