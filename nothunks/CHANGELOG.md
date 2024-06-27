# Revision history for nothunks

## next version

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
