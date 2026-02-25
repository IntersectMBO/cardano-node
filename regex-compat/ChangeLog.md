## 0.95.2.2

_2025-03-02 Andreas Abel_

- Drop support for GHC 7
- Make `Prelude` imports explicit, add `LANGUAGE NoImplicitPrelude`
- Make upper bounds of dependencies major-major (all are shipped with GHC)
- Tested with GHC 8.0 - 9.12.1

## 0.95.2.1 revision 2

- Allow `base >= 4.17` (GHC 9.4)

## 0.95.2.1 revision 1

- Allow `base-4.16` (GHC 9.2)

## 0.95.2.1

- Allow `base-4.15` (GHC 9.0)

- Workaround for `{-# LANGUAGE Haskell2010 #-}` parser regression introduced in GHC 9.0

- Optimization flag `-O2` has been removed

## 0.95.2.0

- Declare `Text.Regex` module `Trustworthy` under SafeHaskell
