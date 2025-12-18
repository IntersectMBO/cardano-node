# sop-extras Changelog

# Changelog entries

<a id='changelog-0.4.1.0'></a>
## 0.4.1.0 -- 2025-09-30

### Non-Breaking

- Add `ctraverse`, `traverse_`, and `HTraverse_` to `OptNP`

<a id='changelog-0.4.0.0'></a>
## 0.4.0.0 -- 2025-04-16

### Breaking

- Delete the `K2` combinator.
- Define `Fn2` and add `composeFromTo`.
- Define `telescopeMismatch`.
- Define `inPairsToTails` and `extendWithTails`.

<a id='changelog-0.3.0.0'></a>
## 0.3.0.0 -- 2025-04-03

### Breaking

- Refactor `Index` to use `NS ((:~:) x) xs`. Pattern synonyms `IZ` and `IS` are
  provided, making this change transparent for users. Some functions need
  additional constraints, hence the breaking nature of this change.

<a id='changelog-0.2.2.0'></a>
## 0.2.2.0 -- 2025-03-25

### Non-Breaking

- Bump upper bound on `base` dependency.

<a id='changelog-0.2.1.0'></a>
## 0.2.0.0 -- 2024-08-26

### Non-Breaking

- Bump to `nothunks` 0.2

<a id='changelog-0.2.0.0'></a>
## 0.2.0.0 -- 2024-05-13

### Non-Breaking

- Added `K1` and `Flip` basic functors (poly-kinded).
- Provide `hczipWith` for `InPairs`.
- Added `HTrans` instances to `OptNP`, `Match`, `Telescope`.

### Breaking

- `Index` became poly-kinded.

<a id='changelog-0.1.0.0'></a>
## 0.1.0.0 -- 2023-08-25

* First version. Released on an unsuspecting world.
