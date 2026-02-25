# Revision history for constraints-extras

## 0.4.0.2

* Support GHC 9.12

## 0.4.0.1

* Support GHC 9.10
* Bump `base` and `template-haskell` upper bounds

## 0.4.0.0 revision 2 - 2023-11-20

* Support GHC 9.8

  Bump `base` and `template-haskell` upper bounds

## 0.4.0.0 revision 1 - 2023-05-10

* Support GHC 9.6

  Bump `base` and `template-haskell` upper bounds

## 0.4.0.0 - 2022-11-18

* Make `Has` from a type synonym into the class upon which everything else is based. Instances may define either `has` or `argDict` which are now both methods of `Has`. This should hopefully improve the readability of type errors involving the library a fair bit, as everything wanted to use `Has`, but it was defined in terms of the less commonly appearing `ArgDict` and `ConstraintsFor`.
* The `ConstraintsFor` type family has been removed as it is now unnecessary, as instances of `Has` can simply be constrained directly. This has the added benefit of allowing `QuantifiedConstraints` in those instance heads that formerly would not have been allowed as part of the result of a type family.
* The `ArgDict` class has also been removed, as it was also basically never used on its own.

## 0.3.2.1 - 2021-12-17

* Support GHC 9.2

## 0.3.2.0 - 2021-10-28

* Provide `ArgDict` instances for sums of functors.

## 0.3.1.0 - 2021-03-24

* Allow deriving instances with `deriveArgDict` for data and newtype family instances by supplying the name of one of its constructors
* Support GHC 9.0.1

## 0.3.0.3 - 2020-06-22

* Update version bounds for GHC 8.10

## 0.3.0.2 - 2019-09-30

* Update version bounds for GHC 8.8

## 0.3.0.1 - 2019-05-17

* Drop markdown-unlit in favor of using regular "Bird"-style LHS to avoid some cross-compilation problems

## 0.3 - 2019-05-16

* Added a parameter for the type class, to allow for custom not-fully-polymorphic instances of ArgDict in cases where e.g. your key type contains dictionaries for specific classes. You will now need FlexibleInstances, MultiParamTypeClasses for the instances created by deriveArgDict.

## 0.2.3.5 - 2019-05-04

* Bumped version bounds on base and template-haskell to admit the versions from GHC 8.6.x

## 0.2.3.4 - 2019-03-22

* Added ChangeLog.md
* Replaced some occurrences of <> in Data.Constraint.Extras.TH with ++ so that the module will hopefully build with GHC 8.0.2 and 8.2.2 without needing to import Data.Semigroup.
