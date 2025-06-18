# Strict Software Transaction Memory

The `strict-stm` package provides a strict interface to software transaction
memory.  It builds on top of [`io-classes`] and thus it provides the interface
for both [`stm`] as well as [`io-sim`].

# Novel testing / space-leak elimination approach

The strict interface provides a novel way of testing/eliminating space-leaks
which might lurk in `stm` shared mutable variables.  Together with the
[`nothunks`] library it was successfully used to eliminate and keep a large
system ([`cardano-node`]) space leak free.

[`cardano-node`]: https://www.github.com/input-output-hk/cardano-node
[`io-classes`]: https://hackage.haskell.org/package/io-classes
[`io-sim`]: https://hackage.haskell.org/package/io-sim
[`nothunks`]: https://hackage.haskell.org/package/nothunks
[`stm`]: https://hackage.haskell.org/package/stm
