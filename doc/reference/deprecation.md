# Deprecation

When deprecating a function, class, type, or data constructor in the cardano-node repo, follow the procedure outlined below:

1. Use the [DEPRECATED](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pragmas.html#warning-and-deprecated-pragmas) pragma.

2. In the deprecation message, indicate what should be used in its place.

3. Include the deprecation in the release notes.

4. Wait 2 release cycles (inclusive of the release of the initial deprecation) to remove the deprecated function, class etc.