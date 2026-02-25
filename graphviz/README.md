
The graphviz Library
====================

The _graphviz_ library provides bindings to the [Graphviz] graph
visualisation suite of tools for the purely functional programming
language [Haskell].  It can be downloaded from [HackageDB] or - if you
have [cabal-install] - installing it is as simple as:

~~~~~~~~~~~~~~~~~~~~ {.bash}
cabal update
cabal install graphviz
~~~~~~~~~~~~~~~~~~~~

[Graphviz]: http://www.graphviz.org/
[Haskell]: http://haskell.org/
[HackageDB]: http://hackage.haskell.org/package/graphviz
[cabal-install]: http://haskell.org/haskellwiki/Cabal-Install

Library features
----------------

Main features of the graphviz library include:

* Almost complete coverage of all Graphviz attributes and syntax.

* Support for specifying clusters.

* The ability to use a custom node type.

* Functions for running a Graphviz layout tool with all specified
  output types.

* The ability to not only generate but also parse Dot code with two
  options: strict and liberal (in terms of ordering of statements).

* Functions to convert [FGL] graphs and other graph-like data
  structures to Dot code - including support to group them into
  clusters - with a high degree of customisation by specifying which
  attributes to use and limited support for the inverse operation.

* Round-trip support for passing an [FGL] graph through Graphviz to
  augment node and edge labels with positional information, etc.

[FGL]: http://web.engr.oregonstate.edu/~erwig/fgl/haskell/

graphviz is free software licensed under a [3-Clause BSD License].

\(C\) 2008 [Matthew Sackman](http://www.wellquite.org/)

\(C\) 2008 - onwards [Ivan Lazar Miljenovic](http://ivanmiljenovic.wordpress.com/)

[3-Clause BSD License]: http://www.opensource.org/licenses/bsd-license.php