
Future Plans for graphviz
=========================

This is a list of planned feature improvements to graphviz along with
an indication of when it's likely to be implemented (note that these
time scales are in relation to releases, not actual time; however, it
is quite possible that the order will not be adhered to).

Short term
----------

* Quickstart-style documentation to help users get going with graphviz
  quickly.

* Add nicer syntax for record labels, and specifying ports in Monadic
  Dot graphs.

* Add support for custom shapes; in particular, a nice way of
  re-defining the `Shape` datatype (as just adding a non-nullary
  constructor would make it unwieldy to make sure tests, etc. were
  kept up-to-date).

* Define new classes to distinguish between printing/parsing Attribute
  values and other values (as only the former requires quoted
  variants).

* Clean up AttributeGenerator and get it to use a better
  pretty-printing library.

Medium term
-----------

* Improve the test suite such that the generated `DotGraph` values are
  valid (and thus can be passed to Graphviz proper).  This may not in
  fact be possible as guaranteeing an arbitrary `Attribute` is valid
  is rather tricky (as the value itself needs to be verified,
  especially stateful ones).

* Switch to a proper test-suite library rather than the hand-rolled
  one currently being used.

* Add support for clusters as endpoints of edges.

Long term
---------

* Allow usage of non-FGL graphs with graphviz.  This will require
  implementing a separate library to represent graphs (see initial
  discussions about this
  [here](http://www.haskell.org/pipermail/haskell-cafe/2009-June/063402.html)).
