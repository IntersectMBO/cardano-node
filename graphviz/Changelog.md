
Release History and Changelog
=============================

The following is information about what major changes have gone into
each release.

Changes in 2999.20.2.0
----------------------

* Metadata update.
* Relax QuickCheck upper boundary.

Changes in 2999.20.2.0
----------------------

* Add `PrintDot` instances for `Word32` and `Word64`.
* Dependency bumps.

Changes in 2999.20.1.0
----------------------

* Add `MonadFix` instance for `DotM` (thanks to **George Wilson**)

* Fix exception catching for missing executables (thanks to **Kostas
  Dermentzis**)

* Dependency bumps.

Changes in 2999.20.0.4
----------------------

* Dependency bumps.

Changes in 2999.20.0.3
----------------------

* Dependency bumps.

Changes in 2999.20.0.2
----------------------

* Fix Haddock issue (thanks to **Moritz Kiefer**)

* Bump HSpec upper bound (thanks to **Moritz Kiefer**)

* Make Hackage happier with Cabal-Version field

Changes in 2999.20.0.1
----------------------

* Allow building with temporary-1.3.*.

Changes in 2999.20.0.0
----------------------

* Can now create subgraphs using the Monadic representation.

* Allow unescaped `-` and `'` in HTML labels (thanks to **Andrey
  Kartashov**)

* Support for strict `Text` instances for printing/parsing.

* Creating a Graph representation with `mkGraph` was not adding edges
  correctly (reported by **Joshua Chia**).

* Test suite now uses HSpec, making it a lot easier to add tests for
  specific issues.

* Builds with GHC 8.4.* (thanks to **Tony Day**).

* Monoid and Semigroup instances for Monadic representation (thanks to
  **Chris Martin**).

* Be more lenient in parsing some attributes (e.g. allow `top` instead
  of just `t` for `VerticalPlacement`).

* Add new HTML attributes: `Columns`, `GradientAngle`, `Rows`, `Sides`
  and `Style`.

* Improve/update the TestParsing executable to work on being able to
  parse all the sample `Dot` graphs shipped with Graphviz.

* Bump dependencies.

Changes in 2999.19.0.0
----------------------

* Roll back change in 2999.18.1.0 for Monadic graphs, as they turn out
  to not actually work in practice (reported by **Lennart Spitzner**).

* Add a `quickParams` value to help with testing graphs in ghci
  (requested by **Ian Jeffries**).

* Fix parsing of edge chains (reported by **Jonas Collberg**).

* Fix how seemingly numeric text literals are quoted (reported by
  **Joey Hess**).

Changes in 2999.18.1.2
----------------------

* Allow dlist-0.8, thanks to **Sean Leather**.

* Properly allow QuickCheck-2.9.

Changes in 2999.18.1.1
----------------------

* Bump allowed version of QuickCheck for the test suite.

Changes in 2999.18.1.0
----------------------

* Monadic graphs can now have edges to and from lists of nodes, as
  requested by **Harold Carr**.

* Compatible with GHC-8.* pre-releases.

    - Please note that there is currently a warning emitted on
      compilation by GHC-8.*; this has been reported
      [here](https://ghc.haskell.org/trac/ghc/ticket/11822).

Changes in 2999.18.0.2
----------------------

* Match new behaviour of `dot`, etc. in supporting edge specifications
  like `a, b -> c, d` (spotted by **Dilawar Singh**).

Changes in 2999.18.0.1
----------------------

* Loosened bound on `fgl-arbitrary`.

Changes in 2999.18.0.0
----------------------

* Now builds on GHC 7.10.*.

* Attribute changes:

    - Added the `NoTranslate` attribute.

    - Add support for overlines in HTML labels.

    Both of these are available as of Graphviz 2.38.0, but were
    somehow overlooked in previous releases of this library.

* Uses new `fgl-arbitrary` library for testing rather than custom
  instances for FGL graphs.

* Migrated to git; central repository now on
  [GitHub](https://github.com/ivan-m/graphviz).

Changes in 2999.17.0.2
----------------------

* Augmentation tests are no longer run by default.  This avoids
  problems where Graphviz is not installed/configured properly.

* Use `.gv` for temporary files instead of `.dot`.

Changes in 2999.17.0.1
----------------------

* Allow usage of `transformers-0.4.*`, by **Sergei Trofimovich**.

Changes in 2999.17.0.0
----------------------

* Add support for Graphviz 2.32.0, 2.32.1, 2.34.0, 2.36.0 and 2.38.0:

    - **WARNING**: at least as of Graphviz-2.32.0, `dot -Tcanon` no
      longer produces Dot code that is in the format expected by the
      Canonical Dot representation in this library.  As such, unless
      you're very sure of your sources you should _always_ use the
      Generalised representation for parsing (see also the new
      `FromGeneralisedDot` class mentioned below).

    - New attributes:

        + `XDotVersion` (as of 2.34.0)

        + `InputScale` (as of 2.36.0)

        + `OverlapShrink` (as of 2.36.0)

    - Changed attributes:

        + `Aspect` no longer available (as of 2.36.0)

        + New `ModeType` values for use with `sfdp`: `SpringMode` and
          `MaxEnt`.

        + `Weight` now takes a value of type `Number`, that explicitly
          distinguishes between `Doubles` and `Ints`.

        + `FixedSize` and `Normalize` now have their own types.

        + New `Shape`s: `Star` and `Underline` (as well as `Square`
          which seems to have been omitted from previous versions).

    - Other relevant changes:

        + `XDot` now takes an optional version.  Note that this
          doesn't have any effect on how _graphviz_ works.

        + The default extension for Dot-graphs is now `.gv` rather
          than `.dot` to reflect Graphviz's changed conventions.

* Other changes to the API:

    - Add the `FromGeneralisedDot` class, which provides a
      semi-inverse to `fromCanonical` in `DotRepr`.

    - `GraphID` now uses `Number` rather than separate `Int` and
      `Double` constructors; this only matters if you manually
      constructed or de-constructed `GraphID` values (`ToGraphID`
      still works).

    - Add the ability to parse a Dot graph "liberally": that is, if an
      `Attribute` doesn't match the specification, then let it fall
      back to an `UnknownAttribute`.  This is still experimental, and
      requires more manual usage than the in-built commands (e.g. it
      isn't supported in the default round-tripping).

    - Now using the definition of `bracket` from `polyparse >= 1.9`.

    - Monadic representation now has `Functor` and `Applicative`
      instances to satisfy the up-coming changes in GHC 7.10.

* Compilation time has been reduced in two ways:

    - The `Data.GraphViz.Attributes.Complete` module has been split up
      (but still exports the same API, so no need to import more
      modules).  Whilst I haven't measured it, this should also reduce
      memory requirements for compilation.

    - The testsuite now uses the library explicitly, and thus no
      longer needs to re-compile half the library.

* Bug-fixes:

    - Double values are now longer parseable without double quotes if
      they have an exponential term (to better match the definition).

    - It is no longer assumed when round-tripping that `dot -Tdot`
      generates canonicalised Dot graphs.

* The `TestParsing` script is now directly buildable by Cabal using
  the `test-parsing` flag (the resulting executable is called
  `graphviz-testparsing`).  This is not made an actual test-suite as
  not all files found will be actual Dot graphs, and it's known that
  it fails on some.  Instead it's meant to be used as an indication of
  how well this library parses "real-world" Dot code.

Changes in 2999.16.0.0
----------------------

* Add support for Graphviz-2.30.0:

    - New attributes:

        + `Area`
        + `Head_LP`
        + `LayerListSep`
        + `LayerSelect`
        + `Tail_LP`
        + `XLP`

    - `BgColor`, `Color` and `FillColor` now take a list of colors
      with optional weightings.

    - Layer handling now supports layer sub-ranges.

    - Added the voronoi-based option to `Overlap`.

    - Added the `Striped` and `Wedged` styles.

* Updates to attributes and their values:

    - The following attributes have had their values changed to better
      reflect what they represent:

        + `FontPath` takes a `Path` value.

        + `Layout` takes a `GraphvizCommand` (and thus
          `GraphvizCommand` has been moved to
          `Data.GraphViz.Attributes.Complete`).

    - Added `MDS` to `Model` (which had somehow been overlooked).

    - Various attributes now have defaults added/updated/removed if
      wrong.

    - Removed the following deprecated attributes:

        + `ShapeFile`

        + `Z`

* Now any value that has a defined default can be parsed when the Dot
  code just has `attribute=""` (which `dot -Tcanon` is fond of doing
  to "reset" attributes).

* `GraphvizCommand` now defines `Sfdp` (which had somehow been
  overlooked up till now).

* The `canonicalise` and related functions have been re-written;
  whilst there might be some cases where their behaviour does not
  fully match how `dot -Tcanon` and `tred` behave (due to the
  interaction of various attributes), the new implementation is much
  more sane.

* Use temporary files rather than pipes when running dot, etc.

    Makes it more portable, and also avoids issues where dot, etc. use
    100% CPU when a graph is passed in via stdin.

    Original patch by **Henning Thielemann**.

* `renderCompactDot` now outputs to just one line (which means it's
  shorter, but more difficult to read for a human).

* The QuickCheck instances are a bit smarter now to avoid having such
  ugly huge outputs when a test fails.

* More documentation to help you understand what certain data types
  are for, etc.

Changes in 2999.15.0.1
----------------------

* Fixed bug where `canonicalise` and related functions did not
  properly deal with attributes of a single node or edge within a
  sub-graph.

Changes in 2999.15.0.0
----------------------

* The repository is now hosted on
  [hub.darcs.net](http://hub.darcs.net/ivanm/graphviz), and using the
  bug-tracker there.

* Updates to various `Attribute` definitions:

    - The list of available shapes has been expanded to take into
      account the new synthetic biology shapes.

    - The `Size` and `FontNames` attributes now have specified data types.

    - The optional start and end points for `Spline`s were previously
      the wrong way round; this has now been fixed.

    - Explicitly only print 2-dimensional `Point` values for `Rect`
      and `DPoint` (previously only 2-dimensional values where parsed,
      but it was possible to print a 3-dimensional value).

    - HTML-like labels previously disallowed empty textual label
      components when parsing; spotted by **Daniel Hummel**.

* `GraphvizParams` now lets you specify whether a "cluster" is
  actually a cluster or a sub-graph.  Requested by **Gabor Greif**.

* Fixed an error where printing edges whose attributes contained a
  `ColorScheme` attribute, that attribute would stay in the state and
  the old color scheme would override the _parent's_ state.

* Previously, some malformed attributes were accepted when being
  parsed as they silently became parsed as an `UnknownAttribute`.
  Now, attributes where the attribute name and the equal sign are
  successfully parsed **but** the value of the attribute is _not_
  successfully parse will throw an unrecoverable error.

    **Note**: this does mean that some Dot graphs that are accepted by
    Graphviz (as they separate tokenizing from parsing; as such
    something like `"0.1"` is successfully accepted as an integer,
    specifically `0`) are no longer accepted when parsing them in.

* Miscellaneous parsing improvements:

    - Whilst not specified anywhere, Graphviz seems to treat empty
      quotes as values for attributes expecting a number as their
      default; as such, this is now taken into account when parsing.

    - `DPoint` values can now parse an optional `+` prefix.

    - Whitespaces after commas in HSV colors are now accepted.

* Error messages from parsing are improved to help you track down
  where the parsing error occurred (in that it's easier to find which
  attribute failed to parse, etc.).

* Some parsing combinators are no longer publicly accessible, but
  shouldn't be needed by end users anyway (as they were there for
  parsing the Attribute data type).

Changes in 2999.14.1.0
----------------------

* Add `isGraphvizInstalled` and `quitWithoutGraphviz` for programs to
  test whether Graphviz has been installed (as previous attempts at
  doing so via actual calls to dot, neato, etc. weren't working and
  would be too verbose anyway).

Changes in 2999.14.0.0
----------------------

* Added an instance of `Labellable` for strict `Text` values, as
  requested by **Erik Rantapaa**.

Changes in 2999.13.0.3
----------------------

* Allow usage of transformers-0.3.*.  Spotted by **Peter Simons**.

Changes in 2999.13.0.2
----------------------

* The `String` instance for `ToGraphID` requires the
  `TypeSynonymInstances` pragma (at least on GHC < 7.4).  Spotted by
  **Gregory Guthrie**.

Changes in 2999.13.0.1
----------------------

* Fixed Haddock typos.

Changes in 2999.13.0.0
----------------------

* Added support for the `osage` and `patchwork` visualisation tools,
  available as of Graphviz-2.28.0.

* Updated attributes as of Graphviz-2.28.0:

    - `SVG` colors are now supported, and the support for different
      colors has been revamped.

    - `overlap=false` is now equivalent to `overlap=prism` and the
      `RemoveOverlaps` option has been removed.

    - `LabelScheme` and `Rotation` are new attributes for use with
      `sfdp`.

    - `Scale` is a new attribute for use with `twopi`.

    - Add new italics, bold, underline, superscript and subscript
      options for HTML-like labels.

    - `LHeight` and `LWidth` for getting the height and width of the
      root graph and clusters.

* Updated attributes from the current development branch of Graphviz
  (i.e. 2.29.*).  Please note that these will probably not work yet,
  but are implemented now for future-proofing.

    - A new style for edges: `Tapered`.

    - `XLabel` allows you to specify labels external to a node or
      edge.  `ForceLabels` allow you to specify that these should be
      drawn even when they will cause overlaps.

    - `ImagePath` allows you to specify where to search for images.

    - HTML-like labels now support `ID` values as well as horizontal
      and vertical rules.

    - `BgColor` and `FillColor` now take a list of colors: this allows
      gradient fills for graphs, clusters and nodes.  The `Radial`
      style and `GradientAngle` are also used for this purpose.

    - `FillColor` is now used by edges to set the color of any arrows.

    - [WebP](http://en.wikipedia.org/wiki/WebP) output support added.

* Other attribute changes:

    - Use a specified data type for the `Ordering` attribute rather
      than an arbitrary `Text` value, and provide a documented wrapper
      in `Data.GraphViz.Attributes`.

    - `Bb` has been renamed `BoundingBox`.

    - `ID` now only takes `EscString` (a type alias for `Text`) values
      rather than arbitrary `Label`s.

    - The `Data.GraphViz.Attributes.HTML` module has had all values
      re-named and is now meant to be imported qualified.  It is also
      no longer re-exported from `Data.GraphViz.Attributes.Complete`.

* The `ToGraphID` class provides a common wrapper to help create
  cluster identifiers, etc.

* Cabal's `Test-Suite` functionality is now used.  As part of this,
  the `Data.GraphViz.Testing` module and sub-modules are no longer
  exported by the library.

* The new `Benchmark` support in Cabal-1.14 is now used for the
  benchmark script.

* Dropped support for base-3.

* The `Data.GraphViz.State` module is no longer exposed, as there's no
  need for users to use it.

* Bugfixes:

    - Some corner cases in canonicalisation prevented it from being
      idempotent.

    - The `TestParsing` script will no longer crash and refuse to
      continue if an IO-based error (e.g. unable to successfully call
      `dot`) occurs.

    - A typo was spotted by **Gabor Greif**.

Changes in 2999.12.0.4
----------------------

* Parsing error messages have been cleared up, especially when parsing
  Dot graphs.  This came about from trying to help **Uri J. Braun**
  with an error in his code (which took a while to diagnose as a
  problem with the node type).

* Made more clear to people looking in `Data.GraphViz` who want to
  create Dot graphs by hand to look in `Data.GraphViz.Types` (came
  about when **Rustom Mody** stated he got confused trying to work out
  how to do this).

* Fixed up augmentation; bug and fix spotted by **Max Rabkin**.

* Fix up the TestParsing script to actually use the new type setup.
  It can also now take a single directory as an argument, and will try
  to parse all (non-recursive) files in that directory.

Changes in 2999.12.0.3
----------------------

* Fixes various mistakes in the Haddock documentation that slipped
  through (either in the `String -> Text` conversion or adding new
  modules and not checking their documentation thoroughly enough).

Changes in 2999.12.0.2
----------------------

* Forgot to explicitly list the module for Arbitrary instance for the
  graph representation.

Changes in 2999.12.0.1
----------------------

* Fix a bug that prevented graphviz from being built with GHC 7.2.

Changes in 2999.12.0.0
----------------------

Many large-level changes were done in this release; in rough
categories these are:

### Conversions from other types

* Can now more easily create Dot graphs from other graph-like data
  structures with the new `graphElemsToDot` function, which takes a
  list of nodes and edges.

* It is now no longer possible to use `graphToDot`, etc. to create Dot
  graphs with anonymous clusters; all clusters must now have explicit
  names (note that uniqueness is not enforced, and it is still
  possible to directly create Dot graphs with anonymous clusters).

### Dot graph representations

* The canonical graph representation has been moved to its own module:
  `Data.GraphViz.Types.Canonical`.

* The generalised representation has had all its "G" and "g" prefixes
  removed.

* Two new representations:

    - `Data.GraphViz.Types.Graph` allows graph-like manipulation of
      Dot code.

    - `Data.GraphViz.Types.Monadic` provides a monadic interface into
      building relatively static graphs, based upon the
      [_dotgen_](http://hackage.haskell.org/package/dotgen) library by
      Andy Gill.

* The `DotRepr` class has been expanded, and three pseudo-classes have
  been provided to reduce type-class contexts in type signatures.

### Using and manipulation Dot graphs

* Pure Haskell implementations of `dot -Tcanon` and `tred` are
  available in `Data.GraphViz.Algorithms`.

* A new module is available for more direct low-level I/O with Dot
  graphs, including the ability to run custom commands as opposed to
  being limited to the standard dot, neato, etc.

### Attributes

* `Data.GraphViz.Attributes` now contains a slimmed-down recommended
  list of attributes; the complete list can still be found in
  `Data.GraphViz.Attributes.Complete`.

* The `charset` attribute is no longer available.

* Functions for specifying custom attributes (for pre-processors,
  etc.) are available.

### Implementation

* Now uses [`Text`](http://hackage.haskell.org/package/text) values
  rather than `String`s.  Whilst performing this migration, the
  improvements in speed for both printing and parsing improved
  dramatically.

    - As part of this, human-readable Dot code is now produced by
      default.  As such, the `prettyPrint` and `prettyPrint'`
      functions have been removed.

* Now uses state-based printing and parsing, so that things like graph
  directedness, layer separators and color schemes can be taken into
  account.

* Parsing large data-types (e.g. `Attributes`) now uses less
  back-tracking.

* Now has a benchmarking script for testing printing and parsing
  speed.

* Uses a custom exception type rather than a mish-mash of error,
  `Maybe`, `Either`, exception types from used libraries, etc.

* Usage of UTF-8 is now enforced when doing I/O.  If another encoding
  is required, the `Text` value that's printed/parsed has to be
  written/read from disk/network/etc. manually.

### Bug-Fixes

* The `Rects` `Attribute` should be able to take a list of `Rect`
  values; error spotted by **Henning Gunther**.

* In some cases, global attribute names were being printed without
  even an empty list (which doesn't match what dot, etc. expect).

Changes in 2999.11.0.0
----------------------

* Addition of the `Labellable` class (and its method `toLabel`) to
  make it easier to construct labels.

* Backslashes (i.e. the `\` character) are now escaped/unescaped
  properly (bug spotted by **Han Joosten**).  As part of this:

    - Dot-specific escapes such as `\N` are now also handled
      correctly, so the slash does not need to be escaped.

    - Newline (`'\n'`) characters in labels, etc. are escaped to
      centred-newlines in Dot code, but not unescaped.

* `Point` values can now have the optional third dimension and end in
  a `!` to indicate that that position should be used (as input to
  Graphviz).

* `LayerList` uses `LayerID` values, and now has a proper `shrink`
  implementation in the test suite.

Changes in 2999.10.0.1
----------------------

* Fixed a mistake in one of the source files that was made just to
  make
  [haskell-src-exts](http://hackage.haskell.org/package/haskell-src-exts)
  happier.

* Fix the `Arbitrary` instance for `Point` in the testsuite (since
  there's only one constructor now).

Changes in 2999.10.0.0
----------------------

* Conversion of `FGL`-style graphs to `DotRepr` values is now achieved
  using the new `GraphvizParams` configuration type.  This allows us
  to define a single parameter that stores all the conversion
  functions to pass around rather than having to pass around several
  functions.  This also allows all the non-clustered and clustered
  functions to be collapsed together, whereas what used to be handled
  by the primed functions is now achieved by using the
  `setDirectedness` function.

    There are three default `GraphvizParams` available:

    - `defaultParams` provides some sensible defaults (no attributes
      or clustering).

    - `nonClusteredParams` is an alias of `defaultParams` where the
      clustering type is explicitly set to be `()` for cases where you
      don't want any clustering at all (whereas `defaultParams` allows
      you to set your own clustering functions).

    - `blankParams` sets all fields to be `undefined`; this is useful
      for situations where you have functions that will set some
      values for you and there is no sensible default you can use
      (mainly for the clustering function).

* Expansion of the `DotRepr` class:

    - More common functions are now defined as methods (`getID`,
      etc.).

    - The ability to get more information about the structure of the
      `DotRepr` graph, as well as where all the `DotNode`s are, etc.

    - `graphNodes` now returns `DotNode`s defined only as part of
      `DotEdge`s, and will also merge duplicate `DotNode`s together.

    - `graphNodes` and `graphEdges` also return `GlobalAttributes`
      that apply to them.

* The `Point` type now only has one constructor: `Point Double
  Double`.  The `Int`-only constructor was present due to historical
  purposes and I thought that the `Pos` value for a `DotNode` would
  always be a pair of `Int`s; this turns out not to be the case.

* `SortV` and `PrismOverlap` now only take `Word16` values rather than
  `Int`s, as they're not meant to allow negative values (the choice of
  using `Word16` rather than `Word` was arbitrary, and because it's
  unlikely those values will be large enough to require the larger
  values available in `Word`).

* `NodeCluster` has been generalised to not have to take an `LNode`
  for the node type; the type alias `LNodeCluster` is available if you
  still want this.

* Several documentation typos fixed, including one spotted by **Kevin
  Quick**.

* The test-suite now allows running individual tests.

Changes in 2999.9.0.0
---------------------

* graphviz now has an FAQ and an improved README as well as its own
  [homepage](http://projects.haskell.org/graphviz/).

* Canonicalisation of `DotRepr` values is now available with the
  `canonicalise` function.

* Add support for record labels; values are automatically
  escaped/unescaped.  The `Record` and `MRecord` shapes have been
  added for use with these labels.  **Requested by Minh Thu and Eric
  Kow.**

* Add support for HTML-like values (this replaces the wrong and
  completely broken URL datatype).  Strings are automatically
  escaped/unescaped.

* Named `PortPos` values are now accepted (as required for record and
  HTML-like labels).

* `GraphID` no longer allows HTML-like values (since Graphviz doesn't
  seem to allow it).

* `RankSep` takes a list of `Double` values as required.

* `Attribute` has a new constructor `UnknownAttribute` for use when
  parsing deprecated Graphviz attributes in old Dot code.

* Various parsing fixes; of special note:

    - Statements no longer need to end in a semi-colon;

    - Anonymous sub-graphs are now supported.

    - Edge statements can now handle node groupings (e.g. ` a -> {b
      c}`) as well as `portPos` values (e.g. `a:from -> b:to`).

    - Unquoted `String`s containing non-ASCII characters are now
      parsed properly (though they are assumed to be encoded with
      UTF-8).  **Thanks to Jules Bean (aka quicksilver) for working
      out how to do this.**

    More specifically: almost all Dot files that ship with Graphviz, as
    documentation in the Linux kernel, etc. are now parseable.

* A new script to assist in testing whether "real-world" Dot graphs
  are parseable.

* Slight performance increase when parsing: whereas parsing is done
  case-insensitively, the "correct" case is now checked by default
  which has a moderate affect on parsing times.

* Split lines are now able to be handled when parsing.

Changes in 2999.8.0.0
---------------------

* Added support for generalised `DotGraph`s; this optional
  representation removes the restriction of ordering of Dot
  statements.  This allows for greater flexibility of how to specify
  Dot code.  As an offshoot from this, most relevant functions now
  utilise a new `DotRepr` class that work with both `DotGraph`s and
  the new `GDotGraph`s; this shouldn't affect any code already in use.

* With the **prompting of Noam Lewis**, the augmentation functions
  have been revamped in two ways:

  - Now contains support for multiple edges.

  - The ability to perform "manual" augmentation if greater control is
    desired.

* Add a preview function to quickly render and visualise an `FGL`
  graph using the `Xlib` canvas.

* Added a pseudo-inverse of the `FGL -> Dot` functions (a graph is
  created, but the original node and edge labels are unrecoverable).

* The `Printing` and `Parsing` modules have been moved (from
  `Data.GraphViz.Types` to `Data.GraphViz`).

* Reworked file-generating commands such that they return the filename
  of the created file if successful.

Changes in 2999.7.0.0
---------------------

* Updated and extended functions to interact with the Graphviz tools.
  This now includes:

  - a better listing of available output types;

  - distinguishing file outputs from canvas outputs;

  - ability to automatically add the correct file extension to file
    outputs

  - Return any errors if calling Graphviz failed rather than just
    printing them to stderr

* Improved `Color` support:

  - fixed `ColorScheme` values

  - explicitly named `X11` colors

  - conversion to/from values from the [colour] library

  [colour]: http://www.haskell.org/haskellwiki/Colour

* Removed `OrientationGraph`; problems with distinguishing it when
  parsing from node-based `Orientation` values; its functionality is
  duplicated by `Rotate`.

* By default, the generated Dot code is now no longer indented; you
  can now use the `prettyPrint` functions in `Data.GraphViz` to
  produce readable Dot code.

* Added a testsuite; this is buildable by building with
  `--flags-test`.  Numerous printing and parsing bugs were picked up
  with this.

Changes in 2999.6.0.0
---------------------

* Remove some `Shape` aliases and change capitalisation of others.

* Properly parse and print IDs of clusters.

* Allow `NodeCluster` values to have node types different from the
  `LNode` they come from.

Changes in 2999.5.1.1
---------------------

* When used as labels, etc., the Dot keywords `node`, `edge`, `graph`,
  `digraph`, `subgraph`, and `strict` need to be quoted.  **Spotted by
  Kathleen Fisher.**

Changes in 2999.5.1.0
---------------------

* Potentially fixed the `graphvizWithHandle` bug; correct approach
  **spotted by Nikolas Mayr**.

* Fixed up `Parsing` of various `Attribute` sub-values, especially
  `Point` and `Spline` (and hence `Pos`, which uses them).

* Pre-process out comments and join together multi-line strings before
  parsing.

* Properly parse `Doubles` like `".2"`.

Changes in 2999.5.0.0
---------------------

A major re-write occured; these are the highlights:

* Re-write parsing and printing of Dot code using the new `ParseDot`
  and `PrintDot` classes.  This should finally fix all quoting issues,
  as well as leaving `Show` as the code representation for hacking
  purposes.  As part of this, the `Data.GraphViz.ParserCombinators`
  module has been moved to `Data.GraphViz.Types.Parsing`.

* Re-write the various `Dot*` datatypes in `Data.GraphViz.Types`.
  Sub-graphs/clusters are now their own entity rather than being part
  of `DotNode` and the Node ID type is now a type parameter rather
  than being just `Int`.  Sub-graphs/clusters can now also be parsed.

* The various conversion functions in `Data.GraphViz` now come in two
  flavours: the unprimed versions take in a `Bool` indicating if the
  graph is directed or not; the primed versions attempt to
  automatically detect this.

* Add cluster support for the graph augmentation functions, **as
  requested by Nikolas Mayr**.

* Allow custom arrow types as supported by GraphViz; **as requested by
  Han Joosten**.

* Fixed a bug in HSV-style `Color` values where `Int` was used instead of
  `Double`; **spotted by Michael deLorimier**.

* Properly resolved the situation initially spotted by Neil Brown:
  Matthew Sackman was following Dot terminology for an edge `a -> b`
  when using _head_ for `b` and _tail_ for `a` (this is presumably
  because the head/tail of the arrow are at those nodes).  `DotEdge`
  now uses _from_ and _to_ avoid ambiguity; the various `Attribute`
  values still follow upstream usage.

Changes in 2999.1.0.2
---------------------

* Fix a bug **spotted by Srihari Ramanathan** where `Color` values
  were double-quoted.

Changes in 2999.1.0.1
---------------------

* The `Color` `Attribute` should take `[Color]`, not just a single
  `Color`.

Changes in 2999.1.0.0
---------------------

* Stop using `Either` for composite `Attributes` and use a custom
  type: this avoids problems with the `Show` instance.

Changes in 2999.0.0.0
---------------------

* Fixed a bug where the Show instance and read function for `DotEdge`
  had the from/to nodes the wrong way round.  This was not immediately
  noticed since the `Graph` to `DotGraph` functions created them the
  wrong way round, so for those users who only used these this was not
  apparent.  **Spotted by Neil Brown.**

* Greatly improved `Attribute` usage: almost all attributes are now
  covered with allowed values.

* Extend DotGraph to include whether a graph is strict or not and if
  it has an ID.  Also move the directedGraph field.

* Make `Dot` refer to the actual dot command and `DotArrow` refer to
  the `ArrowType` (rather than `DotCmd` and `Dot` as before).

* Make the `Data.GraphViz.ParserCombinators` module available to end
  users again, but not re-exported by `Data.GraphViz`; it has a
  warning message up the top not to be used.  It is there purely for
  documentative purposes.

* Use [extensible-exceptions] so that `base-3.x` is once again
  supported.

  [extensible-exceptions]: http://hackage.haskell.org/package/extensible-exceptions

* Follow the [Package Versioning Policy] rather than using dates for
  versions.

  [Package Versioning Policy]: http://www.haskell.org/haskellwiki/Package_versioning_policy


Changes in 2009.5.1
-------------------

* New maintainer: Ivan Lazar Miljenovic.

* Support `polyparse >= 1.1` (as opposed to `< 1.3`)

* Require `base == 4.*` (i.e. `GHC 6.10.*`) due to new exception handling.

* Include functions from [Graphalyze-0.5] for running GraphViz
  commands, etc.

  [Graphalyze-0.5]: http://hackage.haskell.org/package/Graphalyze-0.5

* Module re-organisation.

* The `Data.GraphViz.ParserCombinators` module is no longer available
  to end users.

* Improved Haddock documentation.

Changes in 2008.9.20
--------------------

* Differentiate between undirected and directed graphs (previously
  only directed graphs were supported).

* Clustering support was added.

Older versions
--------------

For versions of graphviz older than `2008.9.20`, the exact differences
between versions is unknown.
