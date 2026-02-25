
Fortuitously Anticipated Queries (FAQ)
======================================

Note that to distinguish it from [Graphviz], the library shall be
henceforth referred to as _graphviz_.

Graphviz vs _graphviz_
----------------------

### What is the difference between Graphviz and _graphviz_? ###

[Graphviz] is an open source library and collection of utility
programs using that library to visualise [graphs] (which are specified
using the [Dot] language).

_graphviz_ is a library for the purely functional programming language
[Haskell] that provides "bindings" to Graphviz's programs.  It does so
by allowing programmers to specify the layout of the graph and then
converts that to Dot code before calling the appropriate program to
perform the visualisation.

[Graphviz]: http://www.graphviz.org/
[graphs]: http://en.wikipedia.org/wiki/Graph_theory
[Dot]: http://www.graphviz.org/doc/info/lang.html
[Haskell]: http://haskell.org/

### Why should I use graphviz over one of the other Haskell Graphviz libraries? ###

Various Haskell libraries have support for Graphviz to one extent or
another; however _graphviz_ has the most comprehensive support
available out of all of them:

* There are [four different representations] of Dot graphs:

       1. Canonical, which provides a clean separated definition of a
          Dot graph (that matches the former layout of `dot -Tcanon`).
       2. Generalised, which allows statements to be in any order.
       3. A graph-based one that allows manipulation of the Dot graph.
       4. A monadic interface for embedding relatively static graphs
          in Haskell.

    There are also conversion functions between them.

 [four different representations]: #whats-the-difference-between-the-different-dotgraph-types

* The ability to parse and generate most aspects of Dot [syntax] and
  [attributes].  This includes taking into account escaping and
  quoting rules where applicable.

  [syntax]: http://graphviz.org/doc/info/lang.html
  [attributes]: http://graphviz.org/doc/info/attrs.html

* The ability to use a custom node type for Dot graphs.

* Support for all stated layout algorithm programs and all specified
  [output formats] as well as the ability to use custom programs, etc.

  [output formats]: http://www.graphviz.org/doc/info/output.html

* Functions to convert [FGL] graphs and other graph-like structures
  (albeit not as nicely) to and from the internal Dot representations.
  In future, this will be expanded to a much larger range of
  graph-like values once a suitable abstraction is available.

  [FGL]: http://web.engr.oregonstate.edu/~erwig/fgl/haskell/

* The ability to augment Dot and FGL graphs with positioning
  information by round-trip passing through Graphviz.

* Pure Haskell implementations of `dot -Tcanon` and `tred`.

* _graphviz_ is continually being worked upon and expanded to better
  suit/match the requirements of Graphviz, to improve performance and
  to make it easier for the programmer to use.

### Is the API of _graphviz_ stable? ###

For the most part, yes: the only items that are likely to change in
the future are those with bugs/errors or if a radically better way of
doing things is found.  For most uses, however, the API should not
change for the foreseeable future.

Note that _graphviz_'s version numbers follow the
[package versioning policy]; this means that you can immediately tell
when the API has had a backwards-incompatible change by comparing the
first two elements of the version.  However, these changes won't
always affect most users.

[package versioning policy]: http://www.haskell.org/haskellwiki/Package_versioning_policy

### What aspects of Dot syntax and attributes are covered? ###

It's easier to state which aspects of Dot [syntax] and [attributes]
_aren't_ covered:

#### Overall syntax items not covered ####

* Cannot specify a sub-graph as an end point in an edge;

* Comments, pre-processor lines and split lines are (currently) not
  supported within HTML-like labels.

* _graphviz_ only uses UTF-8 encoding for printing and parsing
  (whereas Graphviz allows Latin1 encoding with the `charset`
  attribute).

* Graphviz is more liberal in accepting "invalid" values
  (e.g. accepting a floating-point value when only integer values are
  meant to be accepted); _graphviz_ is more strict in this aspect (and
  will indeed throw an exception if it cannot parse something
  properly).

* No extensions (e.g. postscript-specific attributes) are available.

#### Attribute and value items not covered ####

* The global `orientation` attribute is not defined; however its
  behaviour is duplicated by the `rotate` attribute.

* The deprecated `overlap` algorithms have not been defined, and the
  ability to specify an integer prefix for use with the `fdp` layout
  tool is not available.

* The deprecated `shapefile` attribute is not available; instead, you
  should specify the file on the command line.

* The deprecated `z` attribute is not available; use the optional
  third dimension for the `pos` attribute instead.

* Only polygon-based `shape`s are available (i.e. no custom shapes as
  yet).

* The `charset` attribute is not available as _graphviz_ assumes that
  all Dot graphs will be in UTF-8 for simplicity; if Latin1-encoded
  graphs need to be parsed then you shall need to do all I/O for them
  by hand.

* `colorscheme` attributes _are_ parsed, but the behaviour is not
  quite the same: consider the following minimal Dot graph:

    ~~~~~ {.dot}
    digraph {
        a [ style=filled, fillcolor=gray, colorscheme=svg ]
    }
    ~~~~~

    Despite the fact that the color is specified before the colorscheme,
    Graphviz will use that colorscheme to parse the color (as an SVG
    gray differs from the X11 gray); _graphviz_, however, will use the
    default colorscheme of `x11` to parse the color, and **then** set
    the colorscheme to be `svg` (despite it not being used after it is
    set).

#### Available items of note ####

There are a few items of note that are available that are worthy of
special note (as they may not be immediately obvious from the
generated documentation):

* _graphviz_ is able to parse (but not print) the following special
  aspects of specifying edges in Dot code:

    - The `node:port` method of specifying of head/tail `portPos`
      values.

    - Stating multiple edges with common interior nodes (e.g. `a -> b
      -> c`).

    - Stating edges with a grouping of nodes (e.g. `a -> {b c}`).

* Sub-graphs are specified as being clusters when the subgraph name
  starts with either `"cluster"` or `"cluster_"`; note that this
  prefix is removed when determining the subraph's name for the
  internal datatypes.

* Anonymous subgraphs (where not even the `subgraph` keyword is
  specified) are also parseable.

* HTML-like and record labels are available, and feature proper
  escaping/unescaping when printing/parsing.

* Other syntactic issues are taken care of for you automatically (such
  as escaping/unescaping quotation marks).  Even newlines are
  automatically escaped (but not unescaped) for you, defaulting to
  centered lines.

Getting _graphviz_ and more documentation
-----------------------------------------

### Where can I obtain _graphviz_? ###

The best place to get _graphviz_ is from its [HackageDB] page.

[HackageDB]: http://hackage.haskell.org/package/graphviz

### Where can I find the API documentation for _graphviz_? ###

Also on its [HackageDB] page.

### Is it safe to install and use _graphviz_ from its git repository? ###

No; unlike other projects I make no guarantees as to the stability of
the live version of _graphviz_.  Whilst the [git] [repository] is
_usually_ stable, it's often in a state of flux and at times patches
that break the repository are recorded (when it's simpler/cleaner to
break one patch into several smaller patches).

[git]: http://git-scm.com/
[repository]: https://github.com/ivan-m/graphviz/

### How is _graphviz_ licensed? ###

_graphviz_ is licensed under a [3-Clause BSD License] (note that the
ColorBrewer Color Schemes found in `Data.GraphViz.Attributes.Colors.Brewer`
are covered under
[their own license](http://graphviz.org/doc/info/colors.html#brewer_license)).

[3-Clause BSD License]: http://www.opensource.org/licenses/bsd-license.php

Simplistically, this means that you can do whatever you want with
_graphviz_ as long as you cite both myself and [Matthew Sackman] (the
original author) as being the authors of _graphviz_.  However, I would
appreciate at least an [email] letting me know how _graphviz_ is being
used.

[Matthew Sackman]: http://www.wellquite.org/
[email]: mailto:Ivan.Miljenovic@gmail.com

### Where can I find more information on _graphviz_? ###

From its [home page].

[home page]: http://projects.haskell.org/graphviz/

### Are there any tutorials on how to use _graphviz_? ###

A basic tutorial on
[how to visualise graph-like data](http://ivanmiljenovic.wordpress.com/2011/10/16/graphviz-in-vacuum/)
is available; more will come if people ask for it.

### What other packages use _graphviz_? ###

This is a list of all known packages that use _graphviz_: if you know
of any others please let me know and I'll add it to the list.

* [Graphalyze](http://hackage.haskell.org/package/Graphalyze)
* [SourceGraph](http://hackage.haskell.org/package/SourceGraph)

### What is the history of _graphviz_? ###

_graphviz_ was originally written by [Matthew Sackman] (if you want
his reasons for doing so, you'll have to ask him yourself) with the
first known release being on 10 July, 2008.  In 2008 I (Ivan
Miljenovic) needed a library that provided bindings to Graphviz with
clustering support; at the time _graphviz_ was the most fully featured
and closest to what I wanted, so I submitted a patch that provided
support for both clustering and undirected graphs.

In April 2009, Matthew wanted to step down from maintaining _graphviz_
and asked if I wanted to take over.  Since then the library has been
almost completely re-written with greatly improved coverage of the Dot
language and extra features.  However, the original outline of the
library still remains.

Using _graphviz_
----------------

### Can I start using _graphviz_ without knowing anything about Graphviz? ###

You can, but if you want to start doing anything more advanced then
you should be reading Graphviz's documentation as well as
_graphviz_'s.  This is because the layout and design of _graphviz_ is
heavily based upon the Dot language and the various [attributes] that
Graphviz supports.

### Can I just use _graphviz_ without reading its documentation? ###

You should _at least_ read the various messages about possible
ambiguities, etc. at the top of each module and for the attributes you
use before you use _graphviz_.

### Do I need to have Graphviz installed to use _graphviz_? ###

Technically, no if you're only dealing with the Dot language aspects.
However, usage of the functions in the `Commands` module, or the
augmentation of pretty-printing functions in the GraphViz module _do_
require Graphviz to be installed.

### Why didn't you use FFI to bind to the Graphviz library? ###

Because I just kept working where [Matthew Sackman] left off and it
was already using Graphviz's tools rather than the actual library.
However, most other language bindings (for Python, Perl, etc.) seem to
do the same: generate Dot code and pass that to the relevant tool.

This, however, does provide a fortunate side effect where the ability
to print and parse Dot code means that _graphviz_ can be used for more
than just visualising graphs created solely in Haskell: it can also
import pre-defined graphs, or else generate Dot code for use with
other tools.

### What's the difference between the different DotGraph types? ###

_graphviz_ has four different "implementations" of Dot code:

**Canonical:**

:   matches the (former) output of `dot -Tcanon`.  Recommended for use
    when converting existing data into Dot (especially with the
    `graphElemsToDot` function in `Data.GraphViz`).

**Generalised:**

:   most closely matches the layout of actual Dot code, as such this is
    preferred when parsing in arbitrary Dot graphs.  Also useful in
    cases where you want to use the common Graphviz "hack" of specifying
    global attributes that don't apply to sub-graphs _after_ the
    sub-graphs in question.

**Graph:**

:   provides common graph operations on Dot graphs, based upon those
    found in the [FGL].

**Monadic:**

:   a nicer way of defining relatively static Dot graphs to embed within
    Haskell code, etc.  Loosely based (with permission!) upon Andy
    Gill's [dotgen] library.

[dotgen]: http://hackage.haskell.org/package/dotgen

### What's the best way to parse Dot code? ###

Use the `parseDotGraph` function (rather than the general parsing
functions that are available) to parse your Dot code: this is will
strip out comments and pre-processor lines and join together split
lines (if any of these remain the parser will fail).  Also, if you are
not sure what the type of the nodes are, use either String or else the
`GraphID` type as it explicitly caters for both Strings and numbers
(whereas just assuming it being a String will result in numbers being
stored internally as a String).

Unless you are very sure of the representation of the Dot code you
have been provided, you should parse in any Dot code as the
`Generalised.DotGraph` type.  Afterwards you can use
`FromGeneralisedDot` to convert to whichever representation you
prefer.

### There are too many attributes!!! Which ones should I use? ###

The `Data.GraphViz.Attributes` module contains a cut-down list of
recommended and commonly used attributes.

The entire list of attributes can be found in
`Data.GraphViz.Attributes.Complete`.  In particular, the following
attributes are **not** recommended for use:

* `Color` for anything except edge colours or gradients for nodes,
  clusters and graphs when using Graphviz >= 2.29.0 (and if you must,
  the border colour for a node).

* `ColorScheme`: just stick with X11 colours.

* `Comment`: pretty useless.  Enough said.

### Can I use any attribute wherever I want? ###

No: attributes are all defined in one big datatype for the sake of
simplicity, but not all attributes are valid in all places.  Read the
documentation (either for Graphviz or _graphviz_) to determine which is
suitable where.

### How can I use _graphviz_ to visualise non-FGL graphs? ###

The `graphElemsToDot` function allows you to visualise any graph for
which you can specify a list of labelled nodes and a list of labelled
edges.

### How can I use/process multiple graphs like Graphviz does? ###

At one stage, _graphviz_ supported dealing with lists of `DotGraph`s;
however, it was found to be faster to deal with each graph
individually rather than try to get Graphviz to deal with them all in
one go.  In future, once the problem causing this has been tracked
down and fixed this feature will be returned.

### How can I use custom datatypes for node IDs? ###

The important thing here is to ensure that your custom datatype has
defined instances of `PrintDot` and `ParseDot`.  Probably the easiest
way of doing this is to have functions that convert between your type
and `String` or `Text` and let graphviz determine how to print and
parse those.  Here is an example of a more difficult type that should
be printed like `"1: Foo"`:

~~~~~~~~~~~~~~~~~~~~ {.haskell}
data MyType = MyType Int String

instance PrintDot MyType where
  unqtDot (MyType i s) = unqtDot i <> colon <+> unqtDot s

  -- We have a space in there, so we need quotes.
  toDot = doubleQuotes . unqtDot

instance ParseDot MyType where
  parseUnqt = MyType <$> parseUnqt
                     <*  character ':'
                     <*  whitespace1
                     <*> parseUnqt

  -- Has at least one space, so it will be quoted.
  parse = quotedParse parseUnqt
~~~~~~~~~~~~~~~~~~~~

Things to note from this example:

* Whilst `PrintDot` and `ParseDot` have default definitions for
  `toDot` and `parse`, they assume the datatype doesn't need quotes;
  as such if the value will
  [need quoting](http://www.graphviz.org/doc/info/lang.html), then you
  should do so explicitly.

* It is better to use the `PrintDot` instances for common types such
  as `Int` and `String` rather than using the pretty-printers inbuilt
  conversion functions (`int`, `text`, etc.) to ensure that
  quotations, etc. are dealt with correctly.

* Be as liberal as you can when parsing, especially with whitespace:
  when printing only one space is used, yet when parsing we use the
  `whitespace1` parsing combinator that will parse all whitespace
  characters (but it must consume _at least_ one; there is a variant
  that does not need to parse any).

### When parsing Dot code, do I have to worry about the case? ###

Not at all: _graphviz_'s parser is case-insensitive; however, the
correct case is checked first so there is a slight degradation in
performance when the wrong case is used.

### How do I set portPos values for nodes in edges? ###

Graphviz allows you to specify edges such as `from:a -> to:b` where
the nodes "from" and "to" are defined with either `RecordLabel` or
`Html.Label` labels and have different sections; the edge is then drawn
from the "a" section of the "from" node to the "b" section of the "to"
node.

Whilst _graphviz_ can parse this, you can't define this yourself;
instead, do it the manual way:

~~~~~~~~~~~~~~~~~~~~ {.haskell}
DotEdge "from" "to" True [ TailPort (LabelledPort (PN "a") Nothing)
                         , HeadPort (LabelledPort (PN "b") Nothing)
                         ]
~~~~~~~~~~~~~~~~~~~~

I realise that doing this manually isn't as convenient, but I am open
to suggestions on how this can be improved.

Note where `TailPort` and `HeadPort` are used; the next question
explains this.

### Is there anything else I should know? ###

A few other things of note that you should know about:

* For an edge `a -> b`, Graphviz terms "a" to be the _tail_ node and
  "b" to be the _head_ node.

* When creating `GraphID` values for the graphs and sub-graphs, you
  should ensure that they won't clash with any of the `nodeID` values
  when printed to avoid possible problems.

* It is a good idea to have unique IDs for sub-graphs to ensure that
  global attributes are applied only to items in that sub-graph and so
  that clusters aren't combined (it took me a _long_ time to find out
  that this was the case).

* You should specify an ID for the overall graph when outputting to a
  format such as SVG as it becomes the title of that image.

* Graphviz allows a node to be "defined" twice with different
  attributes; in practice they are combined into one node.  Running
  Dot code through `dot -Tcanon` before parsing removes this problem.

* Several attributes are defined with taking a list of items; all of
  these assume that the provided lists are non-empty (sub-values are a
  different story).

* If a particular Dot graph is not parseable, the parser throws an
  error rather than failing gracefully.

Design Decisions
----------------

### Why does _graphviz_ use Polyparse rather than Parsec? ###

Short answer: because _graphviz_ was already using [Polyparse] when I
started working on it (and I hadn't done any parsing before so I had
no preference either way).

[Polyparse]: http://www.cs.york.ac.uk/fp/polyparse/

Longer answer: Polyparse has several advantages I feel over [Parsec]:

* Simpler types.
* Avoids the whole "but Parsec-3 is slower than Parsec-2" debate (with
  its associated baggage/problems).
* Few inbuilt combinators: since there is no inbuilt `character`
  parsing combinator, there are no problems with _graphviz_ using its
  own case-less one.
* [Easier backtracking](http://www.cs.york.ac.uk/fp/polyparse/#how)

[Parsec]: http://hackage.haskell.org/package/parsec

### Why do you have four different representations of Dot graphs? ###

_graphviz_ has [four different representations] of Dot graphs.  Apart
from the reasons given before, the canonical implementation was the
original representation, whereas the generalised one was only
introduced in the 2999.8.0.0 release and the other two in the
2999.12.0.0 release.

Note, however, that I was thinking of adding something like the
generalised implementation back around the time of the
[2999.0.0.0 release](http://www.haskell.org/pipermail/haskell-cafe/2009-July/064436.html),
yet
[people didn't like the idea](http://www.haskell.org/pipermail/haskell-cafe/2009-July/064442.html).

The graph-based implementation was added solely so I could write an
(as-yet finished) tutorial, and thought others might find it useful.
The monadic implementation came about as an attempt to encourage more
people to use _graphviz_ rather than other libraries such as [dotgen],
and I thought a nicer way of writing Dot graphs might help (the
initial plans involved complicated type-hackery to try and almost make
it a DSL for actual Dot code; however it ended up being too
complicated and unwieldy).

### Why are only FGL graphs supported? ###

Love them or hate them, [FGL] currently provides the best graph
datatype and library available for Haskell at this time.  As such, if
any one graph type had to be chosen to have conversion functions
written for it then FGL is the best option.  Furthermore, I needed FGL
graph support (which is the much more important reason!).

### Why are the version numbers so high? ###

To make sure the latest release has the highest version number:
Matthew Sackman originally made releases with date-based versioning,
but when I switched to using the [package versioning policy] I had to
change this.  I could have started with 2010.x.y.z or so, but at the
time I had initial hopes of introducing compatibility with other
graphs (not just [FGL] ones) soon and wanted to make that the
3000.0.0.0 release; however that has not yet come to pass.

### Why do you use the American spelling of colour in _graphviz_? ###

Because that's how Graphviz spells it, and I was following upstream to
avoid confusion.

Bugs, Feature Requests and Development
--------------------------------------

### Do you have any future plans for _graphviz_? ###

Yes, I do!  See the TODO file for more information.

### Does _graphviz_ have a test suite? ###

Yes, there is, using the in-built support for test suites in Cabal:

~~~~~~~~~~~~~~~~~~~~ {.bash}
cabal install graphviz --enable-tests
~~~~~~~~~~~~~~~~~~~~

Then run the `graphviz-testsuite` executable.  This test suite uses
[QuickCheck] to ensure that _graphviz_ can parse the Dot code it
generates (as well as a few other things).  Note that it isn't
perfect: there are no guarantees that the Dot graphs that are
generated are indeed valid, and those more extensive tests are not yet
available.

[QuickCheck]: http://hackage.haskell.org/package/QuickCheck

Furthermore, you can do more controlled testing to try and track down
the source of a bug as the above flag will also expose several
testing modules which give you access to the various tests used as
well as the `Arbitrary` instances for use with [QuickCheck].

For proper testing of real-life Dot code, there is also the
`TestParsing.hs` script that comes in the _graphviz_ tarball (but is
not installed).  Once you have _graphviz_ installed you can just run
this script, passing it any files containing Dot graphs you wish to
test.  It will attempt to parse each Dot graph as a
`Generalised.DotGraph`, and then test to see if the canonicalised form
is parseable as a `DotGraph`.

### I've found a bug! ###

Oh-oh... please file a report at the GitHub [repository] to tell me
the specifics of what you were doing (including the Dot file in
question if it's a parsing problem) and I'll get right on it.

### I have a feature request. ###

Is it in the TODO?  If not, file an issue at the GitHub [repository]
and I'll consider implementing it (depending on time and how well I
think it will fit in the overall library).

### I want to help out with developing _graphviz_. ###

Great!  Whether you have a specific feature in mind or want to help
clear the TODO list, please create a pull-request on the GitHub
[repository].

### What is the purpose of the AttributeGenerator.hs file? ###

Graphviz has a large number of attributes.  Rather than try to edit
everything manually each time I want to change how I use the large
`Attribute` datatype, the AttributeGenerator script generates the
datatype, instances, etc. for me.
