# trace-dispatcher: efficient, simple and flexible program tracing

`trace-dispatcher` is a library that enables definition of __tracing systems__ -- systems that collect and manages traces -- evidence of program execution.

# Contents

0. [Contents](#Contents)
   - [ ] no-dead-links
1. [Document status](#Document-status)
2. [Introduction](#Introduction)
   1. [Motivation](#motivation)
      - [x] edit
      - [ ] agree
   2. [Design decisions](#Design-decisions)
      - [x] edit
      - [ ] agree
      - [ ] complete
   3. [Overview and terminology](#Overview-and-terminology)
      - [x] edit
      - [ ] agree
3. [Interface overview](#Interface-overview)
   1. [The trace / tracer duality](#The-trace--tracer-duality)
      - [x] edit
      - [ ] agree
      - [ ] complete
   2. [Emitting traces](#Emitting-traces)
      - [x] edit
      - [ ] agree
      - [ ] complete
   3. [Tracer namespace](#Tracer-namespace)
      - [x] edit
      - [ ] agree
      - [ ] complete
   4. [Trace annotation](#Trace-annotation)
      - [x] edit
      - [ ] agree
      - [ ] complete
   5. [Filter annotations](#Filter-annotations)
      - [x] edit
      - [ ] agree
      - [ ] complete
      1. [Severity](#Severity)
        - [x] edit
        - [ ] agree
        - [ ] complete
      2. [Privacy](#Privacy)
        - [x] edit
        - [ ] agree
        - [ ] complete
      3. [Frequency](#Frequency)
        - [x] edit
        - [ ] agree
        - [ ] complete
   6. [Presentation](#Presentation)
      1. [Formatting](#Formatting)
        - [x] edit
        - [ ] agree
        - [ ] complete
      2. [Detail level](#Detail-level)
        - [x] edit
        - [ ] agree
        - [ ] complete
      3. [Documentation](#Documentation)
        - [x] edit
        - [ ] agree
        - [ ] complete
   8. [Fold-based aggregation](#Fold-based-aggregation)
      - [x] edit
      - [ ] agree
      - [ ] complete
   9. [Configuration](#Configuration)
      - [x] edit
      - [ ] agree
      - [ ] complete
4. [Integration and implementation in the node](#Integration-and-implementation-in-the-node)
   1. [Overall tracing setup](#Overall-tracing-setup)
      - [x] edit
      - [ ] agree
      - [ ] complete
   2. [Trace-outs](#Trace-outs)
      - [x] edit
      - [ ] agree
      - [ ] complete
   3. [Severity filtering implementation](#Severity-filtering-implementation)
       - [x] edit
       - [ ] agree
       - [ ] complete
   4. [Confidentiality and privacy filtering implementation](#Confidentiality-and-privacy-filtering-implementation)
      - [x] edit
      - [ ] agree
      - [ ] complete
   5. [Note on SilenceF severity filter](#Note-on-SilenceF-severity-filter)
      - [x] edit
      - [ ] agree
      - [ ] complete
   5. [Documentation generation](#Documentation-generation)
      - [x] edit
      - [ ] agree
      - [ ] complete
   6. [Plumbing](#Plumbing)
      - [ ] discuss
      - [ ] edit
      - [ ] agree
      - [ ] complete
5. [Appendix](#Appendix)
   1. [Decisions](#Decisions)
   2. [Future work](#Future-work)

# Document status

Work in progress.

To do list:

* [x] Finish editing.
* [x] [Decide inline trace type annotation with trace function](#Decide-inline-trace-type-annotation-with-trace-function)
* [x] [Decide tracer definedness](#Decide-tracer-definedness)
* [x] [Decide tracer name definition](#Decide-tracer-name-definition)
* [x] [Decide inline trace type annotation with trace function 2](#Decide-inline-trace-type-annotation-with-trace-function-2)
* [x] [Decide on explicit trace filtering](#Decide-on-explicit-trace-filtering)
* [x] [Decide on privately combinator](#Decide-on-privately-combinator)
* [ ] [Decide on type error instead of silent dropping of messages](#Decide-on-type-error-instead-of-silent-dropping-of-messages)
* [ ] [Decide on more direct interface to EKG metrics](#Decide-on-more-direct-interface-to-ekg-metrics)
* [x] [Decide on dispatcher detail level control](#Decide-on-dispatcher-detail-level-control)
* [x] [Decide namespace-aware configuration](#Decide-namespace-aware-configuration)
* [x] [Decide missing configuration](#Decide-missing-configuration)
* [ ] [Discuss possibility of pure, thread-safe aggregation](#Discuss-possibility-of-pure-thread-safe-aggregation)
* [ ] [Decide trace-outs types](#Decide-trace-outs-types)
* [ ] Agree on editing.
* [ ] Final proofreading.
* [ ] Feedback.

# Introduction

## Motivation

`trace-dispatcher` is an attempt to enable design and implementation of simple, efficient __tracing systems__, one that has a reduced footprint in the executed program, has a more pleasant API, and provides self-documenting features.

## Design decisions

Key design decisions were:

1. Retaining the separation of concerns in the frontend side, as provided by the `contra-tracer` library.
2. Rely on __trace combinators__ primarily, as opposed to optinf for a typeclass heavy API.
3. Separation of data plane and control plane:  high-frequency events incur minimal processing on the data-plane, whereas complicated configuration logic only happens on the control plane, that is proportional to infrequent reconfiguration events.
4. A tougher stance on separation of concerns in the backend side:  we choose to move expensive trace processing outside of the system.
5. A measure of backward compatibility with the previous logging system.
6. Retaining the global namespace for all traces.

## Overview and terminology

The emitted __program traces__ (streams of __messages__ of arbitrary data types) are collected across all program components, and undergo __trace interpretation__ by the __dispatcher__ into __metrics__ and __log objects__, which are finally externalised.

Therefore, we can conceptually decompose the __tracing system__ into three components:

* __frontend__, the entry point for __program trace__ collection, which is just a single function (`traceWith` from the `contra-tracer` library);  Program locations that invoke this frontend (thereby injecting messages into the tracing system) is called __trace-ins__.
* __dispatcher__, is a structured, namespaced set of contravariantly-composed `IO` actions, triggered by the entry point, and that performs __trace interpretation__;
* __backend__, which is what externalises the __metrics__ and __log objects__ outside the system, through __trace-outs__.

The trace-emitting program itself is only exposed to the the frontend part of the tracing system, as it only needs to define the traces themselves, and specify the __trace-ins__ -- call sites that inject traces.  It is notably free from any extra obligations, such as the need to define the `LogFormatting` instances.

As mentioned above, __dispatcher__ is the point of interpretation of the program traces -- a structured set of __Tracer__ objects, that defines and implements the __language and policy__ of __trace interpretation__.

__Trace interpretation__ is specified in terms of:

* __trace synthesis__, which means production of __synthetic traces__ -- in cases where we decide it is cheaper (or at all possible) to perform trace aggregation inside the program,
* __trace naming__, which is assignment of hierarchically-structured names to all traces -- which serve identification, documentation and configuration purposes,
* __trace filtering__: which, in turn is based on __severity__, __privacy__ and __frequency__ of messages,
* __trace presentation__ : relying on __detail level__ and on the `LogFormatting` transformation of the traces into JSON, human readable and metric forms -- the last step before traces meet their __trace-outs__,
* __trace documentation__, as it is just another mode of the trace network execution.

The __trace interpretation__ process requires that for each trace the __dispatcher__ is provided with:

* instances of the `LogFormatting` typeclass, and
* __trace prototypes__ and __trace documentation__.

__Trace interpretation__ would have been unusably static, if it wasn't allowed to be configured without recompilation -- and therefore the __effective tracing policy__ used by the __dispatcher__ can be partially defined by the externally-supplied __trace configuration__.

The __effective tracing policy__ defines for each trace a __trace context__, which is what effectively informs interpretation performed by the __dispatcher__ for that particular trace.

The __trace context__, in turn, consists of the __annotation context__ encoded in the __dispatcher__, and the __configuration context__ coming from the __trace configuration__.

As a final note, the __dispatcher__ is not provided by the `trace-dispatcher` library as a ready-made, turn-key component -- instead, we are provided with __trace combinators__, the building blocks that allow its construction -- and therefore, expression of the desirable __trace interpretation policies__.

# Interface overview

## The trace / tracer duality

__Traces__ begin with a definition of their values, or __messages__.  As an example:

```haskell
data TraceAddBlockEvent blk =
    IgnoreBlockOlderThanK (RealPoint blk)
  | IgnoreBlockAlreadyInVolatileDB (RealPoint blk)
  ...
```

__Traces__ cannot be entered into the tracing system, unless they are accompanied by a matching __tracer__ -- a monadic callback, that expresses the action of tracing of values of that particular type:

```haskell
trAddBlock :: Tracer IO (TraceAddBlockEvent blk)
```

From the user perspective, __tracers__ can be approximated (WARNING: simplification!) as:

```haskell
data Tracer m a = Tracer (a -> m ())
```

## Emitting traces

To actually emit a trace, given a __trace value__ (or a __message__) and a corresponding tracer, the `traceWith` function needs to be used:

```haskell
traceWith :: Tracer m a -> a -> m ()
traceWith trAddBlock (IgnoreBlockOlderThanK p)
```

## Tracer namespace

__Tracers__ are organised into a hierarchical __tracer namespace__, where the tree nodes and leaves are identified by `Text` name components.

The __tracer namespace__ appears in the following contexts:

* __documentation__, where it defines the overall structure of the generated documentation output,
* __configuration__, where it allows referring to tracers we want to reconfigure in some way, such as changing their severity,
* __trace-outs__, where the __metrics__ and __log objects__ carry the __tracer name__.

Given a __tracer__ with a particular __tracer name__, we can derive a tracer with an extended __tracer name__:

```haskell
appendName :: Monad m => Text -> Trace m a -> Trace m a
```

As an example, consider the following tracer:

```
appendName "specific" $ appendName "middle" $ appendName "general" tracer
```

..which will have the name `["general", "middle", "specific"]`.

## Trace annotation

As mentioned in the overview, __traces__ are interpreted by the __dispatcher__ in a __trace context__.  This context consists of two parts:

* the __annotation context__ of the trace, as introduced by __trace combinators__, and
* the __configuration context__ coming from the program configuration (initial or runtime).

Both pieces meet together, to inform the following decisions:

1. __trace filtering__ -- whether a trace reaches particular __trace-outs__ or not,
2. __trace presentation__ -- which detail level is used during transformation of the __trace__ into __log objects__.

The __annotation context__ of the trace is defined as follows:

1. __trace filtering__ -- by __privacy__, __severity__ and __frequency__ annotations,
2. __trace presentation__ -- by __detail level__ annotations.

Only severity and detail level can be supplied in configuration.

## Filter annotations

### Severity

__Severity__ is expressed in terms of:

```haskell
data SeverityS
    = Debug | Info | Notice | Warning | Error | Critical | Alert | Emergency
```

..which ranges from minimum (`Debug`) to the maximum (`Emergency`) severity, and allows ignoring messages with severity level _below_ a configured global __severity cutoff__.

The following __trace combinators__ affect __annotated severity__ of a trace:

```haskell
withSeverity :: Monad m => (a -> Severity) -> Trace m a -> Trace m a
setSeverity  :: Monad m => Severity        -> Trace m a -> Trace m a

-- Unconditional annotation:
tracer   = setSeverity Notice trAddBlock
-- ..or equivalently:
tracer'  = withSeverity (const Notice)
                        trAddBlock
-- Conditional annotation:
tracer'' = withSeverity (\case
                           IgnoreBlockOlderThanK{}          -> Warning
                           IgnoreBlockAlreadyInVolatileDB{} -> Notice)
                        trAddBlock
```

If the combinators are applied multiple times to a single trace, only the innermost application affects it -- the rest of them is ignored.

```haskell
traceWith (setSeverity Warning trAddBlock) (IgnoreBlockOlderThanK b)
```

In addition, the __annotated severity__ of a particular trace can be changed in configuration, by referring to the __tracer name__ -- which allows to put it above or below the __global severity cutoff__, effectively either enabling or disabling it.

`Info` is the default __annotated severity__, in the absence of trace annotations.

See [Severity filtering implementation](#Severity-filtering-implementation) for a more formal explanation of semantics.

### Privacy

__Privacy__ is expressed in terms of:

```haskell
data Privacy
    = Confidential | Public
```

__Confidential__ privacy level means that the trace will not be externalised from the system, except via __standard output__.

The annotation mechanism is similar to the one of severity:

```haskell
privately :: Trace m a -> Trace m a
withPrivacy :: Monad m => (a -> Privacy) -> Trace m a -> Trace m a
```

`Public` is the default __annotated privacy__, in the absence of privacy annotations.

See [Confidentiality and privacy filtering implementation](#Confidentiality-and-privacy -filtering-implementation) for a more formal explanation of semantics.

### Frequency

__Frequency filtering__ is yet another part of __trace filtering__ parameter, and represents an optional limit on trace frequency.

Semantically this is implemented a randomly-fair suppression of messages when their moving-average frequency exceeds a given threshold parameter.

The __frequency limiter__ itself emits a __suppression summary__ message under the following conditions:

* when it message suppression begins, and
* when message suppression stops -- adding the number of suppressed messages.

__Frequency limiters__ are given a name to identify its activity.

```haskell
limitFrequency
  :: MonadIO m
  => Double                  -- ^ messages per second
  -> Text                    -- ^ name of this limiter
  -> Trace m a               -- ^ the limited trace
  -> Trace m LimitingMessage -- ^ a trace emitting the messages of the limiter
  -> m (Trace m a)           -- ^ the original trace

data LimitingMessage =
    StartLimiting Text
  | StopLimiting  Text Int
```

The frequency filtering is intended to be applied to a subset of traces (those known to be noisy).  For this subset of traces the frequency limit can be configured globally.

## Presentation
### Formatting

The `LogFormatting` typeclass is used to describe __trace presentation__ -- mapping __traces__ to __metrics__ and __log objects__.

* The `forMachine` method is used for a machine readable representation, which can varied through detail level.
  It's default implementation assumes no machine representation.

* the `forHuman` method shall represent the message in human readable form.
  It's default implementation defers to `forMachine`.

* the `asMetrics` method shall represent the message as 0 to n metrics.
  It's default implementation assumes no metrics. If a text is given it is
  appended as last element to the namespace.

An example implementation:

```haskell
class LogFormatting a where
  forMachine :: DetailLevel -> a -> A.Object
  forMachine _ v = mempty

  forHuman :: a -> Text
  forHuman v = ""

  asMetrics :: a -> [Metric]
  asMetrics v = []

data Metric
    = IntM (Maybe Text) Int
    | DoubleM (Maybe Text) Double
    deriving (Show, Eq)
```

The standard formatters, transforms a stream of messages of a, where a is an instance of _LogFormatter_ to a stream of _FormattedMessages_.

```haskell
data FormattedMessage = Human Text | Machine Text | Metrics [Metric]
```

The human formatter takes a Bool argument, which tells if color codes for stdout output shall be inserted, and an argument which is the app name, which gets prepended to the namespace, while the machineFormatter has as arguments the desired detail level and as well the application name. The metricas formatter takes no extra arguments:

```haskell
humanFormatter :: (LogFormatting a, MonadIO m)
  => Bool
  -> Text
  -> Trace m FormattedMessage
  -> m (Trace m a)

machineFormatter :: (LogFormatting a, MonadIO m)
  => DetailLevel
  -> Text
  -> Trace m FormattedMessage
  -> m (Trace m a)

metricsFormatter :: (LogFormatting a, MonadIO m)
  => Trace m FormattedMessage
  -> m (Trace m a)
```

The __detail level__ can be configured globally, and also per-trace, by referring to a particular __tracer name__.

#### Decide on type error instead of silent dropping of messages

> We cannot allow silent dropping of messages, which is relevant in light of the above:
>
> > It's default implementation assumes no machine representation.

#### Decide on more direct interface to EKG metrics

> One problem with the `asMetrics` interface, is that it forces an intermediate representation on the metrics flow -- an it also doesn't express all possibilities that EKG store provides.

### Detail level

An orthogonal aspect of __trace presentation__ is control over the amount of details presented for each trace.  This is important, because the emitted __program traces__ can be extremely detailed, which makes their complete presentation extremely expensive.

Additionally, because detail level is important for debugging (f.e. sometimes we want to see even the bytestring serialisation of a transaction), this control mechanism has to be configurable.

### Documentation

Self-documentation features are provided by a combination of:

* __Trace documentation__ mechanism, consisting of the `Documented a` type, carrying a list of per-constructor `DocMsg` description, and
* A documentation generation mode, that emits the entire document, using the __tracer namespace__ to guide the document structure.

The per-constructor `DocMsg` objects are constructed by providing:

* Prototype of the message -- a stubbed constructor invocation,
* Message documentation text, in Markdown format,

> * the most special name of the message in the namespace

*Because it is not enforced by the type system, it is very important to provide a complete list, as the prototypes are used as well for configuration*.

```haskell
newtype Documented a = Documented {undoc :: [DocMsg a]}

data DocMsg a = DocMsg {
    dmPrototype :: a
  , dmName      :: Text
  , dmMarkdown  :: Text
}
```

## Fold-based aggregation

Trace aggregation introduces a problem of concurrent access to shared state, because __tracers__ can be invoked from different threads.

While, in general, aggregation can span multiple __traces__, so a universal solution is not possible, we can still encode a useful pattern for a single-trace shared state control based on folds, both pure (`foldTraceM`) and impure (`foldMTraceM`):

```haskell
-- | Folds the function with state b over messages a in the trace.
foldTraceM :: MonadIO m
  => (acc -> a -> acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)

foldMTraceM :: forall a acc m . MonadIO m
  => (acc -> a -> m acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)

newtype Folding a acc = Folding acc
```

As an example we want to log a measurement value together with the sum of all measurements that occurred so far.  For this we define a `Measure` type to hold a `Double`, a `Stats` type to hold the the sum together with the measurement and a `fold`-friendly function to calculate new `Stats` from old `Stats` and `Measure`:

```haskell
data Stats = Stats {
    sMeasure :: Double,
    sSum     :: Double
    }

calculateS :: Stats -> Double -> Stats
calculateS Stats{..} val = Stats val (sSum + val)
```

Then we can define the aggregation tracer with the procedure foldTraceM in the
following way, and it will output the Stats:

```haskell
  aggroTracer <- foldTraceM calculateS (Stats 0.0 0.0) exTracer
  traceWith 1.1 aggroTracer -- measure: 1.1 sum: 1.1
  traceWith 2.0 aggroTracer -- measure: 2.0 sum: 3.1
```

### Discuss possibility of pure aggregation

> I would like to find a function foldTrace, that omits the MVar and can thus be called pure. Help is appreciated.

## Configuration

The configurability of __dispatcher__ this library allows to define is based on:

1. __Tracer namespace__-based configurability, down to trace constructor granularity,
2. Runtime reconfigurability, triggered by invocation of `configureTracers`,
3. Prototypes for each message constructor.

This is implemented by running the entire __dispatcher__ trace network at configuration time.

```haskell
-- The function configures the traces with the given configuration
configureTracers :: Monad m => TraceConfig -> Documented a -> [Trace m a]-> m ()
```

These are the options that can be configured based on a namespace:

```haskell
data ConfigOption = ConfigOption {
    -- | Severity level
    coSeverity     :: SeverityF
    -- | Detail level
  , coDetailLevel  :: DetailLevel
    -- | Privacy level
  , coPrivacy      :: Privacy
}

data TraceConfiguration = TraceConfiguration {
  ,  tcOptions :: Map Namespace ConfigOption
  , ...
}
```
More configuration options e.g. for different transformers and __trace-outs__ can be added by this mechanism.

# Integration and implementation in the node
## Overall tracing setup

As a result of the __trace__ / __tracer__ duality, the program components that wish to emit traces of particular types, must be parametrised with matching tracers.

Because all these tracers are defined as part of the __dispatcher__ definition, which is itself defined in a centralised location, that allows a certain program structure to emerge:

1. The program initialisation routine reads __trace configuration__ and uses that to parametrise the __dispatcher__ that would executes a __tracing policy__ defined by that configuration.
2. As mentioned previously, that dispatcher is generally expressed as a structured (preferably hierarchically structured) value that defines per-program-component set of __tracers__.
3. This __dispatcher__ (in other words, the set of __tracers__ it is composed of) is given as an argument to the rest of the program, which then distributes them to its components and begins execution.

## Trace-outs

__Trace-outs__, as mentioned before, are the destinations of all __traces__, after they have undergone __trace interpretation__ into __metrics__ and __log objects__.

There are two __trace-outs__ defined for the system:

1. `stdout`, the basic standard output destination.  It is notable in that it can also accept `Confidential` traces.
2. `trace-forwarder`, a purely network-only sink that forwards both __log objects__ and __metrics__ using a combination of dedicated protocols over TCP or local sockets.  Only processes `Public` traces.

The `trace-forwarder` is intended to be used as a __metric__ / __log object__ source for `RTView` and `cardano-logger`.

```haskell
stdoutTracer :: MonadIO m
  => Maybe FilePath
  -> m (Trace m FormattedMessage)

forwardingTracer :: MonadIO m
  => Either Metrics.Store Server
  -> m (Trace m FormattedMessage)
```

Configuring a __trace-out__ to output human-readable text (and therefore to use the human formatter), produces a presentation of the form `[HOST.NAMESPACE.SEVERITY.THREADID]`:

    [deus-x-machina.cardano.general.middle.specific.Info.379]

### Decide trace-outs types

> We shouldn't implement file-based tracing, unless we intend to implement it properly, i.e. with log rotation.
>
> One reason why writing to a file in the stdout backend is somewhat undesirable, is because it weakens the security property we assign to this backend -- the Confidential-ity enforcement.
>
> We should consider that we already have a dedicated `cardano-logger` component for file logging.

### Convenient namespace-extending tracing

Sometimes, while implementing the __trace dispatcher__, it's convenient to combine `traceWith` with `appendName`.

For this purpose, we provide `traceNamed`:

> ```haskell
> case event of
>   IgnoreBlockOlderThanK{} ->
>     traceNamed trAddBlock "IgnoreBlockOlderThanK" "Block ignored"
> ```

### Explicit trace filtering

Not all messages shall be logged, but only a subset. The most common case is when we want to see messages that have a minimum severity level of e.g. `Warning`.

This can be done by calling the function `filterTraceBySeverity`, which only processes messages further with a severity equal or greater as the given one. E.g.:

```haskell
let filteredTracer = filterTraceBySeverity WarningF exampleTracer
```

A more general filter function is offered, which gives access to the object and a `LoggingContext`, which contains the namespace, the severity, the privacy and the detailLevel:

```haskell
--- | Don't process further if the result of the selector function
---   is False.
filterTrace :: (Monad m) =>
     ((LoggingContext, a) -> Bool)
  -> Trace m a
  -> Trace m a

data LoggingContext = LoggingContext {
    lcNamespace   :: Namespace
  , lcSeverity    :: Maybe Severity
  , lcPrivacy     :: Maybe Privacy
  , lcDetailLevel :: Maybe DetailLevel
}
```
So you can e.g. write a filter function, which only displays _Public_ messages:

```haskell
filterTrace (\ (c, a) -> case lcPrivacy c of
                Just s  -> s == Public
                Nothing -> True)
```
We come back to trace filtering when we treat the _Configuration_, cause the configuration makes it possible to enable a fine grained control of which messages are filtered out and which are further processed based on the Namespace.

## Severity filtering implementation

__Trace filtering__ is affected by __annotation__ and __configuration__ components of the trace's __severity context__ as follows:

1. The effective __configuration severity__ of a trace is determined as a the __maximum__ of:
   * the configuration-specified __default severity__, and
   * the configuration-specified __trace-specific severity__
2. The trace is then ignored, if the trace's __annotated severity__ is __less__ than its __configuration severity__.

## Detail level control

This detail level control is expressed by:

```haskell
data DetailLevel = DBrief | DRegular | DDetailed
```
For setting this, the following functions can be used:

```haskell
setDetails :: Monad m => DetailLevel -> Trace m a -> Trace m a
withDetails :: Monad m => (a -> DetailLevel) -> Trace m a -> Trace m a
```

We explain in the section about formatting, how to specify which detail levels display which information.

## Confidentiality and privacy filtering implementation

__Trace filtering__ is affected by the __privacy context__ as follows:

1. `Confidential` traces can only reach the `stdout` __trace-out__.
2. `Public` traces reach both the `stdout` and `trace-forwarder` __trace-outs__.

It is worth noting, that enforcement of the trace privacy policy is expressed in terms of __trace-outs__.

In effect, it is impossible to leak the `Confidential` traces due to logging misconfiguration -- a leak can only happen if the user explicitly allows network access to the standard output of the traced program.

## Note on SilenceF severity filter

As en extension to the public set of severities, a private `SilenceF` constructor is defined which encodes unconditional silencing of a particular trace -- and therefore serves as a semantic expression of the `nullTracer` functionality.

## Documentation generation

To generate the documentation, first call `documentMarkdown` with the `Documented` type and all the tracers that are called. Do this for all message types you need, and then call `buildersToText` with the appended lists.

```haskell
  b1 <- documentMarkdown traceForgeEventDocu [t1, t2]
  b2 <- documentMarkdown .. ..
  ..
  bn <- documentMarkdown .. ..
  writeFile "Docu.md" (buildersToText (b1 ++ b2 ++ ... ++ bn))
```

The generated documentation for a simple message my look like this:

> #### cardano.node.StartLeadershipCheck
>   For human:
>   `Checking for leadership in slot 1`
>
>   For machine:
>   `{"kind":"TraceStartLeadershipCheck","slot":1}`
>
>   Integer metrics:
>   `aboutToLeadSlotLast 1`
>
>   > Severity:   `Info`
>   >
>   >   Privacy:   `Public`
>   >
>   >   Details:   `DRegular`
>
>   Backends: `KatipBackend ""` / `Machine`, `KatipBackend ""` / `Human`
>
>   ***
>   Start of the leadership check
>
>   We record the current slot number.
>   ***

## Plumbing

To send the message of a trace to different tracers depending on some criteria use the following function:

```haskell
routingTrace :: Monad m => (a -> Trace m a) -> Trace m a
let resTrace = routingTrace routingf (tracer1 <> tracer2)
  where
    routingf LO1 {} = tracer1
    routingf LO2 {} = tracer2
```
The second argument must mappend all possible tracers of the first argument to one tracer. This is required for the configuration. We could have construct a more secure interface by having a map of values to tracers, but the ability for full pattern matching outweigh this disadvantage in our view.
In the following example we send the messages of one trace to two tracers simultaneously:

```haskell
let resTrace = tracer1 <> tracer2
```
To route one trace to multiple tracers simultaneously we use the fact that Tracer is a `Semigroup` and then use `<>`, or `mconcat` for lists of tracers:

```haskell
(<>) :: Monoid m => m -> m -> m
mconcat :: Monoid m => [m] -> m
```

In the third example we unite two traces to one tracer, for which we trivially use the same tracer on the right side.

```haskell
tracer1  = appendName "tracer1" exTracer
tracer2  = appendName "tracer2" exTracer
```
# Appendix

## Decisions

### Decide inline trace type annotation with trace function

__DECISION: move `traceNamed` to the dispatcher API__

> Alternatively, to trace that value, while extending the name of the trace inside the program (as opposed to deferring that to the dispatcher), the __trace__ function can be used:
>
> ```haskell
> traceNamed trAddBlock "ignoreBlock" (IgnoreBlockOlderThanK p)
> ```

### Decide tracer definedness

DECISION: Every message constructor has to have a unique tracer name.

DECISION: Therefore, each __tracer__ has a __tracer name__ assigned to it, which is, conceptually, a potentially empty list of `Text` identifiers.

### Decide tracer name definition

> We could have used the (`Type` * `Constructor`) pair, which is a more technical approach.  Problems with that:
> 1. __synthetic traces__ exist.
> 2. Developer-provided type/constructor names are not necessarily ideal from user standpoint.

DECISION: there is value in maintaining a user-friendly trace message namespace.

### Decide inline trace type annotation with trace function 2

DECISION: we use `traceWith` in the library code and `traceNamed` in th dispatcher.

> Since we require that every message has its unique name we encourage the use of the already introduced convenience function:
>
> ```haskell
> traceNamed exampleTracer "ignoreBlock" (IgnoreBlockOlderThanK b)
> -- instead of:
> traceWith (appendName "ignoreBlock" exampleTracer) (IgnoreBlockOlderThanK b)
> ```

### Decide on explicit trace filtering

DECISION:  move to [Integration and implementation in the node](#Integration-and-implementation-in-the-node).

### Decide on privately combinator

> Instead of the `setPrivacy` combinator, we could save the trouble of passing the privacy argument, by relying on the fact that default privacy is `Public`, and introduce instead a `privately` combinator:
>
> ```haskell
> privately :: Trace m a -> Trace m a
> ```
> This combinator potentially entirely replaces `setPrivacy` and `withPrivacy`.

DECISION: we agree to add `privately` to the API.

### Decide on dispatcher detail level control

It doesn't seem to make sense to decide on detail level inside the dispatcher -- so seems to be a purely configuration+`LogFormatting`-defined mechanism.

DECISION: Move to the implementation API.

### Decide namespace-aware configuration

> It doesn't make a lot of sense to configure Privacy.
>
> It could make sense to configure frequency limits.

DECISION: Move to the implementation API.  Privacy should not be configurable.  Frequency limits should be configurable, at least globally -- maybe not per-namespace.

### Decide missing configuration

DECISION:

* Global severity cutoff + per namespace.
* Global detail level + per namespace.
* Global *trace-out* configuration: stdout, trace forwarder.

## Future work

There is a number of topics that were discussed, but deferred to a latter iteration of design/implementation:

1. Lightweight documentation references, GHC style -- this would allow us to refer to named pieces of documentation in source code, as opposed to copy-pasting them into trace documentation.
2. Change of human-oriented presentation machinery.
