# trace-dispatcher: efficient, simple and flexible program tracing

`trace-dispatcher` is a library that enables definition of __tracing systems__ -- systems that collect and manages traces -- as evidence of program execution.

- [trace-dispatcher: efficient, simple and flexible program tracing](#trace-dispatcher-efficient-simple-and-flexible-program-tracing)
- [Introduction](#introduction)
  - [Rationale](#rationale)
  - [Transition Period](#transition-period)
  - [Key Recommendations for Developers](#key-recommendations-for-developers)
- [Basic Tracer Topics](#basic-tracer-topics)
  - [Tracer Construction Basics](#tracer-construction-basics)
  - [Namespace Concept Explanation](#namespace-concept-explanation)
  - [Typeclasses Overview](#typeclasses-overview)
    - [LogFormatting Typeclass](#logformatting-typeclass)
    - [MetaTrace Typeclass](#metatrace-typeclass)
  - [Metrics Integration](#metrics-integration)
  - [Frequency Limiting in Trace Filtering](#frequency-limiting-in-trace-filtering)
  - [Configuration](#configuration)
- [Advanced Tracer Topics](#advanced-tracer-topics)
  - [Integrating a New Tracer into cardano-node](#integrating-a-new-tracer-into-cardano-node)
  - [Message Filtering based on Severity](#message-filtering-based-on-severity)
  - [Comprehensive Trace Filtering](#comprehensive-trace-filtering)
  - [Privacy Annotations](#privacy-annotations)
  - [Detail Level in Trace Presentation](#detail-level-in-trace-presentation)
  - [Fold-Based Aggregation](#fold-based-aggregation)
  - [Dispatcher Routing Mechanism](#dispatcher-routing-mechanism)
  - [Documentation Generation](#documentation-generation)
  - [Consistency Checking](#consistency-checking)
  - [Trace Backends Overview](#trace-backends-overview)
  - [Data Points Overview and Deprecation Notice](#data-points-overview-and-deprecation-notice)
- [Appendix](#appendix)
  - [References](#references)
  - [Future work](#future-work)
    - [Versioning](#versioning)
    - [Trace Consumers](#trace-consumers)

# Introduction

## Rationale

The `trace-dispatcher` library serves as a sophisticated solution for streamlined and effective tracing systems. Built upon the arrow-based `contra-tracer` framework, it surpasses the capabilities of the `iohk-monitoring` framework with the following enhancements:

- Persistent activation of all tracers, adhering to the configured severity levels.

- Granular configuration (such as filtering, limiting) of individual tracers based on hierarchical namespaces, extending down to individual messages.

- Seamless transmission of traces to a dedicated `cardano-tracer` process capable of handling traces from multiple nodes.

- Dynamic reconfiguration (i.e. hot-reloading) of tracing settings within a running node (after removal of legacy tracing).

- Automatic generation of comprehensive documentation encompassing all trace messages, metrics, and datapoints.

- Sanity and consistency checking of tracer implementations and tracing settings based on the system's introspective capability.

## Transition Period

During the transitional phase, both legacy tracing, based on the `iohk-monitoring` framework, and new tracing, leveraging `trace-dispatcher` and `cardano-tracer`, will coexist.

This interim period provides an opportunity to thoroughly test and enhance the new tracing system. Given the extensive repertoire of over 600 trace messages, the possibility of uncovering regressions and bugs is anticipated. Your assistance in identifying and rectifying these issues is invaluable.

Please be aware that, owing to compatibility with the legacy system, the new tracing functionality will be slightly constrained during this transitional phase. Certain features, such as dynamic reconfiguration of a running node, will be temporarily unavailable. Additionally, there may be redundant implementations that are currently necessary but slated for refinement.

To activate new tracing, set the `UseTraceDispatcher` in the node's config file value to `true`. When making this switch, ensure that the configuration file includes the requisite values for the new tracing setup, as detailed in the subsequent section.

## Key Recommendations for Developers

Kindly consider the following important suggestions:

- The current tracing system employs two methods for message identification: a hierarchical name known as its Namespace and the Kind field in machine representation. Our implementation is rooted in the namespace, and we are actively moving towards deprecating the Kind field for a singular reliance on namespaces. Therefore, we strongly recommend utilizing namespaces for any trace analysis tools, as the _Kind field will be phased out in the near future_.

- Avoid using strictness annotations for trace types. Given that trace messages are either promptly discarded or instantly converted to another format without storage, strictness annotations introduce unnecessary inefficiencies without tangible benefits.

- When developing new tracers, consider creating the new tracers first and subsequently mapping to old tracers. You can refer to numerous examples in `cardano-node` under `Cardano.Node.Tracing.Tracers`.

- For inquiries and reviews, please reach out to the Performance & Tracing team. Your collaboration and questions are welcome to ensure a seamless transition and optimal utilization of the new tracing framework.

# Basic Tracer Topics

## Tracer Construction Basics

1. Define an Algebraic Data Type (ADT) and assign distinct constructors to each trace message.

An example is:

```haskell
data TraceAddBlockEvent blk =
    IgnoreBlockOlderThanK (RealPoint blk)
  | IgnoreBlockAlreadyInVolatileDB (RealPoint blk)
  ...
```

2. Create a tracer for this data type using the provided Haskell function:

```haskell
-- | Generate a tracer conforming to the cardano node requirements.
-- The tracer must be an instance of LogFormatting for message display
-- and an instance of MetaTrace for meta-information such as
-- severity, privacy, details, and backends.
-- The tracer receives those backends as arguments:
--   * 'trStdout':  stdout tracing
--   * 'trForward': trace forwarding
--   * 'mbTrEkg':   (optional) EKG monitoring
-- The tracer is supplied with a 'name' as an array of text, which is prepended to its namespace.
-- This function returns the new tracer.

mkCardanoTracer :: forall evt.
    ( LogFormatting evt
    , MetaTrace evt )
  => Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> [Text]
  -> IO (Trace IO evt)
```

It is imperative that the tracer backends (the first three parameters) remain consistent across all tracers. For example, only one stdout backend is permitted for use in any program.

3. Configure the returned tracer with:

```haskell
-- | Invoke this function during initialization (and potentially later for reconfiguration).
-- ConfigReflection is utilized to gather information about the tracers
-- and is employed to optimize the tracers.
-- TraceConfig represents the configuration, typically loaded from a configuration file.
-- While it is feasible to provide more than one tracer of the same type,
-- this scenario is not common.
-- This function does not return a value.

configureTracers :: forall a m.
    ( MetaTrace a
    , MonadIO m )
  => ConfigReflection
  -> TraceConfig
  -> [Trace m a]
  -> m ()
```

4. Trace Emission Process

To emit a trace, employing a message and its corresponding tracer, utilize the `traceWith` function:

```haskell
traceWith :: Trace m a -> a -> m ()
-- For example:
addBlockTracer <- mkCardanoTracer trStdout trForward (Just trEkg) ["ChainDB"]
configureTracers configReflect config [addBlockTracer]
..
traceWith addBlockTracer (IgnoreBlockOlderThanK p)
```

## Namespace Concept Explanation

Understanding the concept of namespaces is crucial for comprehending the tracing system and the `MetaTrace` typeclass. Tracers are systematically organized within a hierarchical tracer namespace, with tree nodes and leaves identified by `Text` name components.

The trace dispatcher requires careful organization to ensure that all messages possess a unique name within this namespace. Moreover, the same tracer type can be utilized in different contexts, such as for local and remote messages. To enable this flexibility, the 'inner' namespace is prefixed by the namespace passed to a tracer during construction (refer to `mkCardanoTracer` example above).

```haskell
-- A unique identifier for every message, composed of arrays of text
-- A namespace can also appear with the tracer name (e.g., "ChainDB.OpenEvent.OpenedDB"),
-- or more prefixes; currently, a NamespaceOuter is used.
-- The inner namespace may not be empty.
data Namespace a = Namespace {
    nsPrefix :: [Text]
  , nsInner  :: [Text]}
```

Every namespace is composed of:

- system namespace (empty for cardano, but was cardano in old tracing)
- tracer namespace (argument of mkCardanoTracer)
- inner namespace (provided by the MetaTrace typeclass)

The tracer namespace serves pivotal roles in:

- __Documentation__: It defines the overall structure of the generated documentation output.

- __Configuration__: It allows reference to tracers that need reconfiguration, such as altering their severity.

- __Output__: The messages carry the tracer namespace, providing clarity and context in the output.

## Typeclasses Overview

For the effective integration of trace messages into the tracing system, two essential typeclasses must be implemented: one for message formatting and another for meta-information.

### LogFormatting Typeclass

The `LogFormatting` typeclass governs the presentation of trace messages, encompassing the mapping of traces to metrics and messages. It includes the following methods:

- The `forMachine` method caters to a machine-readable representation, adaptable based on the detail level. Implementation is mandatory for the trace author. The system will render this,
along with trace metadata, as JSON of type `Cardano.Logging.Types.TraceMessage.TraceMessage`.

- The `forHuman` method renders the message in a human-readable form. Its default implementation is an
empty text. Whenever the system encounters the empty text, it will replace it with the machine-readable JSON, rendering it as a value in `{"data": <value>}`, preventing potential loss of log information

- The `asMetrics` method portrays the message as 0 to n metrics. The default implementation assumes no metrics. Each metric can optionally specify a hierarchical identifier as a `[Text]`.

```haskell
class LogFormatting a where
  -- Machine readable representation with varying details based on the detail level.
  forMachine :: DetailLevel -> a -> Aeson.Object

  -- Human readable representation.
  forHuman :: a -> Text
  forHuman _v = ""

  -- Metrics representation.
  asMetrics :: a -> [Metric]
  asMetrics _v = []
```

Metrics, represented as numbers, serve to monitor the running system and can be accessed, for example, through Prometheus.

```haskell
data Metric
  -- Integer metric with a named identifier.
    = IntM Text Integer
  -- Double metric with a named identifier.
    | DoubleM Text Double
  -- Counter metric with a named identifier and an optional limit.
    | CounterM Text (Maybe Int)
  deriving (Show, Eq)
```

### MetaTrace Typeclass

The `MetaTrace` typeclass plays a pivotal role in providing meta-information for trace messages. It includes the following methods:

- __namespaceFor__: Offers a distinct (inner) namespace for each trace message.

- __severityFor__: Provides severity for a given namespace. As some severities depend not only on the message type but also on the individual message, the actual message may be passed as well.

- __privacyFor__: Determines whether a message is `Private` or `Public`. Private messages are not sent to `cardano-tracer` and are only displayed on the stdout trace. If no implementation is given, `Public` is chosen.

- __detailsFor__: Specifies the level of details for printing messages. Options include `DMinimal`, `DNormal`, `DDetailed`, and `DMaximum`. If no implementation is given, `DNormal` is chosen.

- __documentFor__: Allows the addition of optional documentation for messages as text. See section [Documentation Generation](#documentation-generation) later in this document.

- __metricsDocFor__: Enables the addition of documentation for metrics carried by the respective message. If no implementation is given, the default is no metrics.

- __allNamespaces__: Must return an array with all namespaces of this trace type.

```haskell
class MetaTrace a where
  namespaceFor  :: a -> Namespace a

  severityFor   :: Namespace a -> Maybe a -> Maybe SeverityS

  privacyFor    :: Namespace a -> Maybe a -> Maybe Privacy
  privacyFor _ _ =  Just Public

  detailsFor    :: Namespace a -> Maybe a -> Maybe DetailLevel
  detailsFor _ _ =  Just DNormal

  documentFor   :: Namespace a -> Maybe Text

  metricsDocFor :: Namespace a -> [(Text, Text)]
  metricsDocFor _ = []

  allNamespaces :: [Namespace a]
```

## Metrics Integration

Metrics are seamlessly incorporated into the system through regular trace messages implementing the `asMetrics` function within the `LogFormatting` typeclass. Unlike other trace components, metrics are not subjected to filtering and are consistently provided. This occurs as long as the `EKGBackend` is configured for the message. The `EKGBackend` then forwards these metrics to `cardano-tracer` for additional processing. Subsequently, they are dispatched as Prometheus metrics, extending their utility and visibility.

It is essential to implement the metricsDoc function of the MetaTrace typeclass, as this information is utilized to optimize system performance.

The configuration option TraceOptionMetricsPrefix can be used to prepend a prefix to any metrics name. For example, the prefix could be "cardano.node".

## Frequency Limiting in Trace Filtering

Frequency filtering is an integral aspect of trace filtering, offering an optional mechanism to limit the observable frequency of individual trace messages.

In essence, this involves a fair and probabilistic suppression of messages within a particular trace when their moving-average frequency surpasses a specified threshold parameter.

The frequency limiter, in addition to controlling message frequency, emits a suppression summary message under specific conditions:

- When message suppression commences.
- Every 10 seconds during active limiting, providing the count of suppressed messages.
- When message suppression concludes, indicating the total number of suppressed messages.

Usually frequency limiters can be just added by configuration, for special cases you
can construct them in your code. Each frequency limiter is assigned a name for identification purposes:

```haskell
limitFrequency
  :: forall a m . MonadUnliftIO m
  => Double   -- messages per second
  -> Text     -- name of this limiter
  -> Trace m TraceDispatcherMessage -- the limiter's messages
  -> Trace m a -- the trace subject to limitation
  -> m (Trace m a) -- the original trace
```

It is important to note that frequency filtering is designed to be applied selectively to a subset of traces, specifically those identified as potentially noisy. The configuration of frequency limits can thus be tailored to this subset of traces.

## Configuration

The configurability of dispatchers provided by this library relies on:

1. __Tracer Namespace-based Configurability__: Configurable down to single message granularity based on tracer namespaces.

2. __Runtime Reconfigurability__: Triggered by invoking `configureTracers`, enabling changes during program execution.

The usual form to provide a configuration is via a configuration file, which can be in JSON or YAML format. The options that
can be given based on a namespace are: `severity`, `detail`, `backends` and `limiter`.

Backends can be a combination of `Forwarder`, `EKGBackend`, `PrometheusSimple [suffix|nosuffix] [bindhost] <port>` and
one of `Stdout MachineFormat`, `Stdout HumanFormatColoured` and `Stdout HumanFormatUncoloured`.

The connection for the `Forwarder` backend is provided on the application command line. It is a socket path over which applications like `cardano-node` connect with `cardano-tracer`. `--tracer-socket-path-connect /path/to/forward.sock` sets
the backends's role to `Initiator`, whereas `--tracer-socket-path-accept /path/to/forward.sock` sets it to `Responder`. Except for debugging purposes, the former should be chosen: the application takes the `Initiator` role, and `cardano-tracer` is
in the `Responder` role, which means setting its network `tag` to `AcceptAt` in its config (see there).

The `PrometheusSimple` backend provides Prometheus metrics _directly from the process_, without forwarding. It always applies to all tracers globally, and should only be configured once.
Providing an available port number in the connection string is mandatory; this will bind to localhost only by default. By specifying a bind host, the metrics can be queried remotely, e.g. over IPv4 by
binding to `0.0.0.0`, or IPv6 by binding to `::`. Metrics will be available under the URL `/metrics`.
The `nosuffix` modifier removes suffixes like `_int` from metrics names, making them more similar to those in the old system; `suffix` is the implicit default and can be omitted.

*CAUTION*: Generally allowing remote queries of Prometheus metrics is risky and should only be done in an environment you control.

```yaml
# Use new tracing
UseTraceDispatcher: True

TraceOptions:
  "": # Options for all tracers, if not overwritten:
    severity: Notice
    detail: DNormal
    backends:
      - Stdout MachineFormat
      - EKGBackend
      - Forwarder
      - 'PrometheusSimple :: 1234' # Prometheus metrics available over IPv6 (and localhost) on port 1234

  ChainDB: # Show as well messages with severity Info for all ChainDB traces.
    severity: Info
    detail: DDetailed

  ChainDB.AddBlockEvent.AddedBlockToQueue: # Limit the AddedBlockToQueue events to a maximum of two per second.
    maxFrequency: 2.0

TraceOptionForwarder: # Configure the forwarder
    maxReconnectDelay: 20

# Any metrics emittted will get this prefix
TraceOptionMetricsPrefix: "cardano.node.metrics."
```

The same in JSON looks like this:

```json
{
  "UseTraceDispatcher": true,
  "TraceOptions": {
    "": {
      "severity": "Notice",
      "detail": "DNormal",
      "backends": [
        "Stdout MachineFormat",
        "EKGBackend",
        "Forwarder",
        "PrometheusSimple :: 1234"
      ]
    },
    "ChainDB": {
      "severity": "Info",
      "detail": "DDetailed"
    },
    "ChainDB.AddBlockEvent.AddedBlockToQueue": {
      "maxFrequency": 2.0
    }
  },
  "TraceOptionForwarder": {
    "maxReconnectDelay": 20
  },
  "TraceOptionMetricsPrefix": "cardano.node.metrics."
}
```

For explanations of the trace forwarder option refer to the following document:

[New Tracing Quickstart](https://github.com/input-output-hk/cardano-node-wiki/wiki/New-Tracing-Quickstart)

When `TraceOptions` is empty, or other entries are missing in the configuration file, default entries are taken from
[Cardano.Node.Tracing.DefaultTraceConfig](https://github.com/intersectmbo/cardano-node/blob/master/cardano-node/src/Cardano/Node/Tracing/DefaultTraceConfig.hs) module.

# Advanced Tracer Topics

The functionality of the new tracing system is composable using basic combinators defined on contravariant tracing.
In this part of the document we introduce the underlying functions. You should look here if you want to
implement some advanced functionality.

## Integrating a New Tracer into cardano-node

Presently, the process of adding a new tracer involves making changes in three specific modules. However, we anticipate that this requirement will be simplified once the old tracing system is phased out. The current modules where modifications are needed to add a new tracer are:

- __Cardano.Node.Tracing.Tracers__

- __Cardano.Node.Tracing.Documentation__

- __Cardano.Node.Tracing.Consistency__

## Message Filtering based on Severity

The concept of severity in the new system is articulated through an enumeration outlined in [section 6.2.1 of RFC 5424](https://tools.ietf.org/html/rfc5424#section-6.2.1). The severity levels, ranging from the least severe (`Debug`) to the most severe (`Emergency`), provide a framework for ignoring messages with severity levels below a globally configured severity cutoff.

To enhance severity filtering, we introduce the option of `Silence`. This addition allows for the unconditional silencing of a specific trace, essentially representing the deactivation of tracers — a semantic continuation of the functionality in the legacy system.

The following trace combinators play a role in modifying the annotated severity of a trace:

```haskell
-- Sets severities for the messages in this trace based on the MetaTrace class
withSeverity :: forall m a. (Monad m, MetaTrace a) => Trace m a -> Trace m a

-- Sets severity for the messages in this trace
setSeverity :: Monad m => SeverityS -> Trace m a -> Trace m a

-- Filters out messages with a severity less than the given one
filterTraceBySeverity :: Monad m
  => Maybe SeverityF
  -> Trace m a
  -> Trace m a
```

When these combinators are applied multiple times to a single trace, only the outermost application has an effect, rendering subsequent applications inconsequential.

In the absence of trace context or configured severity overrides, `Info` serves as the default severity.

## Comprehensive Trace Filtering

A versatile filtering mechanism is provided, granting access to both the object and a `LoggingContext`, encompassing the namespace along with optional severity, privacy, and detail level:

```haskell
-- Don't process further if the result of the selector function
-- is False.
filterTrace :: (Monad m)
  => ((LoggingContext, a) -> Bool)
  -> Trace m a
  -> Trace m a

-- Context carried by any log message
data LoggingContext = LoggingContext {
    lcNSInner   :: [Text]
  , lcNSPrefix  :: [Text]
  , lcSeverity  :: Maybe SeverityS
  , lcPrivacy   :: Maybe Privacy
  , lcDetails   :: Maybe DetailLevel
  }
```

For instance, you can create a filter function to display only _Public_ messages:

```haskell
filterTrace (\(c, _) -> case lcPrivacy c of
                Just s  -> s == Public
                Nothing -> False) -- privacy unknown, don't send out
```

This capability allows for flexible and fine-grained control over the inclusion or exclusion of messages based on a variety of contextual criteria.

## Privacy Annotations

In our tracing system, privacy annotations empower the distinction of messages that remain within the system and are not sent over the network, but are solely displayed on stdout. This privacy feature is defined through the following enumeration:

```haskell
data Privacy
    = Confidential | Public
```

When a trace carries a __Confidential__ privacy level, it implies that the trace remains internalized within the system, with the exception of being displayed via standard output.

The annotation mechanism for privacy mirrors that of severity:

```haskell
-- Sets privacy for the messages in this trace based on the MetaTrace class
withPrivacy :: forall m a. (Monad m, MetaTrace a) => Trace m a -> Trace m a

-- Sets privacy Confidential for the messages in this trace
privately :: Monad m => Trace m a -> Trace m a

-- Only processes messages further with a privacy greater than the given one
filterTraceByPrivacy :: (Monad m) =>
     Maybe Privacy
  -> Trace m a
  -> Trace m a
```

In the absence of privacy annotations, `Public` serves as the default privacy level.

Trace privacy, unlike severity, is not configurable.

Trace filtering responds to privacy context as follows:

1. Traces marked as `Confidential` can solely reach the `stdout` trace-out.
2. Traces marked as `Public` reach both the `stdout` and `trace-forwarder` trace-outs.

Effectively, preventing leaks of `Confidential` traces due to logging misconfiguration is inherent — any potential leak can only occur if the user explicitly permits network access to the standard output of the traced program.

## Detail Level in Trace Presentation

A crucial facet of trace presentation is the degree of detail provided for each trace. This consideration holds significance because the generated program traces may inherently include exhaustive details. Presenting every intricate detail in its entirety could impose a considerable burden on trace handling.

To address this, a configurable mechanism for controlling the level of detail is introduced, allowing customization down to specific messages.

The control over detail levels is manifested through the following enumeration:

```haskell
data DetailLevel = DMinimal | DNormal | DDetailed | DMaximum
```

This detail level control ensures that the presentation of traces strikes a balance between informativeness and efficiency, catering to diverse needs and preferences.

## Fold-Based Aggregation

When there is a need for aggregating information from multiple consecutive messages, the following fold functions can be employed:

```haskell
-- Folds the monadic cata function with acc over a.
-- Uses an MVar to store the state
foldTraceM :: forall a acc m . (MonadUnliftIO m)
  => (acc -> LoggingContext -> a -> m acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)

-- Like foldTraceM, but filters the trace by a predicate.
foldCondTraceM :: forall a acc m . (MonadUnliftIO m)
  => (acc -> LoggingContext -> a -> m acc)
  -> acc
  -> (a -> Bool)
  -> Trace m (Folding a acc)
  -> m (Trace m a)
```

To facilitate typechecking, the `Folding` type is utilized, and it can be removed by the `unfold` function:

```haskell
newtype Folding a acc = Folding acc

unfold :: Folding a b -> b
unfold (Folding b) = b
```

Given that tracers can be invoked from different threads, an `MVar` is internally employed to ensure correct behavior.

As an illustrative example, let's consider a scenario where we want to log a measurement value along with the sum of all measurements recorded thus far. We define a `Stats` type to store the sum alongside the measurement, and a `fold`-compatible function to calculate new `Stats` from old `Stats` and `Measure`:

```haskell
data Stats = Stats {
    sMeasure :: Double,
    sSum     :: Double
    }

calculateS :: MonadIO m => Stats -> LoggingContext -> Double -> m Stats
calculateS Stats{..} _ val = pure $ Stats val (sSum + val)
```

With these components in place, we can define the aggregation tracer using the `foldTraceM` procedure. Subsequently, when we log measurement values, the tracer outputs the corresponding `Stats`:

```haskell
aggregationTracer <- foldTraceM calculateS (Stats 0.0 0.0) exampleTracer
traceWith 1.1 aggregationTracer -- measure: 1.1 sum: 1.1
traceWith 2.0 aggregationTracer -- measure: 2.0 sum: 3.1
```

This demonstrates how fold-based aggregation facilitates the accumulation of information over consecutive messages, enabling insightful data summaries.

## Dispatcher Routing Mechanism

In the process of defining the trace dispatcher, it can be advantageous to employ a set of functions for routing messages. When there's a need to dispatch a trace message to different tracers based on specific criteria, the following function proves valuable:

```haskell
-- Allows routing to different tracers, based on the message being processed.
-- The second argument must mappend all possible tracers of the first
-- argument to one tracer. This is required for the configuration!
routingTrace :: forall m a. Monad m
  => (a -> m (Trace m a))
  -> Trace m a
  -> Trace m a

let resTrace = routingTrace routingFunction (tracer1 <> tracer2)
  where
    routingFunction LO1 {} = tracer1
    routingFunction LO2 {} = tracer2
```

In this context, the second argument must encapsulate the combination (using `mappend`) of all tracers utilized in the routing trace function into a single tracer. This amalgamation is crucial for the subsequent configuration steps.

While a more secure interface could be constructed using a map of values to tracers, the choice here prioritizes the ability for comprehensive pattern matching. The flexibility offered by full pattern matching outweighs the potential disadvantages, given the context.

Similarly, to route a single trace to multiple tracers simultaneously, the fact that `Tracer` is a `Semigroup` allows us to utilize the `<>` operator or `mconcat` for lists of tracers:

```haskell
(<>) :: Monoid m => m -> m -> m
mconcat :: Monoid m => [m] -> m
```

For instance, to direct messages from one trace to two tracers simultaneously, we can use:

```haskell
let resTrace = tracer1 <> tracer2
```

## Documentation Generation

The documentation for tracers is periodically generated and can be accessed in the cardano-node-wiki repository at the following path: [cardano-node-wiki/tracers_doc_generated.md](https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/new-tracing/tracers_doc_generated.md).

To generate the documentation within GHCi, load the `Cardano.Node.Tracing.Documentation` module and execute the `runTraceDocumentationCmd` function with the appropriate parameters:

```haskell
data TraceDocumentationCmd
  = TraceDocumentationCmd
    { tdcConfigFile :: FilePath -- file path to a node config file
    , tdcOutput     :: FilePath -- file path to output the documentation
    }

runTraceDocumentationCmd
  :: TraceDocumentationCmd
  -> IO ()
```

The self-documentation capabilities of `trace-dispatcher` rely on documentation annotations provided by the `documentFor` and `metricsDocFor` methods within the `MetaTrace` typeclass. Additionally, a specialized dispatcher execution mode emits documentation for all annotated traces, utilizing the tracer namespace to structure the document.

To generate the documentation, first, call `documentTracer` for each message type with the associated tracers, then use `docuResultsToText` with the accumulated lists.

```haskell
-- This function calls document tracers and returns a DocTracer result
documentTracer :: forall a.
     MetaTrace a
  => Trace IO a
  -> IO DocTracer

-- Finally, generate text from all the builders
docuResultsToText :: DocTracer -> TraceConfig -> IO Text

-- For example
  b1 <- documentTracer traceForgeEventDocu [t1, t2]
  b2 <- documentTracer .. ..
  ..
  bn <- documentTracer .. ..
  writeFile "Docu.md" (docuResultsToText (b1 ++ b2 ++ ... ++ bn))
```

A generated documentation snippet for a simple message may appear as follows:

__Forge.Loop.StartLeadershipCheck__

> Start of the leadership check.

Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`

From the current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

## Consistency Checking

As namespaces are essentially strings, the type system doesn't inherently ensure the consistency of namespaces. To address this concern, we have incorporated consistency check functionality into `trace-dispatcher`. Within the node, you can invoke the following procedure from the `Cardano.Node.Tracing.Consistency` module. It returns an array of `Text`, an empty list indicating that everything is in order.

```haskell
-- | Check the configuration in the given file.
-- Check the general structure of namespaces.
-- An empty return list means everything is well.
checkNodeTraceConfiguration ::
     FilePath -- path to a node configuration file
  -> IO [Text]
```

An example text is "Config namespace error: i.am.an.invalid.namespace" .

This check is performed within a `cardano-node` test case (`Test.Cardano.Tracing.NewTracing.Consistency.tests`), ensuring that it is automatically verified with each pull request.

The consistency checks cover the following aspects:

- Every namespace in `all namespaces` must be unique.

- Each namespace is a terminal and is not a part of another namespace.

- Namespaces in the `severityFor`, `privacyFor`, `detailsFor`, `documentFor`, and `metricsDocFor` functions are consistent with the `allNamespaces` definition.

- Any namespace in the configuration must be found by a hierarchical lookup in `all namespaces`.

If the checker encounters any problems it emits a `TracerConsistencyWarnings` message through the
`Cardano.Logging.TraceDispatcherMessage` type. The message is routed via the `Reflection` namespace
and carries `Warning` severity so that misconfigured namespaces are surfaced prominently in both the
logs and forwarded tracing output.

## Trace Backends Overview

As mentioned earlier, trace backends serve as the final destinations for all traces once they have undergone trace interpretation, resulting in metrics and messages. The system defines three trace backends:

1. __Standard Tracer:__ This is the fundamental standard output tracer. Notably, it can accept both regular and confidential traces.

    ```haskell
    standardTracer :: forall m. (MonadIO m)
      => m (Trace m FormattedMessage)
    ```

2. __Trace-Forward Tracer:__ This is a network-only sink dedicated to forwarding messages using typed protocols over TCP or local sockets. It exclusively handles public traces.

    ```haskell
    forwardTracer :: forall m. (MonadIO m)
      => ForwardSink TraceObject
      -> Trace m FormattedMessage
    ```

3. __EKG Tracer:__ This tracer submits metrics to a local EKG store (which then can be exposed directly via the `PrometheusSimple` backend and/or forwarded).

    ```haskell
    ekgTracer :: MonadIO m
      => Metrics.Store
      -> m (Trace m FormattedMessage)
    ```

It's imperative to note that constructing more than one instance of each tracer in an application should absolutely be avoided, as it may result in unexpected behaviour.

## Data Points Overview and Deprecation Notice

In the imminent future, `DataPoint`s will be deprecated and replaced by a subscription model.

`DataPoint`s provide a means for processes outside of `cardano-node` to inquire about the node's runtime state. Essentially similar to metrics, `DataPoint`s, however, have an Algebraic Data Type (ADT) structure, allowing them to represent structured information beyond simple metrics. This feature enables external processes to query and access specific details of a running cardano-node, such as the node's basic information.

Implemented as special tracers, `DataPoint`s package objects into `DataPoint` constructors and necessitate a `ToJSON` instance for these objects. The set of `DataPoint`s provided by the node follows the same namespace structure as metrics and log messages. While `DataPoint`s operate independently of tracing, they are stored locally, facilitating on-demand queries for the latest values of a specific `DataPoint`.

It is important to note that DataPoints will soon be deprecated, and a subscription model will take their place. Additionally, detailed information on accepting DataPoints from an external process can be found in [this document](https://github.com/input-output-hk/cardano-node-wiki/wiki/cardano-node-and-DataPoints:-demo). The [`demo-acceptor`](https://github.com/intersectmbo/cardano-node/blob/master/cardano-tracer/demo/acceptor.hs) application is available for requesting specific DataPoints by name and displaying their values.

```haskell
-- A simple dataPointTracer supporting the construction of a namespace.
mkDataPointTracer :: forall dp. (ToJSON dp, MetaTrace dp, NFData dp)
  => Trace IO DataPoint
  -> IO (Trace IO dp)
```

# Appendix

## References

The following document is periodically regenerated to provide comprehensive documentation for all trace messages, metrics, and data points within `cardano-node`. It also outlines the handling of these messages based on the current default configuration:

[Generated Cardano Trace Documentation](https://github.com/input-output-hk/cardano-node-wiki/wiki/tracers_doc_generated)

For a quick start for administrators transitioning to new the new tracing system, refer to the following document:

[New Tracing Quickstart](https://github.com/input-output-hk/cardano-node-wiki/wiki/New-Tracing-Quickstart)

Additionally, this document delves into `cardano-tracer`, a separate application designed for logging and monitoring Cardano nodes:

[Cardano Tracer](https://github.com/intersectmbo/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md)

## Future work

### Versioning

Versioning for trace messages stands as a crucial component that significantly contributes to the functionality and maintainability of our system. We acknowledge the importance of associating version numbers with log messages, ensuring transparency and consistency throughout the application lifecycle.

Adhering to a change protocol and establishing a clear correlation between node version numbers and trace version numbers is a prudent strategy. This approach aids in the effective management and communication of updates, alterations, and improvements to our tracing system. Such alignment guarantees that any modifications to the tracing system are accurately reflected and comprehended by both the development team and the broader Cardano community.

Anticipating the forthcoming development phase, we are eager to design and implement this versioning feature. Our goal is to seamlessly integrate it into our overall system architecture, bolstering our capacity to adapt and evolve. This ensures a clear, consistent, and structured approach to trace messages, enhancing our system's resilience and comprehensibility.

### Trace Consumers

We are excited to introduce the innovative concept of "trace consumers" into the Cardano Tracer system. This novel approach empowers trace consumers to register with the `cardano-tracer` application and selectively receive messages based on their subscriptions. We anticipate that this concept will significantly improve the efficiency and flexibility of our tracing system.

The introduction of trace consumers represents a robust and tailored approach to message retrieval, aligning seamlessly with the evolving needs of our network. This concept provides consumers with the ability to specify their message preferences, ensuring that they receive only the data directly relevant to their operations.

As part of this development initiative, we plan to phase out the use of data points. We believe that this evolution will render data points redundant in future versions of the tracing system. The transition to trace consumers aims to streamline our data retrieval processes, eliminating the need for unnecessary data points and offering a more sophisticated and focused mechanism for trace message consumption.
