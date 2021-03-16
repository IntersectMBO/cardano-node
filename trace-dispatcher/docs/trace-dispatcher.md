# trace-dispatcher: An efficient, simple and flexible logging library

The current iohk-monitoring-framework should be replaced with something simpler. The current framework shall be replaced, because it consumes too much resources, and as such too much influences the system it is monitoring.

We call everything that shall be logged or monitored a __Message__ in this context, and are aware that this message can carry a payload, so it is not just a String. We say __TraceIn__, when we refer to the incoming side of a stream of messages while we say __TraceOut__ when we refer to the outgoing side. The `traceWith` function is called on a TraceIn. A TraceOut is doing something effect-full with the messages. So a TraceOut is a backend that not only produces effects but also is the end of the data flow. We say __Trace__ for a stream of messages, that originates at some TraceIn, is then plumbed via transformers, which are implemented via contravariant functions, and end in one or more TraceOuts.

This library requires:

* the messages have to be instances of the typeclass `LogFormatting`
* the developer shall document all messages
* the documentation has to provide a default object for any message

This library offers:

* It can be reconfigured at runtime with one procedure call
* The tracers are optimized for efficiency at configuration time
* It can build documentation for all messages combined with it's parameters

This library is build upon the arrow based contravariant tracer library __contra-tracer__.

## General Interface

Logging and Monitoring is done in a typed fashion. The basic API consists of defining Tracers which are tracing the variants of a sum type:

```haskell
exampleTracer :: Trace IO (TraceAddBlockEvent blk)
```
In this example `TraceAddBlockEvent blk` is the sum type, which
is defined with the code, that should be traced:

```haskell
data TraceAddBlockEvent blk =
    IgnoreBlockOlderThanK (RealPoint blk)
  | IgnoreBlockAlreadyInVolatileDB (RealPoint blk)
  ...  
```

In this sum type every constructor defines a message with a potential payload to be logged or monitored.

To actually log such a message the __trace__ or __traceWith__ function must be called, where the trace
function gives a special name to this message in the context of the namespace:

```haskell
trace exampleTracer "ignoreBlock" (IgnoreBlockOlderThanK p)

traceWith exampleTracer (IgnoreBlockOlderThanK p)
```

### Severity

Every message can have a severity, which is one of:

```haskell
Debug | Info | Notice | Warning | Error | Critical | Alert | Emergency
```
(Remark: I would personally reduce the number of levels to 5: __Debug | Info | Warning | Error | Critical__ )

In addition it exist the a severity type for filtering, which has an additional `SilenceF` constructor, which follows to `EmergencyF`, so that nullTracers (TraceOut without effect) don't need to be used.

You use the following function to give a severity to messages:

```haskell
setSeverity :: Monad m => SeverityS -> Trace m a -> Trace m a
-- e.g.
tracer = setSeverity NoticeS exampleTracer
```

If in the message flow setSeverity gets called multiple times, the first one wins, which is the one closer to TraceIn. In the following code the severity is set, when you actually call traceWith for a single message.

```haskell
trace (setSeverity Warning exampleTracer) "ignoreBlock" (IgnoreBlockOlderThanK b)
```
Or you can set severities for all messages of a type in a central place:
in a style like it is done currently:

```haskell
withSeverity :: Monad m => (a -> SeverityS) -> Trace m a -> Trace m a
```  

If no severity is given Info is the default value.

### Privacy

Every message can have a privacy, which is one of:

```haskell
Confidential | Public
```
The mechanism is the same as with Severity, we use the interface:

```haskell
setPrivacy :: Monad m => Privacy -> Trace m a -> Trace m a
withPrivacy :: Monad m => (a -> Privacy) -> Trace m a -> Trace m a
```
The first call of setPrivacy or withPrivacy wins. Public is the default value.

### Detail Level

Furthermore it is possible to give different __DetailLevel__, which can result in less slots printed when the detail level is not at highest level, or shortened or missing versions of some printed values. We defines the detail levels:

```haskell
data DetailLevel = DBrief | DRegular | DDetailed
```
For setting this, the following functions can be used:

```haskell
setDetails :: Monad m => DetailLevel -> Trace m a -> Trace m a
withDetails :: Monad m => (a -> DetailLevel) -> Trace m a -> Trace m a
```

We explain in the section about formatting, how to specify wich detail levels display which information.

### Namespace

Messages contain a __Namespace__ which is a list of Text. *Every message at a TraceOut must be uniquely identified by its Namespace!* We could have used the constructor and type, which is a more technical approach, but this logging library uses the Namespace as message identifier.

The documentation will be ordered according to this scheme, and it should be possible for a human to locate the origin of a message in this way.

The interface uses the (already known) appendName function:

```haskell
appendName :: Monad m => Text -> Trace m a -> Trace m a
```

Which when used like:

```haskell
appendName "specific" $ appendName "middle" $ appendName "general" tracer
```
gives the name: `general.middle.specific`. Usually the application name is then prepended to the result by the formatter.
For the human formatter furthermore the _Hostname_ is prepended and the _Severity_ and _ThreadId_ is appended to this namespace.

    [deus-x-machina.cardano.general.middle.specific.Info.379]

Since we require that every message has its unique name we encourage the use of the already introduced convenience function:

```haskell
trace exampleTracer "ignoreBlock" (IgnoreBlockOlderThanK b)
-- instead of:
traceWith (appendName "ignoreBlock" exampleTracer) (IgnoreBlockOlderThanK b)
```

### Filtering

Not all messages shall be logged or monitored, but only a subset. The most common case is when in a production system you only want to see messages that have a minimum level of e.g. `Warning`.

This can be done by calling the function filterTraceBySeverity, which only processes messages further with a severity equal or greater as the given one. E.g.:

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
We come back to filtering when we treat the _Configuration_, cause the configuration makes it possible to enable a fine grained control of which messages are filtered out and which are further processed based on the Namespace.

### Plumbing

TraceIns shall be routed through different transformations to traceOuts. Calling *trace* passes the message through the transformers to zero to n traceOuts.
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
To route one trace to multiple tracers simultaneously we use the fact that Tracer is an instance of Monoid and then use <> (formerly known as mappend), or mconcat for lists of tracers:

```haskell
(<>) :: Monoid m => m -> m -> m
mconcat :: Monoid m => [m] -> m
```

In the third example we unite two traces to one tracer, for which we trivially use the same tracer on the right side.

```haskell
tracer1  = appendName "tracer1" exTracer
tracer2  = appendName "tracer2" exTracer
```

### Aggregation

Sometime it is desirable to aggregate information from multiple consecutive messages and pass this aggregated data further.

As an example we want to log a measurement value together with the sum of all measurements that occurred so far. For this we define a _Measure_ type to hold a Double, a _Stats_ type to hold the the sum together with the measurement and a function to calculate new Stats from old Stats and Measure:

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

The procedure foldTraceM uses an MVar to hold the state, and has thus to be called in a monad stack, which contains the IO Monad.

```haskell
-- | Folds the function with state b over messages a in the trace.
foldTraceM :: MonadIO m
  => (acc -> a -> acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)

newtype Folding a acc = Folding acc  
```

Another variant of this function calls the aggregation function in a monadic context, so that you e.g. can get the current time:

```haskell
foldMTraceM :: forall a acc m . MonadIO m
  => (acc -> a -> m acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)
```

(I would like to find a function foldTrace, that omits the MVar and can thus be called pure. Help is appreciated)

### Frequency Limiting

The current framework has the concept of eliding tracers, which have the
ability to suppress repeated messages when they are equal in some way. We hope to replace this concept with frequency limited tracers, which more or less randomly suppress messages when the number of messages exceeds a certain threshold, which can be specified. The argument for this concept is that it consumes less resources, as the storage and comparison between objects can be resource intensive.

The frequency limiter itself emits a message when its start to suppress messages, and reports if limiting stops together with the number of suppressed messages.

A limiter is given a name to identify its activity.

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

### Formatting

We want to limit the use type classes, so we only use one type class for the formatting of messages.

* The `forMachine` method is used for a machine readable representation, which can be different depending on the detail level.
  It's default implementation assumes no machine representation

* the `forHuman` method shall represent the message in human readable form.
  It's default implementation assumes no representation for humans   

* the `asMetrics` method shall represent the message as 0 to n metrics.  
  It's default implementation assumes no metrics. If a text is given it is
  appended as last element to the namespace.

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

### TraceOuts

We currently offer the following traceOuts (backends):

* StandardTracer (writes to stdout or a file)
  StandardTracer is used for for writing human and machine readable log files.

* EKG (for metrics)
  EKG is used for displaying measurements (Int and Double types). The contents of the EKG store can be forwarded by the ekg-forward package.

We will add

* Forwarding (forwards the messages to another process or machine).
  Since we want to get rid of any resource hungry computation in the node process, the most important backend in the actual node will be the Forwarding tracer, which forwards traces to a special logging process.  

Here is the interface to construct these tracers. The standard tracer may take a file path. It is disallowed to construct multiple tracers which writes to stdout or the same file. Doing this will result in crumbled output.

```haskell
standardTracer :: MonadIO m
  => Maybe FilePath
  -> m (Trace m FormattedMessage)

ekgTracer :: MonadIO m
  => Either Metrics.Store Server
  -> m (Trace m FormattedMessage)
```

### Configuration

With the new system of configuration we have:

1. Fine-grained configure options based on _namespaces_ up to individual messages  
2. Reconfigurable at any time with a call to _configureTracers_
3. Optimized, as configuration options are either fixed at configuration time if possible, but never require more then one lookup
4. Requires for any tracer of type a to get a list of prototypes for all messages and all entry points for this kind of object

This is implemented by running the trace network at configuration time.

```haskell
--   The function configures the traces with the given configuration
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
More configuration options e.g. for different transformers and traceOuts can be added by this mechanism.

### Documentation

This library requires that all messages of a type `a` are document by providing a `Documented a`, which consists of a list of `DocMsg`. A DocMsg is constructed by giving:

* a prototype of the message
* the most special name of the message in the namespace
* the documentation for the message in markdown format

*Because it is not enforced by the type system, it is very important to provide a complete list, as the prototypes are used as well for configuration*.

```haskell
newtype Documented a = Documented {undoc :: [DocMsg a]}

data DocMsg a = DocMsg {
    dmPrototype :: a
  , dmName      :: Text
  , dmMarkdown  :: Text
}
```

To build the documentation first call `documentMarkdown` with the Documented type and all the tracers that are called. Do this for all message types you need, and then call `buildersToText` with the appended lists.

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
>
>   Privacy:   `Public`
>
>   Details:   `DRegular`
>
>   Backends: `KatipBackend ""` / `Machine`, `KatipBackend ""` / `Human`
>
>   ***
>   Start of the leadership check
>
>   We record the current slot number.
>   ***
