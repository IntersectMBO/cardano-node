# trace-dispatcher: Towards an efficient and simple logging solution

The current iohk-monitoring-framework should be replaced with something simpler. The current framework shall be replaced, because it consumes too much resources, and as such too much influences the system it is monitoring. As well the API should be simplified. And finally we are happy if we can offer a powerful and flexible solution for DevOps and Stakeholders.

We call everything that shall be logged or monitored a __Message__ in this context, and are aware that this message can carry a payload, so it is not just a String. We say __TraceIn__, when we refer to the incoming side of a stream of messages while we say __TraceOut__ when we refer to the outgoing side. The traceWith function is called on a TraceIn. A TraceOut is doing something effect-full with the messages. So a TraceOut is a backend that not only produces effects but also is always the end of the data flow. We say __Trace__ for a stream of messages, that originates at some TraceIn, is then plumbed via transformers and ends in one or more TraceOuts.

## General Interface

Logging and Monitoring is done in a typed fashion. The basic API consists of defining Tracers which are tracing the variants of a sum type:

```haskell
exampleTracer :: Tracer IO (TraceAddBlockEvent blk)
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

To actually log such a message the important __traceWith__ function will be called:

```haskell
traceWith exampleTracer (IgnoreBlockOlderThanK b)
```

### Severity

Every message can have a severity, which is one of:

```haskell
DebugS | InfoS | NoticeS | WarningS | ErrorS | CriticalS | AlertS | EmergencyS

```

In addition it exist the a severity type for filtering, which has an additional `SilenceF` constructor, which follows to `EmergencyF`, so that nullTracers (TraceOut without effect) can be omitted.

You use the following function to give a severity to messages:

```haskell
setSeverity :: Monad m => Severity -> Trace m a -> Trace m a
-- e.g.
tracer = setSeverity NoticeS exampleTracer
```

If in the message flow setSeverity gets called multiple times, the first one wins, which is the one closer to TraceIn.
In the following code the severity is set, when you actually call traceWith for a single message.

```haskell
traceWith (setSeverity WarningS exampleTracer) (IgnoreBlockOlderThanK b)
```
Or you can set severities for all messages of a type in a central place:
in a style like it is done currently:

```haskell
withSeverity :: Monad m => (a -> Severity) -> Trace m a -> Trace m a
```  

We repeat that the first call of setSeverity or withSeverity wins. If no severity is given InfoS is the default value.

### Privacy

Every message can have a privacy, which is one of:

```haskell
Confidential | Public
```
The changes are just in sync with Severity, we get rid of the type class and use the interface:

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

We explain in the section about type classes, how to specify wich detail levels display which information.

### Namespace

Log items contain a namespace which is a concatenation of strings. Every message at a TraceOut should be uniquely identified by its namespace.

The documentation will be ordered according to this scheme. And is should be made possible for a human to locate the origin of a problem this way.

We could have used the constructor and type, which is a more technical approach,
and is currently used in the machine readable format, but the logging framework will not use it.

The interface remains the same, as names are set with appendName.

```haskell
appendName :: Monad m => Text -> Trace m a -> Trace m a
```

Which when used like:

```haskell
appendName "specific" $ appendName "middle" $ appendName "general" tracer
```
gives the name: `general.middle.specific`. (So maybe we should rename this function, cause the result looks more like prepend). Usually the application name is then prepended to the result. In the current logs furthermore the _Hostname_ is prepended and the _Severity_ and _ThreadId_ is appended to this namespace. We would like to change this, to make clear what the identifying name is, and to report this independently like:

    [yupanqui-PC.cardano.general.middle.specific.Info.379]
    ->
    [cardano.general.middle.specific][Info][yupanqui-PC][ThreadId 379])

Since we require that every message has its unique name we offer the following convenience function:
```haskell
traceWith (appendName "ignoreBlockOlderThanK" exampleTracer) (IgnoreBlockOlderThanK b)
-- Can be written as:
traceNamed exampleTracer "ignoreBlockOlderThanK" (IgnoreBlockOlderThanK b)
```

### Filtering

Not all messages shall be logged or monitored, but only a subset. The most common case is when in a production system you only want to see messages that have a minimum level of e.g. `Warning`.

This can be done by calling the function filterTraceBySeverity, which only processes messages further with a severity equal or greater as the given one. E.g.:

```haskell
let filteredTracer = filterTraceBySeverity WarningF exampleTracer
```
A more general filter function is offered, which gives access to the object and a `LoggingContext`, which contains the name, the severity and privacy:

```haskell
--- | Don't process further if the result of the selector function
---   is False.
filterTrace :: (Monad m) =>
     ((LoggingContext, a) -> Bool)
  -> Trace m a
  -> Trace m a

data LoggingContext = LoggingContext {
    lcContext     :: Namespace
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
We come back to filtering when we treat the _Configuration_, cause the configuration makes it possible to enable a fine grained control of which messages are filtered out and which are further processed based on the namespace.

### Plumbing

TraceIns shall be routed through different transformations to traceOuts.
Here we treat the static implementation of this. Calling traceWith trace passes the message through the transformers to zero to n traceOuts.
In the first example we route the trace to one of two tracers based on the constructor of the message.

```haskell
routingTrace :: forall m a . Monad m => (a -> Trace m a) -> Trace m a
let resTrace = routingTrace routingf (tracer1 <> tracer2)
  where
    routingf LO1 {} = tracer1
    routingf LO2 {} = tracer2
```    
The second argument must mappend all possible tracers of the first argument to one tracer. This is required for the configuration. We could have construct a more secure interface by having a map of values to tracers, but the ability for full pattern matching outweigh this disadvantage in our view.
To send the message of a trace to different tracers depending on some criteria use the following function:
In the second example we send the messages of one trace to two tracers simultaneously:

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

The procedure foldTraceM uses an IORef to hold the state, and has thus to be called in a monad stack, which contains the IO Monad.

```haskell
-- | Folds the function with state b over messages a in the trace.
foldTraceM :: forall a acc m . MonadIO m
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

(I would like to find a function foldTrace, that omits the Ioref and can thus be called pure. Help is appreciated)

### Frequency Limiting




### TraceOuts

We offer different traceOuts (backends):

* Forwarding
* Katip
* EKG
* Prometheus (via EKG)
* Stdout

Since we want to get rid of any resource hungry computation in the node process, the most important backend in the actual node will be the Forwarding tracer, which forwards traces to a special logging process. The only other possibility in the node will be a simple Stdout Tracer.  

Katip is used for for writing human and machine readable log files. One basic choice is between a __human readable__ text representation, or a __machine readable__ JSON representation. This choice is made by sending the message either to a Katip tracer, which is configured for a human or machine readable configuration or to both.  

EKG is used for displaying measurements (Int and Double types). The contents of the EKG store can be forwarded to Prometheus.

Backends will be named and can be configured by a matching configuration from a configuration file. We will offer a bunch of functions to construct these traceOuts:

```haskell
stdoutObjectKatipTracer :: (MonadIO m, LogItem a) => m (Trace m a)
stdoutJsonKatipTracer :: (MonadIO m, LogItem a) => m (Trace m a)
ekgTracer  :: MonadIO m => Metrics.Store -> m (Trace m Metric)
ekgTracer' :: MonadIO m => Server -> m (Trace m Metric)
```

### Typeclasses

We want to limit the use type classes to the use for representation of messages. So we combine the current classes ToObject and LogObject (which are coming from Katip) together with a method to query a potential measurement representation for EKG. As you can derive a generic aeson instance, you can as well derive a default Logging instance.

```haskell
class Logging a where
    toObject :: a -> A.Object
    default toObject :: ToJSON a => a -> A.Object
    toObject v = case A.toJSON v of
      A.Object o -> o
      _          -> mempty
    payloadKeys :: DetailLevel -> a -> PayloadSelection
    toMeasurement :: a -> Maybe Measurement
    toMeasurement _ = Nothing

data PayloadSelection
    = AllKeys
    | SomeKeys [Text]
    deriving (Show, Eq)      

data Measurement
    = IntM Int
    | DoubleM Double
    deriving (Show, Eq)      
```
The trace-dispatcher library will then care for the various instances that e.g. Katip or other backends require.

### Configuration

With the new system of configuration we have

1. fine-grained configure options based on namespaces (currently not for individual messages, but that can be changed).  
2. reconfigurable at any time with with a call to configureTracers
3. Optimized as config options are either fixed at configuration time if possible, but never require more then one simple lookup
4. Requires the allTraces function, which returns all traces which are used with traceWith

This is implemented by running the trace network at configuration time.
The configuration can be stored and read from a YAML or JSON file.

```haskell
-- | Needs to list all traces which are used with traceWith.
allTraces :: [Trace]

--   The function configures the traces with the given configuration
configureTraces :: MonadIO m => [Trace m a] -> Configuration -> m ()
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
Many more configuration options e.g. for different backends will be added.

### Documentation



### Cardano Node Tracing



![Cardano Tracing Overview](./trace-and-metric-lifecycle.pdf)
