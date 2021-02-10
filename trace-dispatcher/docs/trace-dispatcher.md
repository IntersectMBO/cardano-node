# trace-dispatcher: Towards an efficient and simple logging solution for Cardano

The current iohk-monitoring-framework should be replaced with something simpler. The current framework shall be replaced, because it consumes too much resources, and as such too much influences the system it is monitoring. As well the API should be simplified. And finally we are happy if we can offer a powerful and flexible solution for DevOps.

We call everything that shall be logged or monitored a __Message__ in this context, and are aware that this message can carry a payload, so it is not just a String. We say a __Trace__,  when we refer to the incoming side of a stream of messages. A __Trace__ receives messages, when the traceWith function gets called. We say a __Tracer__, when we refer to the outgoing side of a stream of messages. A __Tracer__ is doing something effect-full with the messages of some trace. So a Tracer is a backend that not only produces effects but also is always the end of the data flow. So you always have a trace as data source, which is then plumbed via transformers to one ore more tracers.

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

To actually log such a message the central traceWith function will be called:

```haskell
traceWith exampleTracer (IgnoreBlockOlderThanK b)
```

### Severity

Every message can have a severity, which is one of:

```haskell
DebugS | InfoS | NoticeS | WarningS | ErrorS | CriticalS | AlertS | EmergencyS

```

In addition it exist the a severity type for filtering, which has an additional `Silence` constructor, which follows to `Emergency`, so that nullTracers are not needed.

_Currently the severity is set by making TraceAddBlockEvent an instance of HasSeverityAnnotation. As well a WithSeverity type is used. In the new system the HasSeverityAnnotation typeclass and the WithSeverity wrapper type will be discontinued. Severity will be simply set by a function:_

```haskell
setSeverity :: Monad m => Severity -> Trace m a -> Trace m a
```  

You can use this function when you construct the tracer to set a standard severity:

```haskell
tracer = setSeverity NoticeS katipTracer
```  

Then you can specialize the severity when you actually call traceWith.

```haskell
traceWith (setSeverity WarningS exampleTracer) (IgnoreBlockOlderThanK b)
```

Or you can set severities for all messages of a type in a central place
in a style like it is done currently with the following function.

```haskell
withSeverity :: Monad m => (a -> Severity) -> Trace m a -> Trace m a
```
The first call of setSeverity or withSeverity wins. If no severity is given InfoS is the default value.

### Privacy

Every message can have a privacy, which is one of:

```haskell
Confidential | Public
```
The changes are just in sync with Severity, we get rid of the type class HasPrivacyAnnotation and use the interface:

```haskell
setPrivacy :: Monad m => Privacy -> Trace m a -> Trace m a
withPrivacy :: Monad m => (a -> Privacy) -> Trace m a -> Trace m a
```
The first call of setPrivacy or withPrivacy wins. Public is the default value.

### Namespace

Log items contain a named context or namespace, which should (uniquely?) identify them.
The interface remains the same, as this names are set with appendName.

```haskell
appendName :: Monad m => Text -> Trace m a -> Trace m a
```

Which when used like:

```haskell
appendName "out" $ appendName "middle" $ appendName "in" tracer
```
gives the name: `in.middle.out`. (In the current logs, usually the _Hostname_ is prepended and the _Severity_ and _ThreadId_ is appended to this name, so that the result looks like:

    [yupanqui-PC.in.middle.out.Info.379]
The standard Kapi representation of the same is:     

    [in.middle.out][Info][yupanqui-PC][ThreadId 379])

I like the second representation more, as it does not mix things which don't belong together, but the first is more compact.

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
    lcContext  :: [Text]
  , lcSeverity :: Maybe Severity
  , lcPrivacy  :: Maybe PrivacyAnnotation
}   
```

So you can e.g. write a filter function, which only displays _Public_ messages:

```haskell
filterTrace (\ (c, a) -> case lcPrivacy c of
                Just s  -> s == Public
                Nothing -> True)
```

We come back to filtering when we treat the _Configuration_, cause the configuration shall make it possible to enable a fine grained control of which messages are filtered out and which are further processed.

### Aggregation

Sometime it is desirable to aggregate information from multiple consecutive messages and pass this aggregated data further. As an example we want to log a measurement value together with the sum of all measurements that occurred so far.

For this we define a _Measure_ type to hold a Double, a _Stats_ type to hold the the sum together with the measurement and a function to calculate new Stats from old Stats and Measure:

```haskell
newtype Measure = MDouble Double

data Stats = Stats {
    sMeasure :: Double,
    sSum     :: Double
    }

calculate :: Stats -> Measure -> Stats
calculate Stats{..} (MDouble val) = BaseStats val (sSum + val)    
```

Then we can define the aggregation tracer with the procedure foldTraceM in the
following way, and it will output the Stats:

```haskell
  aggroTracer <- foldTraceM calculate (Stats 0.0 0.0) exTracer
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

Another variant of this function calls the function in a monadic context, so that you e.g. can get the current time:

```haskell
foldTraceM' :: forall a acc m . MonadIO m
  => (acc -> a -> m acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)
```

I would like to find a function foldTrace, that omits the ioref and can thus be called pure:

```haskell
foldTrace :: forall a acc m .
  => (acc -> a -> acc)
  -> acc
  -> Trace m (Folding a acc)
  -> Trace m a
```

### Identity of a message

Requirements:

1. Every message shall have a unique identity.
2. We want to be able to document this with ease.
3. Should make it possible for a human to locate the origin of a message.

It can be defined by:

1. Take constructor and type
2. Use the concatenated name (namespace)
3. (Add constructor and type to namespace)

### Plumbing

Traces shall be routed through different transformations to tracers.
Here we treat the static implementation of this. As we said before: A trace is data source, which is then plumbed via transformers to one ore more tracers. Calling traceWith trace passes the object through the transformers to zero to n tracers.

In the first example we route the trace to one of two tracers based on the constructor of the message.

```haskell
let resTrace = routingTrace routingf
  where
    routingf LO1 {} = tracer1
    routingf LO2 {} = tracer2
```    

To send the message of a trace to different tracers depending on some criteria use the following function:

```haskell
routingTrace :: forall m a . Monad m => (a -> Trace m a) -> Trace m a
```

In the second example we send the messages of one trace to two tracers simultaneously:

```haskell
let resTrace = tracer1 <> tracer2
```
To route one trace to multiple tracers simultaneously we use the fact that Tracer is an instance of Monoid and then use <>, which is also known as mappend:

```haskell
(<>) :: Monoid m => m -> m -> m
```

In the third example we unite two traces to one tracer, for which we trivially use the same tracer on the right side.

```haskell
tracer1  = appendName "tracer1" exTracer
tracer2  = appendName "tracer2" exTracer
```

### Formatting

We have different kinds of formatting, and options which influence formatting.

One basic choice is the format, which can be either a __human readable__ text representation or a __machine readable__ JSON representation.

This choice can be made by selecting different tracers, where one kind outputs text and the other JSON. So more details about this is given in the section about tracers.

For the JSON representation it is possible to give different levels of __DetailLevel__, which can result in less slots printed when verbosity is not at highest level, or shortened or missing versions of some printed values.

The system defines the detail levels:

```haskell
 DBrief | DRegular | DDetailed
```

### Tracers

For logging of text and JSON _Katip_ is used as backend library, and we will continue with this solution. Additional we have currently the possibilities to write _systemd_ logs on Linux, to use the _graylog_ logging system and to _forward traces_.

For logging of measurements _EKG_ is used as a library and we will keep this solution. Measurements are as well routed to _Prometheus_, this possibility shall be kept, but can be  implemented by forwarding the contents of EKG store.

<!-- Here is a list of all current backend kinds
data BackendKind =
AggregationBK
| EditorBK
| EKGViewBK
| GraylogBK
| KatipBK
| LogBufferBK
| MonitoringBK
| TraceAcceptorBK
| TraceForwarderBK
| UserDefinedBK Text
| SwitchboardBK
deriving (Eq,Ord,Show,Read) -->

#### Katip

Katip has its own and very different approach to logging. Specially it needs a LogEnv to function, which is usually provided by the class Katip m. We design a different interface, which constructs the LogEnv when constructing the tracer.

Katip makes use of the following Type classes. ToObject is used to serialize an object, and uses the ToJSON instance of Aeson as default.      

```haskell
class ToObject a where
    toObject :: a -> A.Object
    default toObject :: ToJSON a => a -> A.Object
    toObject v = case toJSON v of
      A.Object o -> o
      _          -> mempty
```

The LogItem type class is used to fix what output should be produced for different verbosity levels:

```haskell
class ToObject a => LogItem a where
    -- | List of keys in the JSON object that should be included in message.
    payloadKeys :: Verbosity -> a -> PayloadSelection

data PayloadSelection
    = AllKeys
    | SomeKeys [Text]
    deriving (Show, Eq)

```    
Shall we keep this type classes or just require _ToJson_ and set the payload selection with a function?   

```haskell
stdoutObjectKatipTracer :: (MonadIO m, LogItem a) => m (Trace m a)

stdoutJsonKatipTracer :: (MonadIO m, LogItem a) => m (Trace m a)
```

#### EKG

EKG has to phases: 1. A Counter, Gauge, Label or Distribution has to be registered for any value to be logged. 2. The actual value will be set. We can do the registration lazily when it first called. We have two variants depending if it gets initialized by a Store or a Server:

```haskell
ekgTracer :: MonadIO m => Metrics.Store -> m (Trace m Metric)

ekgTracer' :: MonadIO m => Server -> m (Trace m Metric)

data Metric = Integer Int | Double  | Label String
```
What types do we want to carry the Measurable data structure? EKG offers: Counter (simple incremental counter on integers), Gauge (integer valued), Label (displays strings) and Distribution (aggregates distribution for a double value).

As well the concept of a Group of metrics is offered. This may work with configuration items.

#### Forwarder

#### Stdout

#### Prometheus

### Configuration

The configuration can be read from a YAML or JSON file.

The system shall be reconfigurable. This can be done by changing the YAML file and then by calling reconfigureTracing in
the Node CLI.

The trace-dispatcher library code requires the allTraces function, which returns all traces which are used with traceWith.

```haskell
-- |
allTraces :: [Trace]

-- | The argument list all traces which are used with traceWith.
--   The function configures the traces with the given configuration
configureTraces :: MonadIO m => [Trace m a] -> Configuration -> m ()

-- Aternatively
-- | The function has to be called for all traces,
--   which are used with traceWith.
--   The function configures the traces with the given configuration
--   The returned trace has to be

configureTrace :: MonadIO m => Trace m a -> Configuration -> m (Trace m a)

```  

```haskell
-- *** General options:
data ConfigOption = ConfigOption {
    -- | Severity level for a message  
    coSeverity    :: SeverityF
    -- | Detail level for a message  
  , coDetailLevel :: DetailLevel
    -- | Defined in messages per second
  , coMaxFrequency :: Int
}

data TraceConfiguration = TraceConfiguration {
  ,  tcDefaultOptions :: ConfigOption

  ,  tcOptions :: Map Namespace ConfigOption

  -- *** Forwarder:
     -- Forward messages to the following address
  ,  tcForwardTo :: RemoteAddr

  -- *** Katip:
  ,  tcDefaultScribe :: ScribeDefinition

  ,  tcScribes :: Map TracerName -> ScribeDefinition

  -- *** EKG:
     -- | Port for EKG server
  ,  tcPortEKG :: Int

  -- *** Prometheus:
    -- Host/port to bind Prometheus server at
  ,  tcBindAddrPrometheus :: Maybe (String,Int)
}
```

The current system has the notion of subtrace. We will drop this concept.

### Multithreaded Tracing  

### Frequency Limiting (Eliding Tracers)

### Cardano Node Tracing
