{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

{-# OPTIONS_GHC -Wno-partial-fields  #-}

module Cardano.Logging.Types (
    Trace(..)
  , LogFormatting(..)
  , Metric(..)
  , getMetricName
  , emptyObject
  , Documented(..)
  , DocMsg(..)
  , LoggingContext(..)
  , emptyLoggingContext
  , Namespace(..)
  , nsReplacePrefix
  , nsReplaceInner
  , nsCast
  , nsPrependInner
  , nsGetComplete
  , nsGetTuple
  , nsRawToText
  , nsToText
  , MetaTrace(..)
  , DetailLevel(..)
  , Privacy(..)
  , SeverityS(..)
  , SeverityF(..)
  , ConfigOption(..)
  , ForwarderAddr(..)
  , FormatLogging(..)
  , ForwarderMode(..)
  , Verbosity(..)
  , TraceOptionForwarder(..)
  , defaultForwarder
  , ConfigReflection(..)
  , emptyConfigReflection
  , TraceConfig(..)
  , emptyTraceConfig
  , FormattedMessage(..)
  , TraceControl(..)
  , DocCollector(..)
  , LogDoc(..)
  , emptyLogDoc
  , BackendConfig(..)
  , Folding(..)
  , unfold
  , TraceObject(..)
  , PreFormatted(..)
  , HowToConnect(..)
) where

import           Codec.Serialise (Serialise (..))
import           Control.Applicative ((<|>))
import           Control.DeepSeq (NFData)
import qualified Control.Tracer as T
import qualified Data.Aeson as AE
import           Autodocodec.Schema (ObjectSchema)
import qualified Data.Aeson.Types as AE (Parser)
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Proxy (Proxy (..))
import           Data.Text as T (Text, breakOnEnd, intercalate, null, pack, singleton, unpack,
                   unsnoc, words)
import           Data.Text.Read as T (decimal)
import           Data.Time (UTCTime)
import           Data.Word (Word16)
import           GHC.Generics
import           Network.HostName (HostName)
import           Network.Socket (PortNumber)


-- | The Trace carries the underlying tracer Tracer from the contra-tracer package.
--   It adds a 'LoggingContext' and maybe a 'TraceControl' to every message.
newtype Trace m a = Trace
                            {unpackTrace :: T.Tracer m (LoggingContext, Either TraceControl a)}

-- | Contramap lifted to Trace
instance Monad m => T.Contravariant (Trace m) where
    contramap f (Trace tr) = Trace $
      T.contramap (\case
                      (lc, Right a) -> (lc, Right (f a))
                      (lc, Left tc) -> (lc, Left tc))
                  tr

-- | @tr1 <> tr2@ will run @tr1@ and then @tr2@ with the same input.
instance Monad m => Semigroup (Trace m a) where
  Trace a1 <> Trace a2 = Trace (a1 <> a2)

instance Monad m => Monoid (Trace m a) where
    mappend = (<>)
    mempty  = Trace T.nullTracer

-- | A unique identifier for every message, composed of text
-- A namespace can as well appear with the tracer name (e.g. "ChainDB.OpenEvent.OpenedDB"),
-- or more prefixes, in this moment it is a NamespaceOuter is used
data Namespace a = Namespace {
    nsPrefix :: [Text]
  , nsInner  :: [Text]}
  deriving stock Eq

instance Show (Namespace a) where
  show (Namespace [] []) = "emptyNS"
  show (Namespace [] nsInner') =
    unpack $ intercalate (singleton '.') nsInner'
  show (Namespace nsPrefix' nsInner') =
    unpack $ intercalate (singleton '.') (nsPrefix' ++ nsInner')

nsReplacePrefix :: [Text] -> Namespace a -> Namespace a
nsReplacePrefix o (Namespace _ i) =  Namespace o i

nsReplaceInner :: [Text] -> Namespace a -> Namespace a
nsReplaceInner i (Namespace o _) =  Namespace o i


nsPrependInner :: Text -> Namespace a -> Namespace b
nsPrependInner t (Namespace o i) =  Namespace o (t : i)

{-# INLINE nsCast #-}
nsCast :: Namespace a -> Namespace b
nsCast (Namespace o i) =  Namespace o i

nsGetComplete :: Namespace a -> [Text]
nsGetComplete (Namespace [] i) = i
nsGetComplete (Namespace o i)  = o ++ i

nsGetTuple :: Namespace a -> ([Text],[Text])
nsGetTuple (Namespace o i)  = (o,i)

nsRawToText :: ([Text], [Text]) -> Text
nsRawToText (ns1, ns2) = intercalate "." (ns1 ++ ns2)

nsToText :: Namespace a -> Text
nsToText (Namespace ns1 ns2) = intercalate "." (ns1 ++ ns2)

-- | Every message needs this to define how to represent itself
class LogFormatting a where
  -- | Machine readable representation with the possibility to represent with varying serialisations based on the detail level.
  -- This will result in JSON formatted log output.
  -- A `forMachine` implementation is required for any instance definition.
  forMachine :: DetailLevel -> a -> AE.Object

  -- | Human-readable representation.
  -- The empty text indicates there's no specific human-readable formatting for that type - this is the default implementation.
  -- If however human-readble output is explicitly requested, e.g. by logs, the system will fall back to a JSON object
  -- conforming to the `forMachine` definition, and rendering it as a value in `{"data": <value>}`.
  -- Leaving out `forHuman` in some instance definition will not lead to loss of log information that way.
  forHuman :: a -> Text
  forHuman _v = ""

  -- | Metrics representation.
  -- The default indicates that no metric is based on trace occurrences of that type.
  asMetrics :: a -> [Metric]
  asMetrics _v = []


class MetaTrace a where
  namespaceFor  :: a -> Namespace a

  severityFor   :: Namespace a -> Maybe a -> Maybe SeverityS
  privacyFor    :: Namespace a -> Maybe a -> Maybe Privacy
  privacyFor _  _ =  Just Public
  detailsFor    :: Namespace a -> Maybe a -> Maybe DetailLevel
  detailsFor _  _ =  Just DNormal

  documentFor   :: Namespace a -> Maybe Text
  metricsDocFor :: Namespace a -> [(Text,Text)]
  metricsDocFor _ = []
  schemaFor     :: Proxy a -> Maybe ObjectSchema
  schemaFor _   = Nothing
  allNamespaces :: [Namespace a]

data Metric
  -- | An integer metric.
  -- Text is used to name the metric
    = IntM Text Integer
  -- | A double metric.
  -- Text is used to name the metric
    | DoubleM Text Double
  -- | A counter metric.
  -- Text is used to name the metric
    | CounterM Text (Maybe Int)
  -- | A prometheus metric with key label pairs.
  -- Text is used to name the metric
  -- [(Text, Text)] is used to represent the key label pairs
  -- The value of the metric will always be "1"
  -- e.g. if you have a prometheus metric with the name "prometheus_metric"
  -- and the key label pairs [("key1", "value1"), ("key2", "value2")]
  -- the metric will be represented as "prometheus_metric{key1=\"value1\",key2=\"value2\"} 1"

    | PrometheusM Text [(Text, Text)]
  deriving stock (Eq, Show)


getMetricName :: Metric -> Text
getMetricName (IntM name _) = name
getMetricName (DoubleM name _) = name
getMetricName (CounterM name _) = name
getMetricName (PrometheusM name _) = name


-- | A helper function for creating an empty |Object|.
emptyObject :: HM.HashMap Text a
emptyObject = HM.empty

-- Document all log messages by providing a list of DocMsgs for all constructors.
-- Because it is not enforced by the type system, it is very
-- important to provide a complete list, as the prototypes are used as well for configuration.
-- If you don't want to add an item for documentation enter an empty text.
newtype Documented a = Documented {undoc :: [DocMsg a]}
  deriving stock Show
  deriving newtype Semigroup

-- | Document a message by giving a prototype, its most special name in the namespace
-- and a comment in markdown format
data DocMsg a = DocMsg {
    dmNamespace :: Namespace a
  , dmMetricsMD :: [(Text, Text)]
  , dmMarkdown  :: Text
}

instance Show (DocMsg a) where
  show (DocMsg _ _ md) = unpack md

-- | Context any log message carries
data LoggingContext = LoggingContext {
    lcNSInner   :: [Text]
  , lcNSPrefix  :: [Text]
  , lcSeverity  :: Maybe SeverityS
  , lcPrivacy   :: Maybe Privacy
  , lcDetails   :: Maybe DetailLevel
  }
  deriving stock
    (Show, Generic)
  deriving anyclass
    Serialise

emptyLoggingContext :: LoggingContext
emptyLoggingContext = LoggingContext [] [] Nothing Nothing Nothing

-- | Formerly known as verbosity
data DetailLevel =
      DMinimal
    | DNormal
    | DDetailed
    | DMaximum
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Serialise, AE.FromJSON)

instance AE.ToJSON DetailLevel where
    toEncoding = AE.genericToEncoding AE.defaultOptions

-- | Privacy of a message. Default is Public
data Privacy =
      Confidential              -- ^ confidential information - handle with care
    | Public                    -- ^ can be public.
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass Serialise

-- | Severity of a message
data SeverityS
    = Debug                   -- ^ Debug messages
    | Info                    -- ^ Information
    | Notice                  -- ^ Normal runtime Conditions
    | Warning                 -- ^ General Warnings
    | Error                   -- ^ General Errors
    | Critical                -- ^ Severe situations
    | Alert                   -- ^ Take immediate action
    | Emergency               -- ^ System is unusable
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (AE.ToJSON, AE.FromJSON, Serialise)

-- | Severity for a filter
-- Nothing means don't show anything (Silence)
-- Nothing level means show messages with severity >= level
newtype SeverityF = SeverityF (Maybe SeverityS)
  deriving stock Eq

instance Enum SeverityF where
  toEnum 8 = SeverityF Nothing
  toEnum i = SeverityF (Just (toEnum i))
  fromEnum (SeverityF Nothing)  = 8
  fromEnum (SeverityF (Just s)) = fromEnum s

instance AE.ToJSON SeverityF where
    toJSON (SeverityF (Just s)) = AE.String ((pack . show) s)
    toJSON (SeverityF Nothing)  = AE.String "Silence"

instance AE.FromJSON SeverityF where
    parseJSON (AE.String "Debug")     = pure (SeverityF (Just Debug))
    parseJSON (AE.String "Info")      = pure (SeverityF (Just Info))
    parseJSON (AE.String "Notice")    = pure (SeverityF (Just Notice))
    parseJSON (AE.String "Warning")   = pure (SeverityF (Just Warning))
    parseJSON (AE.String "Error")     = pure (SeverityF (Just Error))
    parseJSON (AE.String "Critical")  = pure (SeverityF (Just Critical))
    parseJSON (AE.String "Alert")     = pure (SeverityF (Just Alert))
    parseJSON (AE.String "Emergency") = pure (SeverityF (Just Emergency))
    parseJSON (AE.String "Silence")  = pure (SeverityF Nothing)
    parseJSON invalid = fail $ "Parsing of filter Severity failed."
                          <> "Unknown severity: " <> show invalid

instance Ord SeverityF where
  compare (SeverityF (Just s1)) (SeverityF (Just s2)) = compare s1 s2
  compare (SeverityF Nothing) (SeverityF Nothing)     = EQ
  compare (SeverityF (Just _s1)) (SeverityF Nothing)  = LT
  compare (SeverityF Nothing) (SeverityF (Just _s2))  = GT

instance Show SeverityF where
  show (SeverityF (Just s)) = show s
  show (SeverityF Nothing)  = "Silence"


----------------------------------------------------------------
-- Configuration

-- |
data ConfigReflection = ConfigReflection {
    crSilent          :: IORef (Set [Text])
  , crNoMetrics       :: IORef (Set [Text])
  , crAllTracers      :: IORef (Set [Text])
  }

emptyConfigReflection :: IO ConfigReflection
emptyConfigReflection  = do
    silence     <- newIORef Set.empty
    hasMetrics  <- newIORef Set.empty
    allTracers  <- newIORef Set.empty
    pure $ ConfigReflection silence hasMetrics allTracers

data FormattedMessage =
      FormattedHuman Bool Text
      -- ^ The bool specifies if the formatting includes colours
    | FormattedMachine Text
    | FormattedMetrics [Metric]
    | FormattedForwarder TraceObject
    | FormattedCBOR ByteString
  deriving stock (Eq, Show)


data PreFormatted = PreFormatted {
    pfTime              :: !UTCTime
  , pfNamespace         :: !Text
  , pfThreadId          :: !Text
  , pfForHuman          :: !(Maybe Text)
  , pfForMachineObject  :: AE.Object
}

-- | Used as interface object for ForwarderTracer
data TraceObject = TraceObject {
    toHuman     :: !(Maybe Text)
  , toMachine   :: !Text
  , toNamespace :: ![Text]
  , toSeverity  :: !SeverityS
  , toDetails   :: !DetailLevel
  , toTimestamp :: !UTCTime
  , toHostname  :: !Text
  , toThreadId  :: !Text
} deriving stock
    (Eq, Show, Generic)
  -- ^ Instances for 'TraceObject' to forward it using 'trace-forward' library.
  deriving anyclass
    (Serialise)

-- |
data BackendConfig =
    Forwarder
  | Stdout FormatLogging
  | EKGBackend
  | DatapointBackend
  | PrometheusSimple Bool (Maybe HostName) PortNumber   -- boolean: drop suffixes like "_int" in exposition; default: False
  deriving stock (Eq, Ord, Show, Generic)

instance AE.ToJSON BackendConfig where
  toJSON Forwarder  = AE.String "Forwarder"
  toJSON DatapointBackend = AE.String "DatapointBackend"
  toJSON EKGBackend = AE.String "EKGBackend"
  toJSON (Stdout f) = AE.String $ "Stdout " <> (pack . show) f
  toJSON (PrometheusSimple s h p) = AE.String $ "PrometheusSimple "
    <> bool mempty "nosuffix" s
    <> maybe mempty ((<> " ") . pack) h
    <> (pack . show) p

instance AE.FromJSON BackendConfig where
  parseJSON = AE.withText "BackendConfig" $ \case
    "Forwarder"                     -> pure Forwarder
    "EKGBackend"                    -> pure EKGBackend
    "DatapointBackend"              -> pure DatapointBackend
    "Stdout HumanFormatColoured"    -> pure $ Stdout HumanFormatColoured
    "Stdout HumanFormatUncoloured"  -> pure $ Stdout HumanFormatUncoloured
    "Stdout MachineFormat"          -> pure $ Stdout MachineFormat
    prometheus                      -> either fail pure (parsePrometheusString prometheus)

parsePrometheusString :: Text -> Either String BackendConfig
parsePrometheusString t = case T.words t of
  ["PrometheusSimple", portNo_] ->
    parsePort portNo_ >>= Right . PrometheusSimple False Nothing
  ["PrometheusSimple", arg, portNo_] ->
    parsePort portNo_ >>= Right . if validSuffix arg then PrometheusSimple (isNoSuffix arg) Nothing else PrometheusSimple False (Just $ unpack arg)
  ["PrometheusSimple", noSuff, host, portNo_]
    | validSuffix noSuff  -> parsePort portNo_ >>= Right . PrometheusSimple (isNoSuffix noSuff) (Just $ unpack host)
    | otherwise           -> Left $ "invalid modifier for PrometheusSimple: " ++ show noSuff
  _
    -> Left $ "unknown backend: " ++ show t
  where
    validSuffix s = s == "suffix" || s == "nosuffix"
    isNoSuffix    = (== "nosuffix")
    parsePort p = case T.decimal p of
      Right (portNo :: Word, rest)
        | T.null rest && 0 < portNo && portNo < 65536 -> Right $ fromIntegral portNo
      _                                               -> failure
      where failure = Left $ "invalid PrometheusSimple port: " ++ show p

data FormatLogging =
    HumanFormatColoured
  | HumanFormatUncoloured
  | MachineFormat
  deriving stock (Eq, Ord, Show)

-- Configuration options for individual namespace elements
data ConfigOption =
    -- | Severity level for a filter (default is Warning)
    ConfSeverity {severity :: SeverityF}
    -- | Detail level (default is DNormal)
  | ConfDetail {detail :: DetailLevel}
  -- | To which backend to pass
  --   Default is [EKGBackend, Forwarder, Stdout MachineFormat]
  | ConfBackend {backends :: [BackendConfig]}
  -- | Construct a limiter with limiting to the Double,
  -- which represents frequency in number of messages per second
  | ConfLimiter {maxFrequency :: Double}
  deriving stock (Eq, Ord, Show, Generic)

newtype ForwarderAddr
  = LocalSocket FilePath
  deriving stock (Eq, Ord, Show)

instance AE.FromJSON ForwarderAddr where
  parseJSON = AE.withObject "ForwarderAddr" $ \o -> LocalSocket <$> o AE..: "filePath"

data ForwarderMode =
    -- | Forwarder works as a client: it initiates network connection with
    -- 'cardano-tracer' and/or another Haskell acceptor application.
    Initiator
    -- | Forwarder works as a server: it accepts network connection from
    -- 'cardano-tracer' and/or another Haskell acceptor application.
  | Responder
  deriving stock (Eq, Ord, Show, Generic)

data Verbosity =
    -- | Maximum verbosity for all tracers in the forwarding protocols.
    Maximum
    -- | Minimum verbosity, the forwarding will work as silently as possible.
  | Minimum
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass AE.ToJSON

instance AE.FromJSON Verbosity where
  parseJSON (AE.String "Maximum") = pure Maximum
  parseJSON (AE.String "Minimum") = pure Minimum
  parseJSON other                 = fail $ "Parsing of Verbosity failed."
                                    <> "Unknown Verbosity: " <> show other

data TraceOptionForwarder = TraceOptionForwarder {
    tofQueueSize           :: Word
  , tofVerbosity           :: Verbosity
  , tofMaxReconnectDelay   :: Word
} deriving stock (Eq, Ord, Show, Generic)

-- A word regarding queue size:
--
-- In case of a missing forwarding service consumer, traces messages will be
-- buffered. This mitigates short forwarding interruptions, or delays at startup
-- time.
--
-- The queue capacity should thus correlate to the expected log lines per second
-- given a particular tracing configuration - to avoid unnecessarily increasing
-- memory footprint.
--
-- The default values here are chosen to accomodate verbose tracing output
-- (i.e., buffering 1min worth of trace data given ~32 messages per second). A
-- config that results in less than 5 msgs per second should also provide
-- `TraceOptionForwarder` a queue size value considerably lower.
--
-- The queue size ties in with the max number of trace objects cardano-tracer
-- requests periodically, the default for that being 100. Here, the queue can
-- hold enough traces for 10 subsequent polls by cardano-tracer.
instance AE.FromJSON TraceOptionForwarder where
    parseJSON = AE.withObject "TraceOptionForwarder" $ \obj -> do
      -- Field "queueSize" is the new field that replaces and unifies
      -- both "connQueueSize" and "disconnQueueSize".
      maybeQueueSize <- obj AE..:? "queueSize"
      queueSize <- case maybeQueueSize of
                     -- If the new field was provided we use it.
                     (Just qs) -> return qs
                     -- Else we look for the deprectaed fields.
                     Nothing   -> do
                       connQueueSize    <- obj AE..:? "connQueueSize"    AE..!= 128
                       disconnQueueSize <- obj AE..:? "disconnQueueSize" AE..!= 192
                       return $ max connQueueSize disconnQueueSize
      verbosity         <- obj AE..:? "verbosity"         AE..!= Minimum
      maxReconnectDelay <- obj AE..:? "maxReconnectDelay" AE..!= 45
      return $ TraceOptionForwarder queueSize verbosity maxReconnectDelay

instance AE.ToJSON TraceOptionForwarder where
  toJSON TraceOptionForwarder{..} = AE.object
    [
      "queueSize"         AE..= tofQueueSize,
      "verbosity"         AE..= tofVerbosity,
      "maxReconnectDelay" AE..= tofMaxReconnectDelay
    ]

defaultForwarder :: TraceOptionForwarder
defaultForwarder = TraceOptionForwarder {
    tofQueueSize           = 192
  , tofVerbosity           = Minimum
  , tofMaxReconnectDelay   = 45
}

instance AE.FromJSON ForwarderMode where
  parseJSON (AE.String "Initiator") = pure Initiator
  parseJSON (AE.String "Responder") = pure Responder
  parseJSON other                   = fail $ "Parsing of ForwarderMode failed."
                        <> "Unknown ForwarderMode: " <> show other

data TraceConfig = TraceConfig {
     -- | Options specific to a certain namespace
    tcOptions   :: Map.Map [Text] [ConfigOption]
     -- | Options for the forwarder
  , tcForwarder :: Maybe TraceOptionForwarder
    -- | Optional human-readable name of the node.
  , tcNodeName  :: Maybe Text
    -- | Optional prefix for metrics.
  , tcMetricsPrefix :: Maybe Text
    -- | Optional resource trace frequency in milliseconds.
  , tcResourceFrequency :: Maybe Int
    -- | Optional ledger metrics frequency in milliseconds.
  , tcLedgerMetricsFrequency :: Maybe Int
}
  deriving stock (Eq, Ord, Show)

emptyTraceConfig :: TraceConfig
emptyTraceConfig = TraceConfig {
    tcOptions = Map.empty
  , tcForwarder = Nothing
  , tcNodeName = Nothing
  , tcMetricsPrefix = Nothing
  , tcResourceFrequency = Just 5000 -- Every five seconds
  , tcLedgerMetricsFrequency = Just 1 -- Every slot
  }

---------------------------------------------------------------------------
-- Control and Documentation

-- | When configuring a net of tracers, it should be run with Config on all
-- entry points first, and then with TCOptimize. When reconfiguring it needs to
-- run TCReset followed by Config followed by TCOptimize
data TraceControl where
    TCReset       :: TraceControl
    TCConfig      :: TraceConfig -> TraceControl
    TCOptimize    :: ConfigReflection -> TraceControl
    TCDocument    :: Int -> DocCollector -> TraceControl

newtype DocCollector = DocCollector (IORef (Map Int LogDoc))

data LogDoc = LogDoc {
    ldDoc             :: !Text
  , ldMetricsDoc      :: !(Map.Map Text Text)
  , ldNamespace       :: ![([Text],[Text])]
  , ldSeverityCoded   :: !(Maybe SeverityS)
  , ldPrivacyCoded    :: !(Maybe Privacy)
  , ldDetailsCoded    :: !(Maybe DetailLevel)
  , ldDetails         :: ![DetailLevel]
  , ldBackends        :: ![BackendConfig]
  , ldFiltered        :: ![SeverityF]
  , ldLimiter         :: ![(Text, Double)]
  , ldSchema          :: !(Maybe ObjectSchema)
  , ldSilent          :: Bool
} deriving stock (Eq, Show)

emptyLogDoc :: Text -> [(Text, Text)] -> LogDoc
emptyLogDoc d m = LogDoc d (Map.fromList m) [] Nothing Nothing Nothing [] [] [] [] Nothing False

-- | Type for the function foldTraceM from module Cardano/Logging/Trace
newtype Folding a b = Folding b

unfold :: Folding a b -> b
unfold (Folding b) = b

instance LogFormatting b => LogFormatting (Folding a b) where
  forMachine v (Folding b) =  forMachine v b
  forHuman (Folding b)     =  forHuman b
  asMetrics (Folding b)    =  asMetrics b

-- | Specifies how to connect to the peer.
--
-- Taken from ekg-forward:System.Metrics.Configuration, to avoid dependency.
type Host :: Type
type Host = Text

type Port :: Type
type Port = Word16

type HowToConnect :: Type
data HowToConnect
  = LocalPipe    !FilePath    -- ^ Local pipe (UNIX or Windows).
  | RemoteSocket !Host !Port  -- ^ Remote socket (host and port).
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

instance Show HowToConnect where
  show = \case
    LocalPipe pipe         -> pipe
    RemoteSocket host port -> T.unpack host ++ ":" ++ show port

instance AE.ToJSON HowToConnect where
  toJSON     = AE.toJSON . show
  toEncoding = AE.toEncoding . show

-- first try to host:port, and if that fails revert to parsing any
-- string literal and assume it is a localpipe.
instance AE.FromJSON HowToConnect where
  parseJSON = AE.withText "HowToConnect" $ \t ->
        (uncurry RemoteSocket <$> parseHostPort t)
    <|> (        LocalPipe    <$> parseLocalPipe t)

parseLocalPipe :: Text -> AE.Parser FilePath
parseLocalPipe t
  | T.null t = fail "parseLocalPipe: empty Text"
  | otherwise   = pure $ T.unpack t

parseHostPort :: Text -> AE.Parser (Text, Word16)
parseHostPort t
  | T.null t
  = fail "parseHostPort: empty Text"
  | otherwise
  = let
    (host_, portText) = T.breakOnEnd ":" t
    host              = maybe "" fst (T.unsnoc host_)
  in if
    | T.null host      -> fail "parseHostPort: Empty host or no colon found."
    | T.null portText  -> fail "parseHostPort: Empty port."
    | Right (port, remainder) <- T.decimal portText
    , T.null remainder
    , 0 <= port, port <= 65535 -> pure (host, port)
    | otherwise -> fail "parseHostPort: Non-numeric port or value out of range."
