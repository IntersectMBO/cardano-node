{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use any" -}

module Cardano.Tracer.Configuration
  ( Address
  , AlarmFilterConfig (..)
  , AlarmsAuthConfig (..)
  , AlarmsConfig (..)
  , AlarmsConsumerConfig (..)
  , AlarmsLimitsConfig (..)
  , AlarmsRetentionConfig (..)
  , AlarmsTimeseriesRuleConfig (..)
  , AlarmsTraceRuleConfig (..)
  , Certificate (..)
  , Net.HowToConnect (..)
  , Endpoint (..)
  , setEndpoint
  , FileOrMap (..)
  , LogFormat (..)
  , LogMode (..)
  , LoggingParams (..)
  , Network (..)
  , ProducerCredentialConfig (..)
  , ReaderCredentialConfig (..)
  , RotationParams (..)
  , TracerConfig (..)
  , Verbosity (..)
  , alarmSeverityToText
  , parseAlarmSeverityText
  , readTracerConfig
  ) where

import           Cardano.Logging.Types (HowToConnect, SeverityS (..))
import qualified Cardano.Logging.Types as Log
import qualified Cardano.Logging.Types as Net

import           Control.Applicative ((<|>))
import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import           Data.Fixed (Pico)
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Data.Kind (Type)
import           Data.List (intercalate, nub)
import           Data.List.Extra (notNull)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word16, Word32, Word64)
import           Data.Yaml (decodeFileEither)
import           GHC.Generics (Generic)
import           Network.Wai.Handler.Warp (HostPreference, Port, Settings, setHost, setPort)
import           System.Exit (die)

type Address :: Type
type Address = HowToConnect

-- | Endpoint for internal services.
data Endpoint = Endpoint
  { epHost     :: !String
  , epPort     :: !Port
  , epForceSSL :: !(Maybe Bool)
  -- ^ `Nothing' (absent field) and `Just False' (`False' field) both
  -- disable SSL.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Certificate = Certificate
  { certificateFile    :: !FilePath
  , certificateKeyFile :: !FilePath
  , certificateChain   :: !(Maybe [FilePath])
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Endpoint {host, port} acting on Settings: setting host and port.
setEndpoint :: Endpoint -> (Settings -> Settings)
setEndpoint Endpoint{epHost, epPort} settings = settings
  & setPort            (epPort :: Port)
  & setHost (fromString epHost :: HostPreference)

-- | Parameters of rotation mechanism for logs.
data RotationParams = RotationParams
  { rpFrequencySecs :: !Word32  -- ^ Rotation period, in seconds.
  , rpLogLimitBytes :: !Word64  -- ^ Max size of log file in bytes.
  , rpMaxAgeMinutes :: !Word64  -- ^ Max age of log file in minutes.
  , rpKeepFilesNum  :: !Word32  -- ^ Number of log files to keep in any case.
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass ToJSON

-- | Some fields are defaulted:
--
-- `rpFrequencySecs` defaults to 1 minute.
--
-- Max age for `RotationParams` can be specified in `rpMaxAgeMinutes`
-- or `rpMaxAgeHours`: otherwise defaulting to 24 hours.
instance FromJSON RotationParams where
  parseJSON = withObject "RotationParams" \o -> do
    rpFrequencySecs <- o .: "rpFrequencySecs"
                   <|> pure 60
    rpLogLimitBytes <- o .: "rpLogLimitBytes"
    rpMaxAgeMinutes <- o .: "rpMaxAgeMinutes"
                   <|> (o .: "rpMaxAgeHours" <&> (* 60))
                   <|> pure (24 * 60)
    rpKeepFilesNum  <- o .: "rpKeepFilesNum"
    pure RotationParams{..}

-- | A conjunctive selection over alarm events, used both for a static
--   consumer's configuration and for a reader credential's allowed
--   (ceiling) or requested filter.
data AlarmFilterConfig = AlarmFilterConfig
  { afcSource      :: !(Maybe Text)
  , afcRuleId      :: !(Maybe Text)
  , afcMinSeverity :: !(Maybe SeverityS)
  , afcScope       :: !(Maybe (Map Text Text))
  , afcLabels      :: !(Maybe (Map Text Text))
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON AlarmFilterConfig where
  parseJSON = withObject "AlarmFilterConfig" \o -> do
    afcSource          <- o .:? "source"
    afcRuleId          <- o .:? "ruleId"
    afcMinSeverityText <- o .:? "minSeverity"
    afcMinSeverity     <- traverse parseSeverityOrFail afcMinSeverityText
    afcScope           <- o .:? "scope"
    afcLabels          <- o .:? "labels"
    pure AlarmFilterConfig{..}
   where
    parseSeverityOrFail t =
      maybe (fail ("unknown severity: " <> Text.unpack t)) pure (parseAlarmSeverityText t)

instance ToJSON AlarmFilterConfig where
  toJSON AlarmFilterConfig{..} = object $ catMaybes
    [ ("source" .=) <$> afcSource
    , ("ruleId" .=) <$> afcRuleId
    , ("minSeverity" .=) . alarmSeverityToText <$> afcMinSeverity
    , ("scope" .=) <$> afcScope
    , ("labels" .=) <$> afcLabels
    ]

-- | A statically configured alarm dispatch target. The only variant
--   implemented so far is @log@; @webhook@/@email@ are the obvious follow-up
--   extension (see @Cardano.Tracer.Handlers.Alarms.Consumers@).
data AlarmsConsumerConfig = AlarmsConsumerLog
  { aclName    :: !Text
  , aclEnabled :: !Bool
  , aclFilter  :: !(Maybe AlarmFilterConfig)
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON AlarmsConsumerConfig where
  parseJSON = withObject "AlarmsConsumerConfig" \o -> do
    consumerType :: Text <- o .: "type"
    case consumerType of
      "log" -> AlarmsConsumerLog
                 <$> o .: "name"
                 <*> (fromMaybe True <$> o .:? "enabled")
                 <*> o .:? "filter"
      other -> fail ("unknown alarm consumer type: " <> Text.unpack other)

instance ToJSON AlarmsConsumerConfig where
  toJSON AlarmsConsumerLog{..} = object
    [ "type"    .= ("log" :: Text)
    , "name"    .= aclName
    , "enabled" .= aclEnabled
    , "filter"  .= aclFilter
    ]

-- | A producer credential: a bearer token (read once from @pcTokenFile@) that
--   authenticates alarm ingress requests as coming from @pcSource@. The
--   source is never taken from the request body itself.
data ProducerCredentialConfig = ProducerCredentialConfig
  { pcName      :: !Text
  , pcTokenFile :: !FilePath
  , pcSource    :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A reader credential: a bearer token that grants history access, capped
--   by an allowed (ceiling) filter that a caller's requested filter may
--   only narrow.
data ReaderCredentialConfig = ReaderCredentialConfig
  { rcName         :: !Text
  , rcTokenFile    :: !FilePath
  , rcAllowHistory :: !(Maybe Bool)
  , rcFilter       :: !(Maybe AlarmFilterConfig)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AlarmsAuthConfig = AlarmsAuthConfig
  { aacProducers :: ![ProducerCredentialConfig]
  , aacReaders   :: ![ReaderCredentialConfig]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AlarmsRetentionConfig = AlarmsRetentionConfig
  { arcMaxAgeSeconds :: !(Maybe Word64)
  , arcMaxEvents     :: !(Maybe Word64)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AlarmsLimitsConfig = AlarmsLimitsConfig
  { alcMaxEventBytes :: !(Maybe Word64)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A rule that raises an alarm for every received trace message whose
--   severity is at or above @threshold@. @suppressForSecs@ (default 300)
--   bounds the alarm frequency: within one window, repeated matches from the
--   same node and namespace collapse into a single alarm via the store's
--   idempotency key (see @Cardano.Tracer.Handlers.Alarms.TraceRules@).
data AlarmsTraceRuleConfig = AlarmsTraceRuleConfig
  { atrRuleId          :: !Text
  , atrSummary         :: !(Maybe Text)
  , atrThreshold       :: !SeverityS
  , atrSuppressForSecs :: !(Maybe Word64)
  , atrLabels          :: !(Maybe (Map Text Text))
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON AlarmsTraceRuleConfig where
  parseJSON = withObject "AlarmsTraceRuleConfig" \o -> do
    atrRuleId          <- o .: "ruleId"
    atrSummary         <- o .:? "summary"
    thresholdText      <- o .: "threshold"
    atrThreshold       <- parseSeverityOrFail thresholdText
    atrSuppressForSecs <- o .:? "suppressForSecs"
    atrLabels          <- o .:? "labels"
    pure AlarmsTraceRuleConfig{..}
   where
    parseSeverityOrFail t =
      maybe (fail ("unknown severity: " <> Text.unpack t)) pure (parseAlarmSeverityText t)

instance ToJSON AlarmsTraceRuleConfig where
  toJSON AlarmsTraceRuleConfig{..} = object $
    [ "ruleId"    .= atrRuleId
    , "threshold" .= alarmSeverityToText atrThreshold
    ] <> catMaybes
    [ ("summary" .=)         <$> atrSummary
    , ("suppressForSecs" .=) <$> atrSuppressForSecs
    , ("labels" .=)          <$> atrLabels
    ]

-- | A rule that periodically evaluates a @cardano-timeseries-io@ query and
--   raises an alarm when the query returns 'Truth' (or a truthy
--   'InstantVector' entry) for at least @for@ seconds. The @sourceEventId@
--   embeds the rule id and a canonical series key from the sample's labels
--   so per-series edges are deduplicated by the store.
data AlarmsTimeseriesRuleConfig = AlarmsTimeseriesRuleConfig
  { atsRuleId        :: !Text
  , atsSummary       :: !(Maybe Text)
  , atsSeverity      :: !SeverityS
  , atsQuery         :: !Text
  , atsEvaluateEvery :: !Word64                 -- ^ seconds between evaluations
  , atsFor           :: !(Maybe Word64)         -- ^ seconds the sample must stay truthy before publishing
  , atsRepeatEvery   :: !(Maybe Word64)         -- ^ seconds between reminder alarms while still true
  , atsLabels        :: !(Maybe (Map Text Text))
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON AlarmsTimeseriesRuleConfig where
  parseJSON = withObject "AlarmsTimeseriesRuleConfig" \o -> do
    atsRuleId        <- o .: "ruleId"
    atsSummary       <- o .:? "summary"
    severityText     <- o .: "severity"
    atsSeverity      <- maybe (fail ("unknown severity: " <> Text.unpack severityText)) pure
                              (parseAlarmSeverityText severityText)
    atsQuery         <- o .: "query"
    atsEvaluateEvery <- o .: "evaluateEvery"
    atsFor           <- o .:? "for"
    atsRepeatEvery   <- o .:? "repeatEvery"
    atsLabels        <- o .:? "labels"
    pure AlarmsTimeseriesRuleConfig{..}

instance ToJSON AlarmsTimeseriesRuleConfig where
  toJSON AlarmsTimeseriesRuleConfig{..} = object $
    [ "ruleId"        .= atsRuleId
    , "severity"      .= alarmSeverityToText atsSeverity
    , "query"         .= atsQuery
    , "evaluateEvery" .= atsEvaluateEvery
    ] <> catMaybes
    [ ("summary" .=)     <$> atsSummary
    , ("for" .=)         <$> atsFor
    , ("repeatEvery" .=) <$> atsRepeatEvery
    , ("labels" .=)      <$> atsLabels
    ]

-- | Configuration for the alarm subsystem (see
--   @cardano-tracer/docs/alarm-system-concept.md@). @Nothing@ for the
--   enclosing 'Maybe' in 'TracerConfig' is the on/off switch for the whole
--   subsystem; every field below is only meaningful once it's turned on.
data AlarmsConfig = AlarmsConfig
  { alEndpoint        :: !Endpoint
  , alAllowInsecure   :: !(Maybe Bool)
  , alRetention       :: !(Maybe AlarmsRetentionConfig)
  , alLimits          :: !(Maybe AlarmsLimitsConfig)
  , alAuthentication  :: !AlarmsAuthConfig
  , alConsumers       :: ![AlarmsConsumerConfig]
  , alTraceRules      :: !(Maybe [AlarmsTraceRuleConfig])
  , alTimeseriesRules :: !(Maybe [AlarmsTimeseriesRuleConfig])
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The alarm envelope's wire form uses lowercase severity names (e.g.
--   @"critical"@), unlike 'SeverityS'\'s own derived 'FromJSON'\/'ToJSON'
--   instances (from @trace-dispatcher@), which serialise the capitalised
--   constructor name (@"Critical"@). Every place a severity crosses the
--   alarm system's wire boundary -- the 'AlarmEvent' envelope, YAML
--   'AlarmFilterConfig', and the @minSeverity=@ query parameter -- must go
--   through these two functions instead of 'SeverityS'\'s own instances, or
--   producers/consumers will silently disagree on casing.
alarmSeverityToText :: SeverityS -> Text
alarmSeverityToText = Text.toLower . Text.pack . show

parseAlarmSeverityText :: Text -> Maybe SeverityS
parseAlarmSeverityText t =
  lookup (Text.toLower t) [(alarmSeverityToText s, s) | s <- [minBound .. maxBound]]

-- | Logging mode.
data LogMode
  = FileMode    -- ^ Store items in log file.
  | JournalMode -- ^ Store items in Linux journal service.
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Format of log files.
data LogFormat
  = ForHuman   -- ^ For human (text)
  | ForMachine -- ^ For machine (JSON)
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Logging parameters.
data LoggingParams = LoggingParams
  { logRoot   :: !FilePath  -- ^ Root directory where all subdirs with logs are created.
  , logMode   :: !LogMode   -- ^ Log mode.
  , logFormat :: !LogFormat -- ^ Log format.
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Connection mode.
data Network
  = AcceptAt  !Address            -- ^ Server mode: accepts connections.
  | ConnectTo !(NonEmpty Address) -- ^ Client mode: initiates connections.
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Tracer's verbosity.
data Verbosity
  = Minimum    -- ^ Display minimum of messages.
  | ErrorsOnly -- ^ Display errors only.
  | Maximum    -- ^ Display all the messages (protocols tracing, errors).
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype FileOrMap = FOM (Either FilePath (Map Text Text))
  deriving stock (Eq, Show)

instance ToJSON FileOrMap where
  toJSON      (FOM fom) = either toJSON toJSON fom
  toEncoding  (FOM fom) = either toEncoding toEncoding fom

instance FromJSON FileOrMap where
  parseJSON v =
    (FOM . Left <$> parseJSON v) <|> (FOM . Right <$> parseJSON v)

-- | Tracer configuration.
data TracerConfig = TracerConfig
  { networkMagic     :: !Word32                       -- ^ Network magic from genesis the node is launched with.
  , network          :: !Network                      -- ^ How cardano-tracer will be connected to node(s).
  , loRequestNum     :: !(Maybe Word16)               -- ^ How many 'TraceObject's will be asked in each request.
  , ekgRequestFreq   :: !(Maybe Pico)                 -- ^ How often to request for EKG-metrics, in seconds.
  , hasEKG           :: !(Maybe Endpoint)             -- ^ Endpoint for EKG web-page.
  , hasPrometheus    :: !(Maybe Endpoint)             -- ^ Endpoint for Prometheus web-page.
  , hasTimeseries    :: !(Maybe Endpoint)
  , alarms           :: !(Maybe AlarmsConfig)      -- ^ Alarm subsystem configuration; 'Nothing' disables it.
  , tlsCertificate   :: !(Maybe Certificate)
    -- | Socket for tracer's to reforward on. Second member of the triplet is the list of prefixes to reforward.
    -- Third member of the triplet is the forwarder config.
  , hasForwarding  :: !(Maybe ( Network
                              , Maybe [[Text]]
                              , Log.TraceOptionForwarder
                              ))
  , logging           :: !(NonEmpty LoggingParams)  -- ^ Logging parameters.
  , rotation          :: !(Maybe RotationParams)    -- ^ Rotation parameters.
  , verbosity         :: !(Maybe Verbosity)         -- ^ Verbosity of the tracer itself.
  , metricsNoSuffix   :: !(Maybe Bool)              -- ^ Prometheus ONLY: Dropping metrics name suffixes (like "_int") increases similarity with old system names - if desired; default: False
  , metricsHelp       :: !(Maybe FileOrMap)         -- ^ Prometheus ONLY: JSON file or object containing a key-value map "metric name -> help text" for "# HELP " annotations
  , resourceFreq      :: !(Maybe Int)               -- ^ Frequency (1/millisecond) for gathering resource data.
  , ekgRequestFull    :: !(Maybe Bool)              -- ^ Request full set of metrics always, vs. deltas only (safer, but more overhead); default: False
  , prometheusLabels  :: !(Maybe (Map Text Text))   -- ^ A common label set for all Prometheus scrape targets (only used in Prometheus HTTP service discovery)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Read the tracer's configuration file.
readTracerConfig :: FilePath -> IO TracerConfig
readTracerConfig pathToConfig =
  decodeFileEither @TracerConfig pathToConfig >>= \case
    Left e -> die $ "Invalid tracer's configuration: " <> show e
    Right (config :: TracerConfig) ->
      case wellFormed config of
        Left problems -> die $ "Tracer's configuration is ill-formed: " <> problems
        Right{} -> return (nubLogging config)

  where
  -- Remove duplicate logging parameters.
  nubLogging :: TracerConfig -> TracerConfig
  nubLogging tracerConfig@TracerConfig{logging} = tracerConfig
    { logging = NE.nub logging
    }

wellFormed :: TracerConfig -> Either String ()
wellFormed TracerConfig
  { network
  , hasEKG
  , hasPrometheus
  , hasTimeseries
  , alarms
  , logging
  } =
  if null problems
    then Right ()
    else Left $ intercalate ", " problems
 where
  problems :: [String]
  problems = catMaybes
    [ case network of
        AcceptAt addr -> check "AcceptAt is empty" $ nullAddress addr
        ConnectTo addrs -> check "ConnectTo are empty" $ null (NE.filter (not . nullAddress) addrs)
    , check "empty logRoot(s)" $ notNull (NE.filter invalidFileMode logging)
    , check "duplicate ports in config" $ hasDuplicates ports
    , check "no host(s) in hasEKG"     . nullEndpoint =<< hasEKG
    , check "no host in hasPrometheus" . nullEndpoint =<< hasPrometheus
    , check "no host in hasTimeseries" . nullEndpoint =<< hasTimeseries
    , check "no host in alarms endpoint" . nullEndpoint . alEndpoint =<< alarms
    , check "alarms: no producer or reader credentials configured" . noCredentials =<< alarms
    , check "alarms: duplicate consumer names" . hasDuplicateConsumerNames =<< alarms
    ]

  -- NB. every internal service's endpoint port is included here, including
  -- 'hasTimeseries' and 'alarms' (the former was previously missing from
  -- this check).
  ports :: [Port]
  ports = epPort <$> catMaybes
    [hasEKG, hasPrometheus, hasTimeseries, alEndpoint <$> alarms]

  check :: String -> Bool -> Maybe String
  check msg True  = Just msg
  check _   False = Nothing

  noCredentials :: AlarmsConfig -> Bool
  noCredentials AlarmsConfig{alAuthentication = AlarmsAuthConfig{aacProducers, aacReaders}} =
    null aacProducers && null aacReaders

  hasDuplicateConsumerNames :: AlarmsConfig -> Bool
  hasDuplicateConsumerNames AlarmsConfig{alConsumers} =
    hasDuplicates (map consumerName alConsumers)

  consumerName :: AlarmsConsumerConfig -> Text
  consumerName AlarmsConsumerLog{aclName} = aclName

  nullAddress :: Address -> Bool
  nullAddress (Net.LocalPipe address)       = null address
  nullAddress (Net.RemoteSocket host _port) = Text.null host

  nullEndpoint :: Endpoint -> Bool
  nullEndpoint (Endpoint host _port _) = null host

  invalidFileMode :: LoggingParams -> Bool
  invalidFileMode (LoggingParams root FileMode    _) = null root
  invalidFileMode (LoggingParams _    JournalMode _) = False

-- | Checks if a list contains duplicate elements.
hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates xs = nub xs /= xs
