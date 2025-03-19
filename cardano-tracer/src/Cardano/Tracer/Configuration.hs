{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Configuration
  ( Address (..)
  , Endpoint (..)
  , setEndpoint
  , FileOrMap (..)
  , LogFormat (..)
  , LogMode (..)
  , LoggingParams (..)
  , Network (..)
  , RotationParams (..)
  , TracerConfig (..)
  , Verbosity (..)
  , readTracerConfig
  ) where

import qualified Cardano.Logging.Types as Log

import           Control.Applicative ((<|>))
import           Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:))
import           Data.Fixed (Pico)
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Data.List (intercalate, nub)
import           Data.List.Extra (notNull)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import           Data.Maybe (catMaybes)
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Word (Word16, Word32, Word64)
import           Data.Yaml (decodeFileEither)
import           GHC.Generics (Generic)
import           Network.Wai.Handler.Warp (HostPreference, Port, Settings, setHost, setPort)
import           System.Exit (die)

-- | Only local socket is supported, to avoid unauthorized connections.
newtype Address = LocalSocket FilePath
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Endpoint for internal services.
data Endpoint = Endpoint
  { epHost :: !String
  , epPort :: !Port
  }
  deriving stock (Eq, Generic, Show)
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

instance FromJSON RotationParams where
  parseJSON = withObject "RotationParams" \o -> do
    rpFrequencySecs <- o .: "rpFrequencySecs"
    rpLogLimitBytes <- o .: "rpLogLimitBytes"
    rpMaxAgeMinutes <- o .: "rpMaxAgeMinutes"
                   <|> (o .: "rpMaxAgeHours" <&> (* 60))
                   <|> pure (24 * 60)
    rpKeepFilesNum  <- o .: "rpKeepFilesNum"
    pure RotationParams{..}

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
  { networkMagic   :: !Word32                       -- ^ Network magic from genesis the node is launched with.
  , network        :: !Network                      -- ^ How cardano-tracer will be connected to node(s).
  , loRequestNum   :: !(Maybe Word16)               -- ^ How many 'TraceObject's will be asked in each request.
  , ekgRequestFreq :: !(Maybe Pico)                 -- ^ How often to request for EKG-metrics, in seconds.
  , hasEKG         :: !(Maybe Endpoint)             -- ^ Endpoint for EKG web-page.
  , hasPrometheus  :: !(Maybe Endpoint)             -- ^ Endpoint for Prometheus web-page.
  , hasRTView      :: !(Maybe Endpoint)             -- ^ Endpoint for RTView web-page.
    -- | Socket for tracer's to reforward on. Second member of the triplet is the list of prefixes to reforward.
    -- Third member of the triplet is the forwarder config.
  , hasForwarding  :: !(Maybe ( Network
                              , Maybe [[Text]]
                              , Log.TraceOptionForwarder
                              ))
  , logging         :: !(NonEmpty LoggingParams)    -- ^ Logging parameters.
  , rotation        :: !(Maybe RotationParams)      -- ^ Rotation parameters.
  , verbosity       :: !(Maybe Verbosity)           -- ^ Verbosity of the tracer itself.
  , metricsNoSuffix :: !(Maybe Bool)                -- ^ Prometheus ONLY: Dropping metrics name suffixes (like "_int") increases similiarity with old system names - if desired; default: False
  , metricsHelp     :: !(Maybe FileOrMap)           -- ^ Prometheus ONLY: JSON file or object containing a key-value map "metric name -> help text" for "# HELP " annotations
  , resourceFreq    :: !(Maybe Int)                 -- ^ Frequency (1/millisecond) for gathering resource data.
  , ekgRequestFull  :: !(Maybe Bool)                -- ^ Request full set of metrics always, vs. deltas only (safer, but more overhead); default: False
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Read the tracer's configuration file.
readTracerConfig :: FilePath -> IO TracerConfig
readTracerConfig pathToConfig =
  decodeFileEither pathToConfig >>= \case
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
  , logging
  , hasRTView
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
    , check "no host in hasRTView"     . nullEndpoint =<< hasRTView
    ]

  ports :: [Port]
  ports = epPort <$> catMaybes [hasEKG, hasPrometheus, hasRTView]

  check :: String -> Bool -> Maybe String
  check msg True  = Just msg
  check _   False = Nothing

  nullAddress :: Address -> Bool
  nullAddress (LocalSocket address) = null address

  nullEndpoint :: Endpoint -> Bool
  nullEndpoint (Endpoint host _port) = null host

  invalidFileMode :: LoggingParams -> Bool
  invalidFileMode (LoggingParams root FileMode    _) = null root
  invalidFileMode (LoggingParams _    JournalMode _) = False

-- | Checks if a list contains duplicate elements.
hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates xs = nub xs /= xs
