{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Configuration
  ( Address (..)
  , Endpoint (..)
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
import           Data.Aeson (FromJSON (..), ToJSON, withObject, (.:))
import           Data.Fixed (Pico)
import           Data.Functor ((<&>))
import           Data.List (intercalate)
import           Data.List.Extra (notNull)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import           Data.Word (Word16, Word32, Word64)
import           Data.Yaml (decodeFileEither)
import           GHC.Generics (Generic)
import           System.Exit (die)

-- | Only local socket is supported, to avoid unauthorized connections.
newtype Address = LocalSocket FilePath
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Endpoint for internal services.
data Endpoint = Endpoint
  { epHost :: !String
  , epPort :: !Word16
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

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
                   <|> o .: "rpMaxAgeHours" <&> (* 60)
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

-- | Tracer configuration.
data TracerConfig = TracerConfig
  { networkMagic   :: !Word32                       -- ^ Network magic from genesis the node is launched with.
  , network        :: !Network                      -- ^ How cardano-tracer will be connected to node(s).
  , loRequestNum   :: !(Maybe Word16)               -- ^ How many 'TraceObject's will be asked in each request.
  , ekgRequestFreq :: !(Maybe Pico)                 -- ^ How often to request for EKG-metrics, in seconds.
  , hasEKG         :: !(Maybe (Endpoint, Endpoint)) -- ^ Endpoint for EKG web-page (list of nodes, monitoring).
  , hasPrometheus  :: !(Maybe Endpoint)             -- ^ Endpoint for Prometheus web-page.
  , hasRTView      :: !(Maybe Endpoint)             -- ^ Endpoint for RTView web-page.
    -- | Socket for tracer's to reforward on. Second member of the triplet is the list of prefixes to reforward.
    -- Third member of the triplet is the forwarder config.
  , hasForwarding  :: !(Maybe ( Network
                              , Maybe [[Text]]
                              , Log.TraceOptionForwarder
                              ))
  , logging        :: !(NonEmpty LoggingParams)     -- ^ Logging parameters.
  , rotation       :: !(Maybe RotationParams)       -- ^ Rotation parameters.
  , verbosity      :: !(Maybe Verbosity)            -- ^ Verbosity of the tracer itself.
  , metricsComp    :: !(Maybe (Map Text Text))      -- ^ Metrics compatibility map from metrics name to metrics name
  , resourceFreq   :: !(Maybe Int)                  -- ^ Frequency (1/millisecond) for gathering resource data.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Read the tracer's configuration file.
readTracerConfig :: FilePath -> IO TracerConfig
readTracerConfig pathToConfig =
  decodeFileEither pathToConfig >>= \case
    Left e -> die $ "Invalid tracer's configuration: " <> show e
    Right (config :: TracerConfig) ->
      case checkMeaninglessValues config of
        Left problems -> die $ "Tracer's configuration is meaningless: " <> problems
        Right{} -> return (nubLogging config)

  where
  -- Remove duplicate logging parameters.
  nubLogging :: TracerConfig -> TracerConfig
  nubLogging tracerConfig@TracerConfig{logging} = tracerConfig
    { logging = NE.nub logging
    }

checkMeaninglessValues :: TracerConfig -> Either String ()
checkMeaninglessValues TracerConfig
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
  problems = catMaybes
    [ case network of
        AcceptAt addr -> check "AcceptAt is empty" $ nullAddress addr
        ConnectTo addrs -> check "ConnectTo are empty" $ null . NE.filter (not . nullAddress) $ addrs
    , check "empty logRoot(s)" $ notNull . NE.filter invalidFileMode $ logging
    , (check "no host(s) in hasEKG" . nullEndpoints) =<< hasEKG
    , (check "no host in hasPrometheus" . nullEndpoint) =<< hasPrometheus
    , (check "no host in hasRTView" . nullEndpoint) =<< hasRTView
    ]

  check msg cond = if cond then Just msg else Nothing

  nullAddress (LocalSocket p) = null p

  nullEndpoint (Endpoint h _) = null h

  nullEndpoints (ep1, ep2) = nullEndpoint ep1 || nullEndpoint ep2

  invalidFileMode (LoggingParams root FileMode    _) = null root
  invalidFileMode (LoggingParams _    JournalMode _) = False
