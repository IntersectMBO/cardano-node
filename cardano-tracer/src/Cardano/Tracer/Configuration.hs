{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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

import           Data.Aeson (FromJSON, eitherDecodeFileStrict')
import           Data.Fixed (Pico)
import           Data.List.NonEmpty
import           Data.Word (Word16, Word64)
import           GHC.Generics (Generic)
import           System.Exit (die)

-- | Only local socket is supported, to avoid unauthorized connections.
newtype Address = LocalSocket FilePath
  deriving (Eq, Generic, FromJSON, Show)

-- | Endpoint for internal services.
data Endpoint = Endpoint
  { epHost :: !String
  , epPort :: !Word16
  } deriving (Eq, Generic, FromJSON, Show)

-- | Parameters of rotation mechanism for logs.
data RotationParams = RotationParams
  { rpFrequencySecs :: !Word    -- ^ Rotation period, in seconds.
  , rpLogLimitBytes :: !Word64  -- ^ Max size of log file in bytes.
  , rpMaxAgeHours   :: !Word    -- ^ Max age of log file in hours.
  , rpKeepFilesNum  :: !Word    -- ^ Number of log files to keep in any case.
  } deriving (Eq, Generic, FromJSON, Show)

-- | Logging mode.
data LogMode
  = FileMode    -- ^ Store items in log file.
  | JournalMode -- ^ Store items in Linux journal service.
  deriving (Eq, Generic, FromJSON, Show)

-- | Format of log files.
data LogFormat
  = ForHuman   -- ^ For human (text)
  | ForMachine -- ^ For machine (JSON)
  deriving (Eq, Generic, FromJSON, Show)

-- | Logging parameters.
data LoggingParams = LoggingParams
  { logRoot   :: !FilePath  -- ^ Root directory where all subdirs with logs are created.
  , logMode   :: !LogMode   -- ^ Log mode.
  , logFormat :: !LogFormat -- ^ Log format.
  } deriving (Eq, Generic, FromJSON, Show)

-- | Connection mode.
data Network
  = AcceptAt  !Address            -- ^ Server mode: accepts connections.
  | ConnectTo !(NonEmpty Address) -- ^ Client mode: initiates connections.
  deriving (Eq, Generic, FromJSON, Show)

-- | Tracer's verbosity.
data Verbosity
  = Minimum    -- ^ Display minimum of messages.
  | ErrorsOnly -- ^ Display errors only.
  | Maximum    -- ^ Display all the messages (protocols tracing, errors).
  deriving (Eq, Generic, FromJSON, Show)

-- | Tracer configuration.
data TracerConfig = TracerConfig
  { network        :: !Network                      -- ^ How cardano-tracer will be connected to node(s).
  , loRequestNum   :: !(Maybe Word16)               -- ^ How many 'TraceObject's will be asked in each request.
  , ekgRequestFreq :: !(Maybe Pico)                 -- ^ How often to request for EKG-metrics, in seconds.
  , hasEKG         :: !(Maybe (Endpoint, Endpoint)) -- ^ Endpoint for EKG web-page (list of nodes, monitoring).
  , hasPrometheus  :: !(Maybe Endpoint)             -- ^ Endpoint for Promeheus web-page.
  , hasRTView      :: !(Maybe Endpoint)             -- ^ Endpoint for RTView web-page.
  , logging        :: !(NonEmpty LoggingParams)     -- ^ Logging parameters.
  , rotation       :: !(Maybe RotationParams)       -- ^ Rotation parameters.
  , verbosity      :: !(Maybe Verbosity)            -- ^ Verbosity of the tracer itself.
  } deriving (Eq, Generic, FromJSON, Show)

-- | Read the tracer's configuration file.
readTracerConfig :: FilePath -> IO TracerConfig
readTracerConfig pathToConfig =
  eitherDecodeFileStrict' pathToConfig >>= \case
    Left e -> die $ "Invalid tracer's configuration: " <> show e
    Right (config :: TracerConfig) -> return config
