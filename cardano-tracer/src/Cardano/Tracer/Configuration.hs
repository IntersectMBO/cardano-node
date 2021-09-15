{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Configuration
  ( Host
  , Port
  , Address (..)
  , Endpoint (..)
  , RotationParams (..)
  , LogMode (..)
  , LogFormat (..)
  , LoggingParams (..)
  , Network (..)
  , TracerConfig (..)
  , readTracerConfig
  ) where

import           Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict')
import           Data.Fixed (Pico)
import           Data.List.NonEmpty
import           Data.Word (Word16, Word64)
import           GHC.Generics (Generic)
import           System.Exit (die)

type Host = String
type Port = Int

-- | Only local socket is supported.
newtype Address = LocalSocket FilePath
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

-- | Endpoint for internal services.
data Endpoint = Endpoint !Host !Port
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data RotationParams = RotationParams
  { rpLogLimitBytes :: !Word64  -- ^ Max size of file in bytes
  , rpMaxAgeHours   :: !Word    -- ^ Hours
  , rpKeepFilesNum  :: !Word    -- ^ Number of files to keep
  } deriving (Eq, Generic, FromJSON, Show, ToJSON)

-- | Log mode: in the file or in Linux journal service.
data LogMode
  = FileMode
  | JournalMode
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

-- | Format of log file: for human (text) or for machine (json).
data LogFormat
  = ForHuman
  | ForMachine
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

-- | Logging parameters.
data LoggingParams = LoggingParams
  { logRoot   :: !FilePath  -- ^ Root directory where all subdirs with logs will be created.
  , logMode   :: !LogMode   -- ^ Log mode.
  , logFormat :: !LogFormat -- ^ Log format.
  } deriving (Eq, Generic, FromJSON, Show, ToJSON)

data Network
  = AcceptAt  !Address
  | ConnectTo !(NonEmpty Address)
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

-- | Complete configuration.
data TracerConfig = TracerConfig
  { network        :: !Network                  -- ^ How cardano-tracer will be connected to node(s).
  , loRequestNum   :: !(Maybe Word16)           -- ^ How many 'TraceObject's will be asked in each request.
  , ekgRequestFreq :: !(Maybe Pico)             -- ^ How often to request for EKG-metrics, in seconds.
  , hasEKG         :: !(Maybe Endpoint)         -- ^ Endpoint for EKG web-page.
  , hasPrometheus  :: !(Maybe Endpoint)         -- ^ Endpoint for Promeheus web-page.
  , logging        :: !(NonEmpty LoggingParams) -- ^ Logging parameters.
  , rotation       :: !(Maybe RotationParams)   -- ^ Rotation parameters.
  } deriving (Eq, Generic, FromJSON, Show, ToJSON)

-- | Read the tracer's configuration file (path is passed via '--config' CLI option).
readTracerConfig :: FilePath -> IO TracerConfig
readTracerConfig pathToConfig =
  eitherDecodeFileStrict' pathToConfig >>= \case
    Left e -> die $ "Invalid tracer's configuration: " <> show e
    Right (config :: TracerConfig) -> return config
