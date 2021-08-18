{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Tracer.CLI
  ( TracerParams (..)
  , parseTracerParams
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import           Options.Applicative

-- | Type for CLI parameters required for the service.
newtype TracerParams = TracerParams
  { tracerConfig :: FilePath
  } deriving (Generic, FromJSON, ToJSON)

parseTracerParams :: Parser TracerParams
parseTracerParams = TracerParams <$>
  strOption (
       long "config"
    <> short 'c'
    <> metavar "FILEPATH"
    <> help "Configuration file for cardano-tracer service"
    <> completer (bashCompleter "file")
    )
