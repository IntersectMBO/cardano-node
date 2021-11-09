module Cardano.Tracer.CLI
  ( TracerParams (..)
  , parseTracerParams
  ) where

import           Options.Applicative

-- | Type for CLI parameters required for the 'cardano-tracer'.
newtype TracerParams = TracerParams
  { tracerConfig :: FilePath
  }

parseTracerParams :: Parser TracerParams
parseTracerParams = TracerParams <$>
  strOption
    (    long "config"
      <> short 'c'
      <> metavar "FILEPATH"
      <> help "Configuration file for cardano-tracer"
      <> completer (bashCompleter "file")
    )
