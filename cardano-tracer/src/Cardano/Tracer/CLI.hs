module Cardano.Tracer.CLI
  ( TracerParams (..)
  , parseTracerParams
  ) where

import           Options.Applicative

-- | CLI parameters required for the tracer.
newtype TracerParams = TracerParams
  { tracerConfig :: FilePath
  }

-- | Parse CLI parameters for the tracer.
parseTracerParams :: Parser TracerParams
parseTracerParams = TracerParams <$>
  strOption
    (    long "config"
      <> short 'c'
      <> metavar "FILEPATH"
      <> help "Configuration file for cardano-tracer"
      <> completer (bashCompleter "file")
    )
