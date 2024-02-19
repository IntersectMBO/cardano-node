module Cardano.Tracer.CLI
  ( TracerParams (..)
  , parseTracerParams
  ) where

import           Cardano.Logging

import           Options.Applicative


-- | CLI parameters required for the tracer.
data TracerParams = TracerParams
  { tracerConfig :: !FilePath
  , stateDir     :: !(Maybe FilePath)
  , logSeverity  :: !(Maybe SeverityS)
  }

-- | Parse CLI parameters for the tracer.
parseTracerParams :: Parser TracerParams
parseTracerParams = TracerParams
  <$> strOption
        (    long "config"
          <> short 'c'
          <> metavar "FILEPATH"
          <> help "Configuration file for cardano-tracer"
          <> completer (bashCompleter "file")
        )
  <*> optional
        (
          strOption
            (    long "state-dir"
              <> metavar "FILEPATH"
              <> help "If specified, RTView saves its state in this directory"
              <> completer (bashCompleter "file")
            )
        )
  <*> optional
        ( option auto
            (    long "min-log-severity"
              <> metavar "SEVERITY"
              <> help "Drop messages less severe than this.  One of: Debug. Info. Notice. Warning. Error. Critical. Alert. Emergency."
            )
        )
