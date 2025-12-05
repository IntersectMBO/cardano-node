{-# LANGUAGE OverloadedRecordDot #-}

import           Cardano.Tracer.CLI (TracerParams(..), parseTracerParams)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Run (runCardanoTracer)

import           Data.Functor (void)
import           Data.Version (showVersion)
import           Options.Applicative

import           Paths_cardano_tracer (version)

main :: IO ()
main = void do
  tracerParams :: TracerParams
     <- customExecParser (prefs showHelpOnEmpty) tracerInfo
  trace :: Trace IO TracerTrace <-
    -- Default `Nothing' severity filter to Info.
    mkTracerTracer $ SeverityF (tracerParams.logSeverity <|> Just Info)
  runCardanoTracer trace tracerParams

tracerInfo :: ParserInfo TracerParams
tracerInfo = info
  (parseTracerParams <**> helper <**> versionOption)
#if RTVIEW
  (fullDesc <> header "cardano-tracer/with RTView - the logging and monitoring service for Cardano nodes.")
#else
  (fullDesc <> header "cardano-tracer/without RTView - the logging and monitoring service for Cardano nodes.")
#endif

versionOption :: Parser (a -> a)
versionOption = infoOption
  do showVersion version
  do mconcat
       [ long  "version"
       , short 'v'
       , help  "Show version"
       ]
