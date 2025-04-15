import                             Cardano.Tracer.CLI (TracerParams, parseTracerParams)
import                             Cardano.Tracer.Run (runCardanoTracer)

import                             Data.Version (showVersion)
import "optparse-applicative-fork" Options.Applicative

import                             Paths_cardano_tracer (version)

main :: IO ()
main =
  runCardanoTracer =<< customExecParser (prefs showHelpOnEmpty) tracerInfo

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
  (showVersion version)
  (long "version" <>
   short 'v' <>
   help "Show version")
