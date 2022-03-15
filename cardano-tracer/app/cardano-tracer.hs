import           Data.Version (showVersion)
import           Options.Applicative

import           Cardano.Tracer.CLI (TracerParams, parseTracerParams)
import           Cardano.Tracer.Run (runCardanoTracer)
import           Paths_cardano_tracer (version)

main :: IO ()
main =
  runCardanoTracer =<< customExecParser (prefs showHelpOnEmpty) tracerInfo
 where
  tracerInfo :: ParserInfo TracerParams
  tracerInfo = info
    (parseTracerParams <**> helper <**> versionOption)
    (fullDesc <> header "cardano-tracer - the logging and monitoring service for Cardano nodes.")
  versionOption = infoOption
    (showVersion version)
    (long "version" <>
     short 'v' <>
     help "Show version")
