import           Data.Version (showVersion)
import           Options.Applicative

import           Cardano.Tracer.Analyze.CLI
import           Cardano.Tracer.Analyze.Process
import           Cardano.Tracer.Analyze.Report
import           Cardano.Tracer.Analyze.Types
import           Paths_trace_analyzer (version)

main :: IO ()
main = do
    tracerParams <- customExecParser (prefs showHelpOnEmpty) analyzerInfo
    db1 <- parseAndPreprocess (apTrace1 tracerParams)
    db2 <- parseAndPreprocess (apTrace2 tracerParams)
    report db1 db2
    pure ()
  where
    analyzerInfo :: ParserInfo AnalyzerParams
    analyzerInfo = info
      (parseAnalyzerParams <**> helper <**> versionOption)
      (fullDesc <> header "cardano-analyzer - the tracer analyzer service for Cardano node.")
    versionOption = infoOption
      (showVersion version)
      (long "version" <>
       short 'v' <>
       help "Show version")
