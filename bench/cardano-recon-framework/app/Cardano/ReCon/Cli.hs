module Cardano.ReCon.Cli(Mode(..), CliOptions(..), opts) where


import           Control.Arrow ((>>>))
import           Data.Char (toLower)
import           Options.Applicative

-- MKREV: maybe add a comment about what this does / what is meant be retention period
-- I mean, we know internally, but...
retentionDefault :: Word
retentionDefault = 200

data Mode = Online | Offline deriving (Show, Eq)

readMode :: ReadM Mode
readMode = eitherReader $ \case
  "offline" -> Right Offline
  "online"  -> Right Online
  _         -> Left "Expected either of: 'offline' or 'online'"

readBool :: ReadM Bool
readBool = eitherReader $ fmap toLower >>> \case
  "true"   -> Right True
  "1"      -> Right True
  "false"  -> Right False
  "0"      -> Right False
  _        -> Left "Can't read a boolean"

parseMode :: Parser Mode
parseMode = option readMode (long "mode" <> metavar "<offline|online>" <> help "mode")

parseEventDuration :: Parser Word
parseEventDuration = option auto (long "duration" <> metavar "INT" <> help "temporal event duration (μs)")

parseDumpMetrics :: Parser Bool
parseDumpMetrics = option readBool $
     long "dump-metrics"
  <> showDefault
  <> value False
  <> metavar "BOOL"
  <> help "enable periodic metric dumps to stdout"

parseFormulasFile :: Parser FilePath
parseFormulasFile = argument str (metavar "FILE")

parseTraceDispatcherCfgFile :: Parser (Maybe FilePath)
parseTraceDispatcherCfgFile =
  option (optional str) (long "trace-dispatcher-cfg" <> value Nothing <> metavar "FILE" <> help "trace dispatcher configuration file")

parseContext :: Parser (Maybe FilePath)
parseContext =
  option (optional str) (long "context" <> value Nothing <> metavar "FILE" <> help "context variables")

parseTraceFiles :: Parser [FilePath]
parseTraceFiles = some (argument str (metavar "FILES"))

parseRetention :: Parser Word
parseRetention = option auto $
     long "retention"
  <> metavar "INT"
  <> showDefault
  <> value retentionDefault
  <> help "temporal event retention period (ms)"

parseSeekToEnd :: Parser Bool
parseSeekToEnd = option readBool $
     long "seek-to-end"
  <> metavar "BOOL"
  <> showDefault
  <> value True
  <> help "seek to the end of the trace file before ingesting it"

data CliOptions = CliOptions
  { formulas            :: FilePath
  , mode                :: Mode
  , duration            :: Word
  , traces              :: [FilePath]
  , retention           :: Word
  , traceDispatcherCfg  :: Maybe FilePath
  , context             :: Maybe FilePath
  , enableProgressDumps :: Bool
  , enableSeekToEnd     :: Bool
  }

parseCliOptions :: Parser CliOptions
parseCliOptions = CliOptions
              <$> parseFormulasFile
              <*> parseMode
              <*> parseEventDuration
              <*> parseTraceFiles
              <*> parseRetention
              <*> parseTraceDispatcherCfgFile
              <*> parseContext
              <*> parseDumpMetrics
              <*> parseSeekToEnd

opts :: ParserInfo CliOptions
opts = info (parseCliOptions <**> helper)
  (fullDesc <> progDesc "Check formula satisfiability against a log of trace messages")
