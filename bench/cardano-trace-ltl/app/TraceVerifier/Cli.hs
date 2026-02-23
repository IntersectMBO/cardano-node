{-# LANGUAGE CPP        #-}

module TraceVerifier.Cli(Mode(..), CliOptions(..), opts) where


import           Options.Applicative

#define RETENTION_DEFAULT 200

type Filename = String

data Mode = Online | Offline deriving (Show, Eq)

readMode :: ReadM Mode
readMode = eitherReader $ \case
  "offline" -> Right Offline
  "online"  -> Right Online
  _         -> Left "Expected either of: 'offline' or 'online'"

parseMode :: Parser Mode
parseMode = option readMode (long "mode" <> metavar "<offline|online>" <> help "mode")

parseEventDuration :: Parser Word
parseEventDuration = option auto (long "duration" <> metavar "INT" <> help "temporal event duration")

parseDumpMetrics :: Parser Bool
parseDumpMetrics = option auto $
     long "dump-metrics"
  <> showDefault
  <> value False
  <> metavar "BOOL"
  <> help "enable periodic metric dumps to stdout"

parseFormulasFile :: Parser Filename
parseFormulasFile = argument str (metavar "FILE")

parseTraceDispatcherCfgFile :: Parser Filename
parseTraceDispatcherCfgFile =
  option str (long "trace-dispatcher-cfg" <> metavar "FILE" <> help "trace dispatcher configuration file")

parseContext :: Parser (Maybe Filename)
parseContext =
  option (optional str) (long "context" <> showDefault <> value Nothing <> metavar "CONTEXT" <> help "context variables")

parseTraceFiles :: Parser [Filename]
parseTraceFiles = some (argument str (metavar "FILES"))

parseRetention :: Parser Word
parseRetention = option auto $
     long "retention"
  <> metavar "INT"
  <> showDefault
  <> value RETENTION_DEFAULT
  <> help "temporal event retention period before it gets consumed"

parseSeekToEnd :: Parser Bool
parseSeekToEnd = option auto $
     long "seek-to-end"
  <> metavar "BOOL"
  <> showDefault
  <> value True
  <> help "seek to the end of the trace file before ingesting it"

data CliOptions = CliOptions
  { formulas            :: Filename
  , mode                :: Mode
  , eventDuration       :: Word
  , traces              :: [Filename]
  , retention           :: Word
  , traceDispatcherCfg  :: Filename
  , context             :: Maybe Filename
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


