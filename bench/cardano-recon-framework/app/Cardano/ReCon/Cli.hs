module Cardano.ReCon.Cli(Timeunit(..), timeunitToMicrosecond, Mode(..), CliOptions(..), opts) where


import           Cardano.ReCon.LTL.Formula (OnMissingKey (..))

import           Control.Arrow ((>>>))
import           Data.Char (toLower)
import           Options.Applicative

-- | We assume that the events from the the input log might be slightly out of order.
--   The value stands for the default amount of time (ms) the log ingestor keeps the read events "in the loop",
--     them subject to reordering by timestamp.
--   The events withheld longer than this amount, will finally be fed to the formula satisfiability checker.
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

data Timeunit = Hour | Minute | Second | Millisecond | Microsecond deriving (Ord, Eq)

-- | Convert `Timeunit` to μs.
timeunitToMicrosecond :: Timeunit -> Word -> Word
timeunitToMicrosecond Hour        = (3_600_000_000 *)
timeunitToMicrosecond Minute      = (60_000_000 *)
timeunitToMicrosecond Second      = (1_000_000 *)
timeunitToMicrosecond Millisecond = (1_000 *)
timeunitToMicrosecond Microsecond = id

instance Show Timeunit where
  show Hour        = "hour"
  show Minute      = "minute"
  show Second      = "second"
  show Millisecond = "millisecond"
  show Microsecond = "microsecond"

readTimeunit :: ReadM Timeunit
readTimeunit = eitherReader $ fmap toLower >>> \case
  "hour"         -> Right Hour
  "h"            -> Right Hour
  "minute"       -> Right Minute
  "min"          -> Right Minute
  "m"            -> Right Minute
  "second"       -> Right Second
  "sec"          -> Right Second
  "s"            -> Right Second
  "millisecond"  -> Right Millisecond
  "millisec"     -> Right Millisecond
  "millis"       -> Right Millisecond
  "ms"           -> Right Millisecond
  "microsecond"  -> Right Microsecond
  "microsec"     -> Right Microsecond
  "micros"       -> Right Microsecond
  "μs"           -> Right Microsecond
  _              -> Left "Can't read a Timeunit"

parseTimeunit :: Parser Timeunit
parseTimeunit = option readTimeunit $
     long "timeunit"
  <> metavar "<hour|minute|second|millisecond|microsecond>"
  <> showDefault
  <> value Second
  <> help "timeunit"

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

readOnMissingKey :: ReadM OnMissingKey
readOnMissingKey = eitherReader $ fmap toLower >>> \case
  "crash"  -> Right CrashOnMissingKey
  "bottom" -> Right BottomOnMissingKey
  _        -> Left "Expected either of: 'crash' or 'bottom'"

parseOnMissingKey :: Parser OnMissingKey
parseOnMissingKey = option readOnMissingKey $
     long "on-missing-key"
  <> metavar "<crash|bottom>"
  <> showDefaultWith (\case BottomOnMissingKey -> "bottom"; CrashOnMissingKey -> "crash")
  <> value BottomOnMissingKey
  <> help "behaviour when a formula atom references a missing event property key"

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
  , timeunit            :: Timeunit
  , onMissingKey        :: OnMissingKey
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
              <*> parseTimeunit
              <*> parseOnMissingKey

opts :: ParserInfo CliOptions
opts = info (parseCliOptions <**> helper)
  (fullDesc <> progDesc "Check formula satisfiability against a log of trace messages")
