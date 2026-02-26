{-# OPTIONS_GHC -Wno-orphans #-}

module Main(main) where

import           Cardano.Logging
import           Cardano.Logging.Types.TraceMessage (TraceMessage (..))
import           Cardano.ReCon.Cli (CliOptions (..), Mode (..), opts)
import           Cardano.ReCon.Common (extractProps)
import           Cardano.ReCon.LTL.Lang.Formula
import           Cardano.ReCon.LTL.Lang.Formula.Parser (Context (..))
import qualified Cardano.ReCon.LTL.Lang.Formula.Parser as Parser
import           Cardano.ReCon.LTL.Lang.Formula.Yaml
import           Cardano.ReCon.LTL.Satisfy
import           Cardano.ReCon.Trace.Feed (TemporalEvent (..),
                   TemporalEventDurationMicrosec, read, readS)
import           Cardano.ReCon.Trace.Ingest
import           Cardano.ReCon.TraceMessage (TraceMessage (..), formulaOutcome)
import qualified Cardano.ReCon.TraceMessage as App

import           Prelude hiding (read)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (cancel, forConcurrently_, withAsync)
import           Control.Monad (forever, when, (>=>))
import           Data.Foldable (for_)
import           Data.IORef (IORef, newIORef, readIORef)
import           Data.List (find)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.Text (Text, unpack)
import qualified Data.Text as Text
import           Data.Traversable (for)
import           Options.Applicative hiding (Success)
import           System.Exit (die)

import           Streaming

instance Event TemporalEvent Text where
  ofTy (TemporalEvent _ msgs) c = isJust $ find (\msg -> msg.tmsgNS == c) msgs
  props (TemporalEvent _ msgs) c =
    case find (\msg -> msg.tmsgNS == c) msgs of
      Just x  -> Map.insert "host" (TextValue x.tmsgHost)       $
                   Map.insert "thread" (TextValue x.tmsgThread) $
                     extractProps x.tmsgData
      Nothing -> error ("Not an event of type " <> unpack c)
  beg (TemporalEvent t _) = t

check :: Trace IO App.TraceMessage -> Formula TemporalEvent Text -> [TemporalEvent] -> IO ()
check tr phi events =
  let result = satisfies phi events in
  traceWith tr $ formulaOutcome phi result

checkS' :: Bool -> Trace IO App.TraceMessage -> Formula TemporalEvent Text -> Stream (Of TemporalEvent) IO () -> IO ()
checkS' enableProgressDumps tr phi events = do
  let initial = SatisfyMetrics 0 phi 0
  metrics <- newIORef initial
  withAsync (when enableProgressDumps $ runDisplayProgressDump initial metrics) $ \counterDisplayThread -> do
    r <- satisfiesS phi events metrics
    traceWith tr $ formulaOutcome phi r
    cancel counterDisplayThread
  where
    runDisplayProgressDump :: SatisfyMetrics TemporalEvent Text -> IORef (SatisfyMetrics TemporalEvent Text) -> IO ()
    runDisplayProgressDump prev counter = do
      next <- readIORef counter
      let eventPerSecond = next.eventsConsumed - prev.eventsConsumed
      let catchupRatio = (fromIntegral (next.currentTimestamp - prev.currentTimestamp) :: Double) / 1_000_000
      traceWith tr $ FormulaProgressDump (fromIntegral eventPerSecond) catchupRatio next.currentFormula
      threadDelay 1_000_000 -- 1s
      runDisplayProgressDump next counter

checkOnline :: Bool
            -> Trace IO App.TraceMessage
            -> TemporalEventDurationMicrosec
            -> Word
            -> FailureMode
            -> IngestMode
            -> [FilePath]
            -> [Formula TemporalEvent Text]
            -> IO ()
checkOnline enableProgressDumps tr eventDuration retentionMs failureMode ingestMode files phis = do
  ing <- mkIngestor (fromIntegral retentionMs)
  for_ files (ingestFileThreaded ing failureMode ingestMode)
  forConcurrently_ phis $ \phi -> mkIngestorReader ing >>= \reader -> forever $ do
    traceWith tr $ FormulaStartCheck phi
    checkS' enableProgressDumps tr phi (readS reader eventDuration)

checkOffline :: Trace IO App.TraceMessage
             -> TemporalEventDurationMicrosec
             -> FilePath
             -> [Formula TemporalEvent Text]
             -> IO ()
checkOffline tr eventDuration file phis = do
  events <- read file eventDuration
  forConcurrently_ phis $ \phi ->
    check tr phi events
  threadDelay 200_000 -- Give the tracer a grace period to output the logs to whatever backend

-- | Convert time unit used in the yaml (currently second) input to μs.
unitToMicrosecond :: Word -> Word
unitToMicrosecond = (1_000_000 *)

setupTraceDispatcher :: Maybe FilePath -> IO (Trace IO App.TraceMessage)
setupTraceDispatcher optTraceDispatcherConfigFile = do
  stdTr <- standardTracer
  configReflection <- emptyConfigReflection
  cfg <- fromMaybe defaultTraceConfig <$> traverse (`readConfigurationWithDefault` defaultTraceConfig) optTraceDispatcherConfigFile
  tr <- mkCardanoTracer @App.TraceMessage stdTr mempty Nothing ["ReCon"]
  configureTracers configReflection cfg [tr]
  pure tr
  where
    defaultTraceConfig :: TraceConfig
    defaultTraceConfig =
       emptyTraceConfig
        { tcOptions = Map.fromList
            [([], [ ConfSeverity (SeverityF (Just Info))
                  , ConfBackend [Stdout HumanFormatColoured]])
            ]
        }



main :: IO ()
main = do
  options <- execParser opts
  ctx <- Map.toList . fromMaybe Map.empty <$> for options.context (readPropValues >=> dieOnYamlException)
  putStrLn "Context:"
  print ctx
  formulas <- readFormulas options.formulas (Context ctx) Parser.text >>= dieOnYamlException
  let formulas' = fmap (interpTimeunit (\u -> unitToMicrosecond u `div` fromIntegral options.duration)) formulas
  tr <- setupTraceDispatcher options.traceDispatcherCfg
  case options.mode of
    Offline -> do
      file <- case options.traces of
        [x] -> pure x
        _   -> die "Only exactly one trace file is supported in 'offline' mode"
      checkOffline tr options.duration file formulas'
    Online -> do
      checkOnline
        options.enableProgressDumps
        tr
        options.duration
        options.retention
        RethrowExceptions
        (if options.enableSeekToEnd then FromFileEnd else FromFileStart)
        options.traces
        formulas'
  where
    dieOnYamlException :: forall a. Either Exception a -> IO a
    dieOnYamlException (Left exc) = die (Text.unpack exc)
    dieOnYamlException (Right ok) = pure ok
