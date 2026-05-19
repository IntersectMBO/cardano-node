{-# LANGUAGE PackageImports #-}

module Main(main) where

import           Cardano.Logging
import           Cardano.Logging.Prometheus.TCPServer (TracePrometheusSimple (..),
                   runPrometheusSimple)
import           Cardano.Logging.Types.TraceMessage ()
import           Cardano.ReCon.Cli (CliOptions (..), Mode (..), opts, timeunitToMicrosecond)
import           Cardano.ReCon.Trace.Event ()
import           Cardano.ReCon.LTL.Check (checkFormula, prettyError)
import           Cardano.ReCon.LTL.Formula
import           Cardano.ReCon.LTL.Formula.Parser (Context (..))
import qualified Cardano.ReCon.LTL.Formula.Parser as Parser
import           Cardano.ReCon.LTL.Formula.Yaml
import           Cardano.ReCon.LTL.Formula.Pretty (prettyFormula)
import qualified Cardano.ReCon.LTL.Formula.Pretty as Prec
import           Cardano.ReCon.LTL.Satisfy
import           Cardano.ReCon.Trace.Feed (TemporalEvent (..), TemporalEventDurationMicrosec, read,
                   readS)
import           Cardano.ReCon.Trace.Ingest
import           Cardano.ReCon.TraceMessage (TraceMessage (..), formulaOutcome)
import qualified Cardano.ReCon.TraceMessage as App

import           Prelude hiding (read)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (cancel, forConcurrently_, link, withAsync)
import           Control.Monad (forever, when, (>=>))
import           "contra-tracer" Control.Tracer (Tracer (..))
import           Data.Foldable (for_)
import           Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable (for)
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           Network.HostName (HostName)
import           Network.Socket (PortNumber)
import           Options.Applicative hiding (Success)
import           System.Exit (die)
import qualified System.Metrics as EKG

import           Streaming


check :: OnMissingKey -> Word -> Trace IO App.TraceMessage -> Formula TemporalEvent Text -> [TemporalEvent] -> IO ()
check omk idx {- Formula index -} tr phi events =
  let result = satisfies omk phi events in
  traceWith tr $ formulaOutcome phi result idx

checkS' :: OnMissingKey -> Bool -> Word -> Trace IO App.TraceMessage -> Formula TemporalEvent Text -> Stream (Of TemporalEvent) IO () -> IO ()
checkS' omk enableProgressDumps idx {- Formula index -} tr phi events = do
  let initial = SatisfyMetrics 0 phi 0
  metrics <- newIORef initial
  withAsync (when enableProgressDumps $ runDisplayProgressDump initial metrics) $ \counterDisplayThread -> do
    r <- satisfiesS omk phi events metrics
    traceWith tr $ formulaOutcome phi r idx
    cancel counterDisplayThread
  where
    runDisplayProgressDump :: SatisfyMetrics TemporalEvent Text -> IORef (SatisfyMetrics TemporalEvent Text) -> IO ()
    runDisplayProgressDump prev counter = do
      next <- readIORef counter
      let eventPerSecond = next.eventsConsumed - prev.eventsConsumed
      let catchupRatio = (fromIntegral (next.currentTimestamp - prev.currentTimestamp) :: Double) / 1_000_000
      traceWith tr $ FormulaProgressDump (fromIntegral eventPerSecond) catchupRatio next.currentFormula idx
      threadDelay 1_000_000 -- 1s
      runDisplayProgressDump next counter

checkOnline :: OnMissingKey
            -> Bool
            -> Trace IO App.TraceMessage
            -> TemporalEventDurationMicrosec
            -> Word
            -> FailureMode
            -> IngestMode
            -> [FilePath]
            -> [Formula TemporalEvent Text]
            -> IO ()
checkOnline omk enableProgressDumps tr eventDuration retentionMs failureMode ingestMode files phis = do
  ing <- mkIngestor (fromIntegral retentionMs)
  for_ files (ingestFileThreaded ing failureMode ingestMode)
  forConcurrently_ (zip [0..] phis) $ \(idx, phi) -> mkIngestorReader ing >>= \reader -> forever $ do
    traceWith tr $ FormulaStartCheck phi idx
    checkS' omk enableProgressDumps idx tr phi (readS reader eventDuration)

checkOffline :: OnMissingKey
             -> Trace IO App.TraceMessage
             -> TemporalEventDurationMicrosec
             -> FilePath
             -> [Formula TemporalEvent Text]
             -> IO ()
checkOffline omk tr eventDuration file phis = do
  events <- read file eventDuration
  forConcurrently_ (zip [0..] phis) $ \(idx, phi) ->
    check omk idx tr phi events
  threadDelay 200_000 -- Give the tracer a grace period to output the logs to whatever backend

setupTraceDispatcher :: Maybe FilePath -> IO (Trace IO App.TraceMessage)
setupTraceDispatcher optTraceDispatcherConfigFile = do
  stdTr <- standardTracer
  configReflection <- emptyConfigReflection
  cfg <- fromMaybe defaultTraceConfig <$> traverse (`readConfigurationWithDefault` defaultTraceConfig) optTraceDispatcherConfigFile
  ekgStore <- EKG.newStore
  ekgTrace <- ekgTracer cfg ekgStore
  tr <- mkCardanoTracer @App.TraceMessage stdTr mempty (Just ekgTrace) ["ReCon"]
  prometheusSimpleTr <- mkCardanoTracer @TracePrometheusSimple stdTr mempty Nothing ["ReCon"]
  configureTracers configReflection cfg [tr]
  configureTracers configReflection cfg [prometheusSimpleTr]
  for_ (prometheusSimple cfg) $ \ps -> do
    runPrometheusSimple (Tracer (traceWith prometheusSimpleTr)) ekgStore ps >>= link
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

    -- This backend can only be used globally, i.e. will always apply to the namespace root.
    -- Multiple definitions, especially with differing ports, are considered a *misconfiguration*.
    prometheusSimple :: TraceConfig -> Maybe (Bool, Maybe HostName, PortNumber)
    prometheusSimple cfg =
      listToMaybe [ (noSuff, mHost, portNo)
                    | options                              <- Map.elems cfg.tcOptions
                    , ConfBackend backends'                <- options
                    , PrometheusSimple noSuff mHost portNo <- backends'
                    ]




main :: IO ()
main = do
  setLocaleEncoding utf8
  options <- execParser opts
  ctx <- Map.toList . fromMaybe Map.empty <$> for options.context (readPropValues >=> dieOnYamlException)
  putStrLn "Context:"
  print ctx
  formulas <- readFormulas options.formulas (Context { interpDomain = ctx, varKinds = Map.empty }) Parser.name >>= dieOnYamlException
  for_ (fmap (\phi -> (phi, checkFormula mempty phi)) formulas) $ \case
    (phi, e : es) -> die $
      Text.unpack $
           "Formula "
        <> prettyFormula phi Prec.Universe
        <> " is syntactically invalid:\n"
        <> Text.unlines (fmap (("— " <>) . prettyError) (e : es))
    (_, []) -> pure ()
  let formulas' = fmap (interpTimeunit (\u -> timeunitToMicrosecond options.timeunit u `div` fromIntegral options.duration)) formulas
  tr <- setupTraceDispatcher options.traceDispatcherCfg
  case options.mode of
    Offline -> do
      file <- case options.traces of
        [x] -> pure x
        _   -> die "Only exactly one trace file is supported in 'offline' mode"
      checkOffline options.onMissingKey tr options.duration file formulas'
    Online -> do
      checkOnline
        options.onMissingKey
        options.enableProgressDumps
        tr
        options.duration
        options.retention
        RethrowExceptions
        (if options.enableSeekToEnd then FromFileEnd else FromFileStart)
        options.traces
        formulas'
  where
    dieOnYamlException :: forall a. Either YamlReadError a -> IO a
    dieOnYamlException (Left exc) = die (Text.unpack exc)
    dieOnYamlException (Right ok) = pure ok
