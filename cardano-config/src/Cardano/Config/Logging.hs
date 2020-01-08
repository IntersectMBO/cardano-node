{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Config.Logging
  ( LoggingLayer (..)
  , LoggingFlag (..)
  , LoggingConfiguration (..)
  , createLoggingFeature
  , createLoggingFeatureCLI
  , loggingCardanoFeatureInit
  , loggingCLIConfiguration
  -- re-exports
  , Trace
  , Configuration
  , LoggerName
  , Severity (..)
  , mkLOMeta
  , LOMeta (..)
  , LOContent (..)
  -- CLI argument parser
  , LoggingCLIArguments (..)
  ) where

import           Cardano.Prelude hiding (trace)

import qualified Control.Concurrent.Async as Async
import           Control.Exception (IOException, catch)
import           Control.Exception.Safe (MonadCatch)

import           Cardano.BM.Backend.Aggregation (plugin)
import           Cardano.BM.Backend.Editor (plugin)
import           Cardano.BM.Backend.EKGView (plugin)
import           Cardano.BM.Backend.Monitoring (plugin)
import qualified Cardano.BM.Backend.Switchboard as Switchboard
import           Cardano.BM.Configuration (Configuration)
import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Counters (readCounters)
import qualified Cardano.BM.Configuration.Model as Config
import           Cardano.BM.Data.Backend (Backend, BackendKind)
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.LogItem ( LOContent (..), LOMeta (..),
                    LoggerName, PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Data.SubTrace
import qualified Cardano.BM.Observer.Monadic as Monadic
import qualified Cardano.BM.Observer.STM as Stm
import           Cardano.BM.Plugin (loadPlugin)
import           Cardano.BM.Setup (setupTrace_, shutdown)
import           Cardano.BM.Trace (Trace, appendName, traceNamedObject)
import qualified Cardano.BM.Trace as Trace
import           Cardano.Shell.Lib (GeneralException (..))
import           Cardano.Shell.Types (CardanoFeature (..))

import           Cardano.Config.Types (CardanoConfiguration(..), ConfigYamlFilePath(..),
                                       CardanoEnvironment, NodeCLI(..), NodeConfiguration(..),
                                       parseNodeConfiguration)


--------------------------------------------------------------------------------
-- Loggging feature
--------------------------------------------------------------------------------

--------------------------------
-- Configuration
--------------------------------

data LoggingConfiguration = LoggingConfiguration
  { lpConfiguration      :: !Configuration
  , recordMetrics        :: !Bool
  }

--------------------------------
-- Layer
--------------------------------

-- | The LoggingLayer interface that we can expose.
-- We want to do this since we want to be able to mock out any function tied to logging.
--
-- The good side of this is that _each function has it's own effects_
-- and that is ideal for tracking the functions effects and constraining
-- the user (programmer) of those function to use specific effects in them.
-- https://github.com/input-output-hk/cardano-sl/blob/develop/util/src/Pos/Util/Log/LogSafe.hs
data LoggingLayer = LoggingLayer
  { llBasicTrace :: forall m. (MonadIO m) => Trace m Text
  , llLogDebug :: forall m a. (MonadIO m, Show a) => Trace m a -> a -> m ()
  , llLogInfo :: forall m a. (MonadIO m, Show a) => Trace m a -> a -> m ()
  , llLogNotice :: forall m a. (MonadIO m, Show a) => Trace m a -> a -> m ()
  , llLogWarning :: forall m a. (MonadIO m, Show a) => Trace m a -> a -> m ()
  , llLogError :: forall m a. (MonadIO m, Show a) => Trace m a -> a -> m ()
  , llAppendName :: forall m a. (Show a) => LoggerName -> Trace m a -> Trace m a
  , llBracketMonadIO :: forall a t. (Show a) => Trace IO a -> Severity -> Text -> IO t -> IO t
  , llBracketMonadM
      :: forall m a t. (MonadCatch m, MonadIO m, Show a)
      => Trace m a -> Severity -> Text -> m t -> m t
  , llBracketMonadX
      :: forall m a t. (MonadIO m, Show a) => Trace m a -> Severity -> Text -> m t -> m t
  , llBracketStmIO :: forall a t. (Show a) => Trace IO a -> Severity -> Text -> STM t -> IO t
  , llBracketStmLogIO
      :: forall a t. (Show a)
      => Trace IO a -> Severity -> Text -> STM (t,[(LOMeta, LOContent a)]) -> IO t
  , llConfiguration :: Configuration
  , llAddBackend :: Backend Text -> BackendKind -> IO ()
  }

--------------------------------
-- Feature
--------------------------------

-- | CLI specific data structure.
data LoggingCLIArguments = LoggingCLIArguments
  { logConfigFile :: !(Maybe FilePath)
  , captureMetrics :: !Bool
  }

data LoggingFlag = LoggingEnabled | LoggingDisabled
  deriving (Eq, Show)

-- | Interpret main logging CLI controls into a tuple of:
--   - a designation of whether logging was disabled,
--   - a valid 'LoggingConfiguration' (still necessary, even if logging was disabled)
loggingCLIConfiguration :: Maybe FilePath -> Bool -> IO (LoggingFlag, LoggingConfiguration)
loggingCLIConfiguration mfp captureMetrics' =
    case mfp of
      Nothing ->
        fmap (LoggingDisabled,) $ LoggingConfiguration <$> basicConfig <*> pure captureMetrics'
      Just fp ->
        fmap (LoggingEnabled,) $ LoggingConfiguration <$> readConfig fp <*> pure captureMetrics'
  where
    basicConfig :: IO Configuration
    basicConfig = do
      c <- Config.empty
      Config.setMinSeverity c Info
      return c
    readConfig :: FilePath -> IO Configuration
    readConfig fp =
      catch (Config.setup fp) $ \(_ :: IOException) -> do
        putTextLn "Cannot find the logging configuration file at location."
        throwIO $ FileNotFoundException fp

createLoggingFeatureCLI
  :: CardanoEnvironment -> CardanoConfiguration -> IO (LoggingLayer, CardanoFeature)
createLoggingFeatureCLI _ cc = do
    (disabled', loggingConfiguration) <- loggingCLIConfiguration
                                           (ccLogConfig cc)
                                           (ccLogMetrics cc)

    -- we construct the layer
    (loggingLayer, cleanUpLogging) <- loggingCardanoFeatureInit disabled' loggingConfiguration


    -- we construct the cardano feature
    let cardanoFeature = CardanoFeature
                            { featureName = "LoggingMonitoringFeature"
                            , featureStart = liftIO . void $ pure loggingLayer
                            , featureShutdown = liftIO $ cleanUpLogging loggingLayer
                            }

    -- we return both
    pure (loggingLayer, cardanoFeature)

createLoggingFeature
  :: CardanoEnvironment -> NodeCLI -> IO (LoggingLayer, CardanoFeature)
createLoggingFeature _ nCli = do
    -- we parse any additional configuration if there is any
    -- We don't know where the user wants to fetch the additional
    -- configuration from, it could be from
    -- the filesystem, so we give him the most flexible/powerful context, @IO@.
    --
    -- Currently we parse outside the features since we want to have a complete
    -- parser for __every feature__.
    nc <- parseNodeConfiguration . unConfigPath $ configFp nCli
    let logConfigFp = if ncLoggingSwitch nc then Just . unConfigPath $ configFp nCli else Nothing

    (disabled', loggingConfiguration) <- loggingCLIConfiguration
                                           logConfigFp
                                           (ncLogMetrics nc)

    -- we construct the layer
    (loggingLayer, cleanUpLogging) <- loggingCardanoFeatureInit disabled' loggingConfiguration


    -- we construct the cardano feature
    let cardanoFeature = CardanoFeature
                            { featureName = "LoggingMonitoringFeature"
                            , featureStart = liftIO . void $ pure loggingLayer
                            , featureShutdown = liftIO $ cleanUpLogging loggingLayer
                            }

    -- we return both
    pure (loggingLayer, cardanoFeature)

-- | Initialize `LoggingCardanoFeature`
loggingCardanoFeatureInit :: LoggingFlag -> LoggingConfiguration -> IO (LoggingLayer, LoggingLayer -> IO())
loggingCardanoFeatureInit disabled' conf = do

  let logConfig = lpConfiguration conf
  (baseTrace, switchBoard) <- setupTrace_ logConfig "cardano"

  let trace = case disabled' of
                LoggingEnabled -> baseTrace
                LoggingDisabled -> Trace.nullTracer

  Config.getGUIport logConfig >>= \p ->
      if p > 0
      then Cardano.BM.Backend.Editor.plugin logConfig trace switchBoard
               >>= loadPlugin switchBoard
      else pure ()
  Config.getEKGport logConfig >>= \p ->
      if p > 0
      then Cardano.BM.Backend.EKGView.plugin logConfig trace switchBoard
                >>= loadPlugin switchBoard
      else pure ()

  Cardano.BM.Backend.Aggregation.plugin logConfig trace switchBoard
      >>= loadPlugin switchBoard
  Cardano.BM.Backend.Monitoring.plugin logConfig trace switchBoard
      >>= loadPlugin switchBoard

  -- record node metrics
  if recordMetrics conf
    then startCapturingMetrics baseTrace
    else pure ()

  -- Construct the logging layer.
  let initLogging
        :: LoggingLayer
      initLogging =
              LoggingLayer
                 { llBasicTrace = Trace.natTrace liftIO trace
                 , llLogDebug = Trace.logDebug
                 , llLogInfo = Trace.logInfo
                 , llLogNotice = Trace.logNotice
                 , llLogWarning = Trace.logWarning
                 , llLogError = Trace.logError
                 , llAppendName = Trace.appendName
                 , llBracketMonadIO = Monadic.bracketObserveIO logConfig
                 , llBracketMonadM = Monadic.bracketObserveM logConfig
                 , llBracketMonadX = Monadic.bracketObserveX logConfig
                 , llBracketStmIO = Stm.bracketObserveIO logConfig
                 , llBracketStmLogIO = Stm.bracketObserveLogIO logConfig
                 , llConfiguration = logConfig
                 , llAddBackend = Switchboard.addExternalBackend switchBoard
                 }

  -- Cleanup function which shuts down the switchboard.
  let cleanupLogging :: LoggingLayer -> IO ()
      cleanupLogging _ = shutdown switchBoard

  pure (initLogging, cleanupLogging)

startCapturingMetrics :: Trace IO Text -> IO ()
startCapturingMetrics trace0 = do
  let trace = appendName "node-metrics" trace0
      counters = [MemoryStats, ProcessStats, NetStats, IOStats]
  _ <- Async.async $ forever $ do
              cts <- readCounters (ObservableTraceSelf counters)
              traceCounters trace cts
              threadDelay 30000000   -- 30 seconds
  pure ()
 where
  traceCounters :: forall m a. MonadIO m => Trace m a -> [Counter] -> m ()
  traceCounters _tr [] = return ()
  traceCounters tr (c@(Counter _ct cn cv) : cs) = do
    mle <- mkLOMeta Notice Confidential
    traceNamedObject tr (mle, LogValue (nameCounter c <> "." <> cn) cv)
    traceCounters tr cs
