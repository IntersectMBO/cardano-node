{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Node.Configuration.Logging
  ( LoggingLayer (..)
  , createLoggingLayer
  , shutdownLoggingLayer
  -- re-exports
  , Trace
  , Configuration
  , LoggerName
  , Severity (..)
  , mkLOMeta
  , LOMeta (..)
  , LOContent (..)
  ) where

import           Cardano.Prelude hiding (trace)

import qualified Control.Concurrent.Async as Async
import           Control.Exception.Safe (MonadCatch)
import           Control.Monad.Trans.Except.Extra (catchIOExceptT)

import           Cardano.BM.Backend.Aggregation (plugin)
import           Cardano.BM.Backend.EKGView (plugin)
import           Cardano.BM.Backend.Monitoring (plugin)
import           Cardano.BM.Backend.Switchboard (Switchboard)
import qualified Cardano.BM.Backend.Switchboard as Switchboard
import           Cardano.BM.Backend.TraceForwarder (plugin)
import           Cardano.BM.Configuration (Configuration)
#ifdef UNIX
import qualified Cardano.BM.Configuration.Model as CM
#endif
import qualified Cardano.BM.Configuration as Config
import qualified Cardano.BM.Configuration.Model as Config
import           Cardano.BM.Counters (readCounters)
import           Cardano.BM.Data.Backend (Backend, BackendKind)
import           Cardano.BM.Data.Counter
import           Cardano.BM.Data.LogItem (LOContent (..), LOMeta (..), LoggerName,
                     PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Observable
#ifdef UNIX
import           Cardano.BM.Data.Output
#endif
import           Cardano.BM.Data.Severity (Severity (..))
import           Cardano.BM.Data.SubTrace
import qualified Cardano.BM.Observer.Monadic as Monadic
import qualified Cardano.BM.Observer.STM as Stm
import           Cardano.BM.Plugin (loadPlugin)
#if defined(SYSTEMD)
import           Cardano.BM.Scribe.Systemd (plugin)
#endif
import           Cardano.BM.Setup (setupTrace_, shutdown)
import           Cardano.BM.Trace (Trace, appendName, traceNamedObject)
import qualified Cardano.BM.Trace as Trace

import           Cardano.Config.Git.Rev (gitRev)
import           Cardano.Node.Configuration.POM (NodeConfiguration (..))
import           Cardano.Node.Types

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
  , llSwitchboard :: Switchboard Text
  }

--------------------------------
-- Feature
--------------------------------

-- | Either parse a filepath into a logging 'Configuration',
--   or supply a mute 'Configuration'.
loggingCLIConfiguration
    :: Maybe FilePath
    -> ExceptT ConfigError IO Configuration
loggingCLIConfiguration = maybe emptyConfig readConfig
 where
   readConfig :: FilePath -> ExceptT ConfigError IO Configuration
   readConfig fp =
     catchIOExceptT (Config.setup fp) $ \(_ :: IOException) -> ConfigErrorFileNotFound fp

   emptyConfig :: ExceptT ConfigError IO Configuration
   emptyConfig = liftIO $ do
     c <- Config.empty
     Config.setMinSeverity c Info
     pure c

-- | Create logging feature for `cardano-node`
createLoggingLayer
  :: Text
  -> NodeConfiguration
  -> ExceptT ConfigError IO LoggingLayer
createLoggingLayer ver nodeConfig' = do

  logConfig <- loggingCLIConfiguration $
    if ncLoggingSwitch nodeConfig'
    -- Re-interpret node config again, as logging 'Configuration':
    then Just . unConfigPath $ ncConfigFile nodeConfig'
    else Nothing

  -- adapt logging configuration before setup
  liftIO $ adaptLogConfig nodeConfig' logConfig

  -- These have to be set before the switchboard is set up.
  liftIO $ do
    Config.setTextOption logConfig "appversion" ver
    Config.setTextOption logConfig "appcommit" gitRev

  (baseTrace, switchBoard) <- liftIO $ setupTrace_ logConfig "cardano"

  let loggingEnabled :: Bool
      loggingEnabled = ncLoggingSwitch nodeConfig'
      trace :: Trace IO Text
      trace = if loggingEnabled
              then baseTrace
              else Trace.nullTracer

  when loggingEnabled $ liftIO $
    loggingPreInit nodeConfig' logConfig switchBoard trace

  pure $ mkLogLayer logConfig switchBoard trace
 where
   loggingPreInit
     :: NodeConfiguration
     -> Configuration
     -> Switchboard Text
     -> Trace IO Text
     -> IO ()
   loggingPreInit nodeConfig logConfig switchBoard trace = do
     Config.getEKGBindAddr logConfig >>= \mbEndpoint ->
       when (isJust mbEndpoint) $
         Cardano.BM.Backend.EKGView.plugin logConfig trace switchBoard
           >>= loadPlugin switchBoard

     Config.getForwardTo logConfig >>= \forwardTo ->
       when (isJust forwardTo) $
         Cardano.BM.Backend.TraceForwarder.plugin logConfig trace switchBoard "forwarderMinSeverity"
           >>= loadPlugin switchBoard

     Cardano.BM.Backend.Aggregation.plugin logConfig trace switchBoard
       >>= loadPlugin switchBoard
     Cardano.BM.Backend.Monitoring.plugin logConfig trace switchBoard
       >>= loadPlugin switchBoard

#if defined(SYSTEMD)
     Cardano.BM.Scribe.Systemd.plugin logConfig trace switchBoard "cardano"
       >>= loadPlugin switchBoard
#endif

     when (ncLogMetrics nodeConfig) $
       -- Record node metrics, if configured
       startCapturingMetrics trace

   adaptLogConfig :: NodeConfiguration -> Configuration -> IO ()
   adaptLogConfig nodeConfig =
     liveViewdisablesStdout (ncViewMode nodeConfig)
   liveViewdisablesStdout SimpleView _ = pure ()
#ifdef UNIX
   liveViewdisablesStdout LiveView logConfig = do -- filter out console scribes
     scribes <- CM.getSetupScribes logConfig
     let newscribes = flip filter scribes $ \case
                        (ScribeDefinition StdoutSK _ _ _ _ _ _) -> False
                        (ScribeDefinition StderrSK _ _ _ _ _ _) -> False
                        _ -> True
     CM.setSetupScribes logConfig newscribes
#else
   liveViewdisablesStdout LiveView _ = pure ()
#endif

   mkLogLayer :: Configuration -> Switchboard Text -> Trace IO Text -> LoggingLayer
   mkLogLayer logConfig switchBoard trace =
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
       , llSwitchboard = switchBoard
       }

   startCapturingMetrics :: Trace IO Text -> IO ()
   startCapturingMetrics trace0 = do
     let trace = appendName "node-metrics" trace0
         counters = [MemoryStats, ProcessStats, NetStats, IOStats, GhcRtsStats, SysStats]
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

shutdownLoggingLayer :: LoggingLayer -> IO ()
shutdownLoggingLayer = shutdown . llSwitchboard
