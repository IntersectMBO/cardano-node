{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}

module Cardano.Logging
  ( LoggingLayer (..)
  , createLoggingFeature
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

import           Control.Exception.Safe (MonadCatch)

import qualified Cardano.BM.Backend.Switchboard as Switchboard
import           Cardano.BM.Configuration (Configuration)
import qualified Cardano.BM.Configuration as Config
import           Cardano.BM.Data.Backend (Backend)
import           Cardano.BM.Data.LogItem ( LOContent (..)
                                         , LOMeta (..)
                                         , LoggerName
                                         , mkLOMeta
                                         )
import           Cardano.BM.Data.Severity (Severity (..))
import qualified Cardano.BM.Observer.Monadic as Monadic
import qualified Cardano.BM.Observer.STM as Stm
import           Cardano.BM.Setup (setupTrace_, shutdown)
import           Cardano.BM.Trace (Trace)
import qualified Cardano.BM.Trace as Trace
import           Cardano.Shell.Lib (GeneralException (..), doesFileExist)
import           Cardano.Shell.Types ( CardanoFeature (..)
                                     , CardanoFeatureInit (..)
                                     , NoDependency (..)
                                     )

import           Cardano.Config.Types (CardanoConfiguration, CardanoEnvironment)

--------------------------------------------------------------------------------
-- Loggging feature
--------------------------------------------------------------------------------

--------------------------------
-- Configuration
--------------------------------

data LoggingConfiguration = LoggingConfiguration
  { lpConfiguration   :: !Configuration
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
  , llAddBackend :: Backend Text -> Text -> IO ()
  }

--------------------------------
-- Feature
--------------------------------

type LoggingCardanoFeature = CardanoFeatureInit
                               CardanoEnvironment
                               NoDependency
                               CardanoConfiguration
                               LoggingConfiguration
                               LoggingLayer

-- | CLI specific data structure.
data LoggingCLIArguments = LoggingCLIArguments { logConfigFile :: !FilePath }

createLoggingFeature
  :: CardanoEnvironment -> CardanoConfiguration
  -> LoggingCLIArguments -> IO (LoggingLayer, CardanoFeature)
createLoggingFeature
  cardanoEnvironment cardanoConfiguration (LoggingCLIArguments loggingCLIArguments) = do
    -- we parse any additional configuration if there is any
    -- We don't know where the user wants to fetch the additional
    -- configuration from, it could be from
    -- the filesystem, so we give him the most flexible/powerful context, @IO@.
    --
    -- Currently we parse outside the features since we want to have a complete
    -- parser for __every feature__.

    whenM (not <$> doesFileExist loggingCLIArguments) $ do
      putTextLn "Cannot find the logging configuration file at location."
      throwIO $ FileNotFoundException loggingCLIArguments

    loggingConfiguration <- LoggingConfiguration <$> (Config.setup loggingCLIArguments)

    -- we construct the layer
    logCardanoFeat <- loggingCardanoFeatureInit loggingConfiguration

    loggingLayer <- (featureInit logCardanoFeat)
                      cardanoEnvironment
                      NoDependency
                      cardanoConfiguration
                      loggingConfiguration

    -- we construct the cardano feature
    let cardanoFeature = createCardanoFeature logCardanoFeat loggingLayer

    -- we return both
    pure (loggingLayer, cardanoFeature)

-- | Initialize `LoggingCardanoFeature`
loggingCardanoFeatureInit :: LoggingConfiguration -> IO LoggingCardanoFeature
loggingCardanoFeatureInit loggingConfiguration = do

    let logConfig = lpConfiguration loggingConfiguration
    (baseTrace, switchBoard) <- setupTrace_ logConfig "cardano"

    -- Construct the logging layer.
    let initLogging
          :: CardanoEnvironment -> NoDependency
          -> CardanoConfiguration -> LoggingConfiguration -> IO LoggingLayer
        initLogging _ _ _ _ = do
          pure $ LoggingLayer
                   { llBasicTrace = Trace.natTrace liftIO baseTrace
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

    pure $ CardanoFeatureInit
        { featureType = "LoggingMonitoringFeature"
        , featureInit = initLogging
        , featureCleanup = cleanupLogging
        }

-- | Create `CardanoFeature`
createCardanoFeature :: LoggingCardanoFeature -> LoggingLayer -> CardanoFeature
createCardanoFeature loggingCardanoFeature loggingLayer = CardanoFeature
    { featureName = "LoggingMonitoringFeature"
    , featureStart = liftIO . void $ pure loggingLayer
    , featureShutdown = liftIO $ (featureCleanup loggingCardanoFeature) loggingLayer
    }
