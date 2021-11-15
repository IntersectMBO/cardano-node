{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- IMPORTANT: please note that the latest changes in this module are temporary ones.
-- It will be replaced by new dispatcher's code, in "integration PR".

module Cardano.Logging.Tracer.Forward
  (
    forwardTracer
  ) where

import           Control.Monad.IO.Class

import qualified Control.Tracer as T
import           "contra-tracer" Control.Tracer (contramap, stdoutTracer)

import           Ouroboros.Network.IOManager (IOManager)

import qualified System.Metrics as EKG
import qualified System.Metrics.Configuration as EKGF
import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Utils.TraceObject

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types
import           Cardano.Logging.Utils(uncurry3)

---------------------------------------------------------------------------

forwardTracer :: forall m. (MonadIO m)
  => IOManager
  -> TraceConfig
  -> m (Trace m FormattedMessage)
forwardTracer iomgr config = liftIO $ do
  forwardSink <- initForwardSink tfConfig
  store <- EKG.newStore
  EKG.registerGcMetrics store
  launchForwarders iomgr (tcForwarder config) store ekgConfig tfConfig forwardSink
  pure $ Trace $ T.arrow $ T.emit $ uncurry3 (output forwardSink)
 where
  output ::
       ForwardSink TraceObject
    -> LoggingContext
    -> Maybe TraceControl
    -> FormattedMessage
    -> m ()
  output sink LoggingContext {} Nothing (FormattedForwarder lo) = liftIO $
    writeToSink sink lo
  output _sink LoggingContext {} (Just Reset) _msg = liftIO $ do
    pure ()
  output _sink lk (Just c@Document {}) (FormattedForwarder lo) = do
    docIt Forwarder (FormattedHuman False "") (lk, Just c, lo)
  output _sink LoggingContext {} _ _a = pure ()

  LocalSocket p = tcForwarder config

  ekgConfig :: EKGF.ForwarderConfiguration
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer    = contramap show stdoutTracer
      , EKGF.acceptorEndpoint   = EKGF.LocalPipe p
      , EKGF.reConnectFrequency = 1.0
      , EKGF.actionOnRequest    = const $ pure ()
      }

  tfConfig :: TF.ForwarderConfiguration TraceObject
  tfConfig =
    TF.ForwarderConfiguration
      { TF.forwarderTracer       = contramap show stdoutTracer
      , TF.acceptorEndpoint      = p
      , TF.disconnectedQueueSize = 200000
      , TF.connectedQueueSize    = 2000
      }

launchForwarders
  :: IOManager
  -> ForwarderAddr
  -> EKG.Store
  -> EKGF.ForwarderConfiguration
  -> TF.ForwarderConfiguration TraceObject
  -> ForwardSink TraceObject
  -> IO ()
launchForwarders _iomgr _ep _store _ekgConfig _tfConfig _sink =
  -- Temp code, will be replaced by new dispatcher's code.
  return ()
