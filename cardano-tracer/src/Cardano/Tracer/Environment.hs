{-# LANGUAGE CPP #-}

module Cardano.Tracer.Environment
  ( TracerEnv (..)
  , TracerEnvRTView (..)
  ) where

import           Cardano.Logging.Types
import           Cardano.Tracer.Configuration
#if RTVIEW
import           Cardano.Tracer.Handlers.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.State.TraceObjects
#endif
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types

import           Control.Concurrent.Extra (Lock)
import           Data.Text (Text)
import           Data.Text.Lazy.Builder (Builder)


-- | Environment for all functions.
data TracerEnv = TracerEnv
  { teConfig                :: !TracerConfig
  , teConnectedNodes        :: !ConnectedNodes
  , teConnectedNodesNames   :: !ConnectedNodesNames
  , teAcceptedMetrics       :: !AcceptedMetrics
  , teCurrentLogLock        :: !Lock
  , teCurrentDPLock         :: !Lock
  , teDPRequestors          :: !DataPointRequestors
  , teProtocolsBrake        :: !ProtocolsBrake
  , teTracer                :: !(Trace IO TracerTrace)
  , teReforwardTraceObjects :: !([TraceObject] -> IO ())
  , teRegistry              :: !HandleRegistry
  , teStateDir              :: !(Maybe FilePath)
  , teMetricsHelp           :: ![(Text, Builder)]
  }

#if RTVIEW
-- | Environment for all functions.
data TracerEnvRTView = TracerEnvRTView
  { teSavedTO               :: !SavedTraceObjects
  , teBlockchainHistory     :: !BlockchainHistory
  , teResourcesHistory      :: !ResourcesHistory
  , teTxHistory             :: !TransactionsHistory
  , teEventsQueues          :: !EventsQueues
  , teRTViewPageOpened      :: !WebPageStatus
  }
#else
data TracerEnvRTView = TracerEnvRTView
#endif
