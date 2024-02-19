module Cardano.Tracer.Environment
  ( TracerEnv (..)
  ) where

import           Cardano.Logging.Types
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.Notifications.Types
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Types

import           Control.Concurrent.Extra (Lock)

-- | Environment for all functions.
data TracerEnv = TracerEnv
  { teConfig                :: !TracerConfig
  , teConnectedNodes        :: !ConnectedNodes
  , teConnectedNodesNames   :: !ConnectedNodesNames
  , teAcceptedMetrics       :: !AcceptedMetrics
  , teSavedTO               :: !SavedTraceObjects
  , teBlockchainHistory     :: !BlockchainHistory
  , teResourcesHistory      :: !ResourcesHistory
  , teTxHistory             :: !TransactionsHistory
  , teCurrentLogLock        :: !Lock
  , teCurrentDPLock         :: !Lock
  , teEventsQueues          :: !EventsQueues
  , teDPRequestors          :: !DataPointRequestors
  , teProtocolsBrake        :: !ProtocolsBrake
  , teRTViewPageOpened      :: !WebPageStatus
  , teRTViewStateDir        :: !(Maybe FilePath)
  , teTracer                :: !(Trace IO TracerTrace)
  , teReforwardTraceObjects :: !([TraceObject] -> IO ())
  }
