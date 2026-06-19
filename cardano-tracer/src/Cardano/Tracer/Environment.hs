module Cardano.Tracer.Environment
  ( TracerEnv (..)
  ) where

import           Cardano.Logging.Types
import           Cardano.Timeseries.Component (TimeseriesHandle)
import           Cardano.Tracer.Configuration
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
  , teTimeseriesHandle      :: !(Maybe TimeseriesHandle)
  }
