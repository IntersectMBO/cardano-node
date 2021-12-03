{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Tracer.Acceptors.Run
  ( runAcceptors
  ) where

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Concurrent.Extra (Lock)
import           "contra-tracer" Control.Tracer (Tracer, contramap, nullTracer,
                   stdoutTracer)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (secondsToNominalDiffTime)

import qualified System.Metrics.Configuration as EKGF
import qualified System.Metrics.ReqResp as EKGF

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TOF
import qualified Trace.Forward.Protocol.TraceObject.Type as TOF

import           Cardano.Tracer.Acceptors.Client (runAcceptorsClient)
import           Cardano.Tracer.Acceptors.Server (runAcceptorsServer)
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.Run (SavedTraceObjects)
import           Cardano.Tracer.Types (AcceptedMetrics, ConnectedNodes,
                   DataPointAskers, ProtocolsBrake)
import           Cardano.Tracer.Utils (runInLoop)

-- | Run acceptors for all supported protocols.
--
--   There are two "network modes" for acceptors:
--   1. Server mode, when the tracer accepts connections from any number of nodes.
--   2. Client mode, when the tracer initiates connections to specified number of nodes.
runAcceptors
  :: TracerConfig
  -> ConnectedNodes
  -> AcceptedMetrics
  -> DataPointAskers
  -> ProtocolsBrake
  -> Lock
  -> SavedTraceObjects
  -> IO ()
runAcceptors c@TracerConfig{network, ekgRequestFreq, loRequestNum, verbosity}
             connectedNodes acceptedMetrics dpAskers stopIt currentLogLock savedTO =
  case network of
    AcceptAt (LocalSocket p) ->
      -- Run one server that accepts connections from the nodes.
      runInLoop
        (runAcceptorsServer c p (acceptorsConfigs p) connectedNodes
                            acceptedMetrics dpAskers currentLogLock savedTO)
        verbosity p 1
    ConnectTo localSocks ->
      -- Run N clients that initiate connections to the nodes.
      forConcurrently_ (NE.nub localSocks) $ \(LocalSocket p) ->
        runInLoop
          (runAcceptorsClient c p (acceptorsConfigs p) connectedNodes
                              acceptedMetrics dpAskers currentLogLock savedTO)
          verbosity p 1
 where
  acceptorsConfigs p =
    ( EKGF.AcceptorConfiguration
        { EKGF.acceptorTracer    = mkVerbosity verbosity
        , EKGF.forwarderEndpoint = EKGF.LocalPipe p
        , EKGF.requestFrequency  = secondsToNominalDiffTime $ fromMaybe 1.0 ekgRequestFreq
        , EKGF.whatToRequest     = EKGF.GetAllMetrics
        , EKGF.shouldWeStop      = stopIt
        }
    , TOF.AcceptorConfiguration
        { TOF.acceptorTracer    = mkVerbosity verbosity
        , TOF.forwarderEndpoint = p
        , TOF.whatToRequest     = TOF.NumberOfTraceObjects $ fromMaybe 100 loRequestNum
        , TOF.shouldWeStop      = stopIt
        }
    , DPF.AcceptorConfiguration
        { DPF.acceptorTracer    = mkVerbosity verbosity
        , DPF.forwarderEndpoint = p
        , DPF.shouldWeStop      = stopIt
        }
    )

  mkVerbosity :: Show a => Maybe Verbosity -> Tracer IO a
  mkVerbosity (Just Maximum) = contramap show stdoutTracer
  mkVerbosity _              = nullTracer
