{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Tracer.Acceptors.Run
  ( runAcceptors
  ) where

import           Cardano.Tracer.Acceptors.Client
import           Cardano.Tracer.Acceptors.Server
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Utils

import           Control.Concurrent.Async (forConcurrently_)
import           "contra-tracer" Control.Tracer (Tracer, contramap, nullTracer, stdoutTracer)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (secondsToNominalDiffTime)
import qualified System.Metrics.Configuration as EKGF
import qualified System.Metrics.ReqResp as EKGF

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TOF
import qualified Trace.Forward.Protocol.TraceObject.Type as TOF

-- | Run acceptors for all supported protocols.
--
--   There are two "network modes" for acceptors:
--   1. Server mode, when the tracer accepts connections from any number of nodes.
--   2. Client mode, when the tracer initiates connections to specified number of nodes.
runAcceptors :: TracerEnv -> IO ()
runAcceptors tracerEnv@TracerEnv{teTracer} = do
  traceWith teTracer $ TracerStartedAcceptors network
  case network of
    AcceptAt (LocalSocket p) ->
      -- Run one server that accepts connections from the nodes.
      runInLoop
        (runAcceptorsServer tracerEnv p $ acceptorsConfigs p)
        verbosity p initialPauseInSec
    ConnectTo localSocks ->
      -- Run N clients that initiate connections to the nodes.
      forConcurrently_ (NE.nub localSocks) $ \(LocalSocket p) ->
        runInLoop
          (runAcceptorsClient tracerEnv p $ acceptorsConfigs p)
          verbosity p initialPauseInSec
 where
  TracerConfig{network, ekgRequestFreq, loRequestNum, verbosity} = teConfig tracerEnv

  acceptorsConfigs p =
    ( EKGF.AcceptorConfiguration
        { EKGF.acceptorTracer    = mkVerbosity verbosity
        , EKGF.forwarderEndpoint = EKGF.LocalPipe p
        , EKGF.requestFrequency  = secondsToNominalDiffTime $ fromMaybe 1.0 ekgRequestFreq
        , EKGF.whatToRequest     = EKGF.GetAllMetrics
        , EKGF.shouldWeStop      = teProtocolsBrake tracerEnv
        }
    , TOF.AcceptorConfiguration
        { TOF.acceptorTracer    = mkVerbosity verbosity
        , TOF.forwarderEndpoint = p
        , TOF.whatToRequest     = TOF.NumberOfTraceObjects $ fromMaybe 100 loRequestNum
        , TOF.shouldWeStop      = teProtocolsBrake tracerEnv
        }
    , DPF.AcceptorConfiguration
        { DPF.acceptorTracer    = mkVerbosity verbosity
        , DPF.forwarderEndpoint = p
        , DPF.shouldWeStop      = teProtocolsBrake tracerEnv
        }
    )

  initialPauseInSec = 1

  mkVerbosity :: Show a => Maybe Verbosity -> Tracer IO a
  mkVerbosity (Just Maximum) = contramap show stdoutTracer
  mkVerbosity _              = nullTracer
