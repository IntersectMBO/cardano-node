{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Tracer.Acceptors.Run
  ( runAcceptors
  ) where

import           Cardano.Logging.Types (TraceObject)
import           Cardano.Logging.Utils (runInLoop)
import           Cardano.Tracer.Acceptors.Client
import           Cardano.Tracer.Acceptors.Server
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment
import           Cardano.Tracer.MetaTrace

import           Control.Concurrent.Chan.Unagi (dupChan)
import           Control.Concurrent.Async (forConcurrently_)
import           Control.Exception (SomeException (..))
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
runAcceptors :: TracerEnv -> TracerEnvRTView -> IO ()
runAcceptors tracerEnv@TracerEnv{teTracer, teInChan = inChan} tracerEnvRTView = do
  traceWith teTracer $ TracerStartedAcceptors network
  case network of
    AcceptAt howToConnect -> let
      -- Run one server that accepts connections from the nodes.
      action :: IO ()
      action = do
        dieOnShutdown =<< dupChan inChan
        runAcceptorsServer tracerEnv tracerEnvRTView howToConnect $ acceptorsConfigs (show howToConnect)

      in runInLoop action (handleOnInterruption howToConnect) initialPauseInSec 10
    
    ConnectTo localSocks ->
      -- Run N clients that initiate connections to the nodes.
      forConcurrently_ (NE.nub localSocks) \howToConnect -> let
        action :: IO ()
        action = runAcceptorsClient tracerEnv tracerEnvRTView howToConnect $ acceptorsConfigs (show howToConnect)

        in do
          dieOnShutdown =<< dupChan inChan
          runInLoop action (handleOnInterruption howToConnect) initialPauseInSec 30
 where
  handleOnInterruption howToConnect (SomeException e)
    | verbosity == Just Minimum = pure ()
    | otherwise                 = traceWith teTracer $ TracerForwardingInterrupted howToConnect $ show e

  TracerConfig{network, ekgRequestFreq, verbosity, ekgRequestFull} = teConfig tracerEnv
  ekgUseFullRequests = fromMaybe False ekgRequestFull

  -- NOTE: The forwarderEndpoint fields may now also contain a TCP socket address.
  --       However, those fields are unused in the context of ouroboros-network mini-protocal application.
  acceptorsConfigs :: FilePath -> (EKGF.AcceptorConfiguration, TOF.AcceptorConfiguration TraceObject, DPF.AcceptorConfiguration)
  acceptorsConfigs p =
    ( EKGF.AcceptorConfiguration
        { EKGF.acceptorTracer    = mkVerbosity verbosity
        , EKGF.forwarderEndpoint = EKGF.LocalPipe p
        , EKGF.requestFrequency  = secondsToNominalDiffTime $ fromMaybe 1.0 ekgRequestFreq
        , EKGF.whatToRequest     = if ekgUseFullRequests then EKGF.GetAllMetrics else EKGF.GetUpdatedMetrics
        , EKGF.shouldWeStop      = teProtocolsBrake tracerEnv
        }
    , TOF.AcceptorConfiguration
        { TOF.acceptorTracer    = mkVerbosity verbosity
        , TOF.whatToRequest     = TOF.NumberOfTraceObjects $ fromMaybe 100 (loRequestNum (teConfig tracerEnv))
        , TOF.shouldWeStop      = teProtocolsBrake tracerEnv
        }
    , DPF.AcceptorConfiguration
        { DPF.acceptorTracer    = mkVerbosity verbosity
        , DPF.shouldWeStop      = teProtocolsBrake tracerEnv
        }
    )

  initialPauseInSec = 1

  mkVerbosity :: Show a => Maybe Verbosity -> Tracer IO a
  mkVerbosity (Just Maximum) = contramap show stdoutTracer
  mkVerbosity _              = nullTracer
