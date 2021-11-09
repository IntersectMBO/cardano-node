{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Tracer.Acceptors.Run
  ( runAcceptors
  , runAcceptorsWithBrake
  ) where

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import "contra-tracer" Control.Tracer (nullTracer)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (secondsToNominalDiffTime)

import           Cardano.Logging (TraceObject)

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TF
import qualified Trace.Forward.Protocol.TraceObject.Type as TF

import qualified System.Metrics.Configuration as EKGF
import qualified System.Metrics.ReqResp as EKGF

import           Cardano.Tracer.Acceptors.Client (runAcceptorsClient)
import           Cardano.Tracer.Acceptors.Server (runAcceptorsServer)
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types
import           Cardano.Tracer.Utils (runInLoop)

runAcceptors
  :: TracerConfig
  -> AcceptedMetrics
  -> AcceptedNodeInfo
  -> DataPointAskers
  -> IO ()
runAcceptors config@TracerConfig{network} acceptedMetrics acceptedNodeInfo dpAskers =
  case network of
    AcceptAt (LocalSocket p) -> do
      stopProtocols <- newTVarIO False
      let configs = mkAcceptorsConfigs config p stopProtocols
      runInLoop
        (runAcceptorsServer config p configs acceptedMetrics acceptedNodeInfo dpAskers)
        p 1
    ConnectTo localSocks ->
      forConcurrently_ (NE.nub localSocks) $ \(LocalSocket p) -> do
        stopProtocols <- newTVarIO False
        let configs = mkAcceptorsConfigs config p stopProtocols
        runInLoop
          (runAcceptorsClient config p configs acceptedMetrics acceptedNodeInfo dpAskers)
          p 1

runAcceptorsWithBrake
  :: TracerConfig
  -> AcceptedMetrics
  -> AcceptedNodeInfo
  -> DataPointAskers
  -> TVar Bool
  -> IO ()
runAcceptorsWithBrake config@TracerConfig{network}
                      acceptedMetrics acceptedNodeInfo dpAskers stopProtocols =
  case network of
    AcceptAt (LocalSocket p) -> do
      let configs = mkAcceptorsConfigs config p stopProtocols
      runInLoop
        (runAcceptorsServer config p configs acceptedMetrics acceptedNodeInfo dpAskers)
        p 1
    ConnectTo localSocks ->
      forConcurrently_ (NE.nub localSocks) $ \(LocalSocket p) -> do
        let configs = mkAcceptorsConfigs config p stopProtocols
        runInLoop
          (runAcceptorsClient config p configs acceptedMetrics acceptedNodeInfo dpAskers)
          p 1

mkAcceptorsConfigs
  :: TracerConfig
  -> FilePath
  -> TVar Bool
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration TraceObject
     , DPF.AcceptorConfiguration
     )
mkAcceptorsConfigs TracerConfig{ekgRequestFreq, loRequestNum} p stopProtocols =
  ( EKGF.AcceptorConfiguration
      { EKGF.acceptorTracer    = nullTracer
      , EKGF.forwarderEndpoint = EKGF.LocalPipe p
      , EKGF.requestFrequency  = secondsToNominalDiffTime $ fromMaybe 1.0 ekgRequestFreq
      , EKGF.whatToRequest     = EKGF.GetAllMetrics
      , EKGF.shouldWeStop      = stopProtocols
      }
  , TF.AcceptorConfiguration
      { TF.acceptorTracer    = nullTracer
      , TF.forwarderEndpoint = p
      , TF.whatToRequest     = TF.NumberOfTraceObjects $ fromMaybe 100 loRequestNum
      , TF.shouldWeStop      = stopProtocols
      }
  , DPF.AcceptorConfiguration
      { DPF.acceptorTracer    = nullTracer
      , DPF.forwarderEndpoint = p
      , DPF.shouldWeStop      = stopProtocols
      }
  )
