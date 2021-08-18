{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Tracer.Acceptors
  ( runAcceptors
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent.Async (forConcurrently_, race_, wait)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import "contra-tracer" Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (fromMaybe)
import           Data.HashMap.Strict ((!), insert)
import           Data.Time.Clock (secondsToNominalDiffTime)
import           Data.Void (Void)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..), ConnectionId (..),
                                           SomeResponderApplication (..),
                                           cleanNetworkMutableState, connectToNode,
                                           newNetworkMutableState, nullNetworkServerTracers,
                                           nullNetworkConnectTracers, withServerNode)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                                                             noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion,
                                                               simpleSingletonVersions)
import           System.IO.Unsafe (unsafePerformIO)

import           Cardano.Logging (TraceObject)

import qualified Trace.Forward.Configuration as TF
import qualified Trace.Forward.Protocol.Type as TF
import           Trace.Forward.Network.Acceptor (acceptTraceObjects, acceptTraceObjectsInit)
import           Trace.Forward.Utils (runActionInLoop)

import qualified System.Metrics.Configuration as EKGF
import qualified System.Metrics.ReqResp as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetrics, acceptEKGMetricsInit)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.TraceObjects (traceObjectsHandler)
import           Cardano.Tracer.Types

runAcceptors
  :: TracerConfig
  -> AcceptedMetrics
  -> AcceptedNodeInfo
  -> IO ()
runAcceptors config@TracerConfig{acceptAt} acceptedMetrics acceptedNodeInfo =
  forConcurrently_ acceptAt runAcceptorsForNode
 where
  runAcceptorsForNode (LocalSocket p) = do
    stopEKG <- newTVarIO False
    stopTF  <- newTVarIO False
    let acceptorsConfigs = mkAcceptorsConfigs config p stopEKG stopTF
    runActionInLoop
      (runAcceptor
         config
         p
         acceptorsConfigs
         acceptedMetrics
         acceptedNodeInfo)
      (TF.LocalPipe p)
      1

mkAcceptorsConfigs
  :: TracerConfig
  -> FilePath
  -> TVar Bool
  -> TVar Bool
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration TraceObject
     )
mkAcceptorsConfigs TracerConfig{ekgRequestFreq, loRequestNum} p stopEKG stopTF =
  ( EKGF.AcceptorConfiguration
      { EKGF.acceptorTracer    = nullTracer
      , EKGF.forwarderEndpoint = EKGF.LocalPipe p
      , EKGF.requestFrequency  = secondsToNominalDiffTime $ fromMaybe 1.0 ekgRequestFreq
      , EKGF.whatToRequest     = EKGF.GetAllMetrics
      , EKGF.shouldWeStop      = stopEKG
      }
  , TF.AcceptorConfiguration
      { TF.acceptorTracer    = nullTracer
      , TF.forwarderEndpoint = TF.LocalPipe p
      , TF.whatToRequest     = TF.NumberOfTraceObjects $ fromMaybe 100 loRequestNum
      , TF.shouldWeStop      = stopTF
      }
  )

runAcceptor
  :: TracerConfig
  -> FilePath
  -> (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration TraceObject)
  -> AcceptedMetrics
  -> AcceptedNodeInfo
  -> IO ()
runAcceptor config@TracerConfig{connectMode} p (ekgConfig, tfConfig) acceptedMetrics acceptedNodeInfo =
  withIOManager $ \iocp -> do
    let snock = localSnocket iocp p
        addr  = localAddressFromPath p
    case connectMode of
      Initiator ->
        doConnectToForwarder snock addr noTimeLimitsHandshake $
          appInitiator
            [ (runEKGAcceptorInit ekgConfig acceptedMetrics, 1)
            , (runTraceObjectsAcceptorInit config tfConfig acceptedNodeInfo, 2)
            ]
      Responder ->
        doListenToAcceptor snock addr noTimeLimitsHandshake $
          appResponder
            [ (runEKGAcceptor ekgConfig acceptedMetrics, 1)
            , (runTraceObjectsAcceptor config tfConfig acceptedNodeInfo, 2)
            ]
 where
  appResponder protocols =
    OuroborosApplication $ \connectionId _shouldStopSTM ->
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = protocol connectionId
         }
      | (protocol, num) <- protocols
      ]

  appInitiator protocols =
    OuroborosApplication $ \connectionId _shouldStopSTM ->
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = protocol connectionId
         }
      | (protocol, num) <- protocols
      ]

doConnectToForwarder
  :: Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'InitiatorMode addr LBS.ByteString IO () Void
  -> IO ()
doConnectToForwarder snocket address timeLimits app =
  connectToNode
    snocket
    unversionedHandshakeCodec
    timeLimits
    (cborTermVersionDataCodec unversionedProtocolDataCodec)
    nullNetworkConnectTracers
    acceptableVersion
    (simpleSingletonVersions
       UnversionedProtocol
       UnversionedProtocolData app
    )
    Nothing
    address

doListenToAcceptor
  :: Ord addr
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
  -> IO ()
doListenToAcceptor snocket address timeLimits app = do
  networkState <- newNetworkMutableState
  race_ (cleanNetworkMutableState networkState)
        $ withServerNode
            snocket
            nullNetworkServerTracers
            networkState
            (AcceptedConnectionsLimit maxBound maxBound 0)
            address
            unversionedHandshakeCodec
            timeLimits
            (cborTermVersionDataCodec unversionedProtocolDataCodec)
            acceptableVersion
            (simpleSingletonVersions
              UnversionedProtocol
              UnversionedProtocolData
              (SomeResponderApplication app)
            )
            nullErrorPolicies
            $ \_ serverAsync -> do
              wait serverAsync -- Block until async exception.

runEKGAcceptor
  :: Show addr
  => EKGF.AcceptorConfiguration
  -> AcceptedMetrics
  -> ConnectionId addr
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runEKGAcceptor ekgConfig acceptedMetrics connId = do
  let (ekgStore, localStore) = unsafePerformIO $ prepareMetricsStores acceptedMetrics connId
  acceptEKGMetrics ekgConfig ekgStore localStore

runTraceObjectsAcceptor
  :: Show addr
  => TracerConfig
  -> TF.AcceptorConfiguration TraceObject
  -> AcceptedNodeInfo
  -> ConnectionId addr
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runTraceObjectsAcceptor config tfConfig acceptedNodeInfo connId = do
  let nodeId = connIdToNodeId connId
  acceptTraceObjects
    tfConfig
    (traceObjectsHandler config nodeId acceptedNodeInfo)
    (nodeInfoHandler nodeId acceptedNodeInfo)

runEKGAcceptorInit
  :: Show addr
  => EKGF.AcceptorConfiguration
  -> AcceptedMetrics
  -> ConnectionId addr
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
runEKGAcceptorInit ekgConfig acceptedMetrics connId = do
  let (ekgStore, localStore) = unsafePerformIO $ prepareMetricsStores acceptedMetrics connId
  acceptEKGMetricsInit ekgConfig ekgStore localStore

runTraceObjectsAcceptorInit
  :: Show addr
  => TracerConfig
  -> TF.AcceptorConfiguration TraceObject
  -> AcceptedNodeInfo
  -> ConnectionId addr
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
runTraceObjectsAcceptorInit config tfConfig acceptedNodeInfo connId = do
  let nodeId = connIdToNodeId connId
  acceptTraceObjectsInit
    tfConfig
    (traceObjectsHandler config nodeId acceptedNodeInfo)
    (nodeInfoHandler nodeId acceptedNodeInfo)

prepareMetricsStores
  :: Show addr
  => AcceptedMetrics
  -> ConnectionId addr
  -> IO Metrics
prepareMetricsStores acceptedMetrics connId = do
  let nodeId = connIdToNodeId connId
  prepareAcceptedMetrics nodeId acceptedMetrics
  metrics <- readTVarIO acceptedMetrics
  return $ metrics ! nodeId

-- | Node's info is required for many parts of cardano-tracer.
--   But some of these parts may be inactive (yet) when node's info
--   is accepted, so we have to store it.
nodeInfoHandler
  :: NodeId
  -> AcceptedNodeInfo
  -> TF.NodeInfo
  -> IO ()
nodeInfoHandler nodeId acceptedNodeInfo ni = atomically $
  modifyTVar' acceptedNodeInfo $ insert nodeId ni
