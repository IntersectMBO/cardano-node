{-# LANGUAGE DataKinds #-}

module Cardano.Tracer.Acceptors.Server
  ( runAcceptorsServer
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent.Async (race_, wait)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (modifyTVar', readTVarIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import           Data.Map.Strict ((!))
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
                                           cleanNetworkMutableState,
                                           newNetworkMutableState, nullNetworkServerTracers,
                                           withServerNode)
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

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Run.DataPoint.Acceptor (acceptDataPointsResp)
import           Trace.Forward.Run.TraceObject.Acceptor (acceptTraceObjectsResp)
import           Trace.Forward.Utils.DataPoint (initDataPointAsker)

import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetrics)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.TraceObjects
import           Cardano.Tracer.Types

runAcceptorsServer
  :: TracerConfig
  -> FilePath
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration TraceObject
     , DPF.AcceptorConfiguration
     )
  -> AcceptedMetrics
  -> AcceptedNodeInfo
  -> DataPointAskers
  -> IO ()
runAcceptorsServer config p (ekgConfig, tfConfig, dpfConfig) acceptedMetrics acceptedNodeInfo dpAskers =
  withIOManager $ \iocp -> do
    doListenToForwarder (localSnocket iocp) (localAddressFromPath p) noTimeLimitsHandshake $
      appResponder
        [ (runEKGAcceptor ekgConfig acceptedMetrics, 1)
        , (runTraceObjectsAcceptor config tfConfig acceptedNodeInfo, 2)
        , (runDataPointsAcceptor dpfConfig dpAskers, 3)
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

doListenToForwarder
  :: Ord addr
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
  -> IO ()
doListenToForwarder snocket address timeLimits app = do
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
  let (ekgStore, localStore) = unsafePerformIO prepareMetricsStores
  acceptEKGMetrics ekgConfig ekgStore localStore
 where
  prepareMetricsStores = do
    let nodeId = connIdToNodeId connId
    prepareAcceptedMetrics nodeId acceptedMetrics
    metrics <- readTVarIO acceptedMetrics
    return $ metrics ! nodeId

runTraceObjectsAcceptor
  :: Show addr
  => TracerConfig
  -> TF.AcceptorConfiguration TraceObject
  -> AcceptedNodeInfo
  -> ConnectionId addr
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runTraceObjectsAcceptor config tfConfig acceptedNodeInfo connId =
  acceptTraceObjectsResp tfConfig $ traceObjectsHandler config (connIdToNodeId connId) acceptedNodeInfo

runDataPointsAcceptor
  :: Show addr
  => DPF.AcceptorConfiguration
  -> DataPointAskers
  -> ConnectionId addr
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runDataPointsAcceptor dpfConfig dpAskers connId = do
  let dpAsker = unsafePerformIO prepareDataPointAsker
  acceptDataPointsResp dpfConfig dpAsker
 where
  prepareDataPointAsker = do
    let nodeId = connIdToNodeId connId
    dpAsker <- initDataPointAsker
    atomically $ modifyTVar' dpAskers $ \askers ->
      if nodeId `M.member` askers
        then M.adjust (const dpAsker) nodeId askers
        else M.insert nodeId dpAsker askers
    return dpAsker
