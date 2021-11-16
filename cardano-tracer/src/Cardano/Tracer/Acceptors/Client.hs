{-# LANGUAGE DataKinds #-}

module Cardano.Tracer.Acceptors.Client
  ( runAcceptorsClient
  ) where

import           Codec.CBOR.Term (Term)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket)
import           Ouroboros.Network.Socket (ConnectionId (..), connectToNode,
                                           nullNetworkConnectTracers)
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
import           Trace.Forward.Run.DataPoint.Acceptor (acceptDataPointsInit)
import           Trace.Forward.Run.TraceObject.Acceptor (acceptTraceObjectsInit)

import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetricsInit)

import           Cardano.Tracer.Acceptors.Utils
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.TraceObjects
import           Cardano.Tracer.Types

runAcceptorsClient
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
runAcceptorsClient config p (ekgConfig, tfConfig, dpfConfig) acceptedMetrics acceptedNodeInfo dpAskers =
  withIOManager $ \iocp ->
    doConnectToForwarder (localSnocket iocp) (localAddressFromPath p) noTimeLimitsHandshake $
      appInitiator
        [ (runEKGAcceptorInit ekgConfig acceptedMetrics, 1)
        , (runTraceObjectsAcceptorInit config tfConfig acceptedNodeInfo, 2)
        , (runDataPointsAcceptorInit dpfConfig dpAskers, 3)
        ]
 where
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
runTraceObjectsAcceptorInit config tfConfig acceptedNodeInfo connId =
  acceptTraceObjectsInit tfConfig $ traceObjectsHandler config (connIdToNodeId connId) acceptedNodeInfo

runDataPointsAcceptorInit
  :: Show addr
  => DPF.AcceptorConfiguration
  -> DataPointAskers
  -> ConnectionId addr
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
runDataPointsAcceptorInit dpfConfig dpAskers connId =
  acceptDataPointsInit dpfConfig $ prepareDataPointAsker dpAskers connId
