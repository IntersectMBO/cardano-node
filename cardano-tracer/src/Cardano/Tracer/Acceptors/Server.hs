{-# LANGUAGE DataKinds #-}

module Cardano.Tracer.Acceptors.Server
  ( runAcceptorsServer
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent.Async (race_, wait)
import           Control.Concurrent.Extra (Lock)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import           Data.Word (Word32)

import           Cardano.Logging (TraceObject)
import           Cardano.Logging.Version (ForwardingVersion (..), ForwardingVersionData (..),
                   forwardingCodecCBORTerm, forwardingVersionCodec)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                   MiniProtocolNum (..), MuxMode (..), OuroborosApplication (..),
                   RunMiniProtocol (..), miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.Snocket (LocalAddress, LocalSocket, Snocket,
                   localAddressFromPath, localSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..), ConnectionId (..),
                   SomeResponderApplication (..), cleanNetworkMutableState,
                   newNetworkMutableState, nullNetworkServerTracers, withServerNode)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                   codecHandshake, noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion,
                   simpleSingletonVersions)
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetricsResp)

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Run.DataPoint.Acceptor (acceptDataPointsResp)
import           Trace.Forward.Run.TraceObject.Acceptor (acceptTraceObjectsResp)

import           Cardano.Tracer.Acceptors.Utils (prepareDataPointRequestor,
                   prepareMetricsStores, removeDisconnectedNode)
import qualified Cardano.Tracer.Configuration as TC
import           Cardano.Tracer.Handlers.Logs.TraceObjects (traceObjectsHandler)
import           Cardano.Tracer.Handlers.RTView.Run (SavedTraceObjects)
import           Cardano.Tracer.Types (AcceptedMetrics, ConnectedNodes, DataPointRequestors)
import           Cardano.Tracer.Utils (connIdToNodeId)

runAcceptorsServer
  :: TC.TracerConfig
  -> FilePath
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration TraceObject
     , DPF.AcceptorConfiguration
     )
  -> ConnectedNodes
  -> AcceptedMetrics
  -> SavedTraceObjects
  -> DataPointRequestors
  -> Lock
  -> IO ()
runAcceptorsServer config p (ekgConfig, tfConfig, dpfConfig) connectedNodes 
                   acceptedMetrics savedTO dpRequestors currentLogLock = withIOManager $ \iocp ->
  doListenToForwarder
    (localSnocket iocp)
    (localAddressFromPath p)
    (TC.networkMagic config)
    noTimeLimitsHandshake $
    -- Please note that we always run all the supported protocols,
    -- there is no mechanism to disable some of them.
    appResponder
      [ (runEKGAcceptor ekgConfig connectedNodes acceptedMetrics errorHandler, 1)
      , (runTraceObjectsAcceptor config tfConfig currentLogLock savedTO errorHandler, 2)
      , (runDataPointsAcceptor dpfConfig connectedNodes dpRequestors errorHandler, 3)
      ]
 where
  appResponder protocolsWithNums =
    OuroborosApplication $ \connectionId _shouldStopSTM ->
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = protocol connectionId
         }
      | (protocol, num) <- protocolsWithNums
      ]
  errorHandler = removeDisconnectedNode connectedNodes acceptedMetrics dpRequestors

doListenToForwarder
  :: Snocket IO LocalSocket LocalAddress
  -> LocalAddress
  -> Word32
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> OuroborosApplication 'ResponderMode LocalAddress LBS.ByteString IO Void ()
  -> IO ()
doListenToForwarder snocket address netMagic timeLimits app = do
  networkState <- newNetworkMutableState
  race_ (cleanNetworkMutableState networkState)
        $ withServerNode
            snocket
            mempty -- LocalSocket does not need to be configured
            nullNetworkServerTracers
            networkState
            (AcceptedConnectionsLimit maxBound maxBound 0)
            address
            (codecHandshake forwardingVersionCodec)
            timeLimits
            (cborTermVersionDataCodec forwardingCodecCBORTerm)
            acceptableVersion
            (simpleSingletonVersions
              ForwardingV_1
              (ForwardingVersionData $ NetworkMagic netMagic)
              (SomeResponderApplication app)
            )
            nullErrorPolicies
            $ \_ serverAsync -> wait serverAsync -- Block until async exception.

runEKGAcceptor
  :: EKGF.AcceptorConfiguration
  -> ConnectedNodes
  -> AcceptedMetrics
  -> (ConnectionId LocalAddress -> IO ())
  -> ConnectionId LocalAddress
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runEKGAcceptor ekgConfig connectedNodes acceptedMetrics errorHandler connId =
  acceptEKGMetricsResp
    ekgConfig
    (prepareMetricsStores connectedNodes acceptedMetrics connId)
    (errorHandler connId)

runTraceObjectsAcceptor
  :: TC.TracerConfig
  -> TF.AcceptorConfiguration TraceObject
  -> Lock
  -> SavedTraceObjects
  -> (ConnectionId LocalAddress -> IO ())
  -> ConnectionId LocalAddress
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runTraceObjectsAcceptor config tfConfig currentLogLock savedTO errorHandler connId =
  acceptTraceObjectsResp
    tfConfig
    (traceObjectsHandler config (connIdToNodeId connId) currentLogLock savedTO)
    (errorHandler connId)

runDataPointsAcceptor
  :: DPF.AcceptorConfiguration
  -> ConnectedNodes
  -> DataPointRequestors
  -> (ConnectionId LocalAddress -> IO ())
  -> ConnectionId LocalAddress
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runDataPointsAcceptor dpfConfig connectedNodes dpRequestors errorHandler connId =
  acceptDataPointsResp
    dpfConfig
    (prepareDataPointRequestor connectedNodes dpRequestors connId)
    (errorHandler connId)
