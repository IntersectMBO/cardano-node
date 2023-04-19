{-# LANGUAGE DataKinds #-}

module Cardano.Tracer.Acceptors.Server
  ( runAcceptorsServer
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent.Async (race_, wait)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import           Data.Word (Word32)

import           Cardano.Logging (TraceObject)
import           Cardano.Logging.Version (ForwardingVersion (..), ForwardingVersionData (..),
                   forwardingCodecCBORTerm, forwardingVersionCodec)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                   MiniProtocolNum (..), MuxMode (..), OuroborosApplication (..),
                   RunMiniProtocol (..), miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                   codecHandshake, noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion,
                   simpleSingletonVersions)
import           Ouroboros.Network.Snocket (LocalAddress, LocalSocket, Snocket,
                   localAddressFromPath, localSnocket, makeLocalBearer)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..), ConnectionId (..),
                   SomeResponderApplication (..), cleanNetworkMutableState, newNetworkMutableState,
                   nullNetworkServerTracers, withServerNode)
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetricsResp)

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Run.DataPoint.Acceptor (acceptDataPointsResp)
import           Trace.Forward.Run.TraceObject.Acceptor (acceptTraceObjectsResp)

import           Cardano.Tracer.Acceptors.Utils (notifyAboutNodeDisconnected,
                   prepareDataPointRequestor, prepareMetricsStores, removeDisconnectedNode)
import qualified Cardano.Tracer.Configuration as TC
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.TraceObjects (traceObjectsHandler)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Utils (connIdToNodeId)

runAcceptorsServer
  :: TracerEnv
  -> FilePath
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration TraceObject
     , DPF.AcceptorConfiguration
     )
  -> IO ()
runAcceptorsServer tracerEnv p (ekgConfig, tfConfig, dpfConfig) = withIOManager $ \iocp -> do
  traceWith (teTracer tracerEnv) $ TracerSockListen p
  doListenToForwarder
    (localSnocket iocp)
    (localAddressFromPath p)
    (TC.networkMagic $ teConfig tracerEnv)
    noTimeLimitsHandshake $
    -- Please note that we always run all the supported protocols,
    -- there is no mechanism to disable some of them.
    appResponder
      [ (runEKGAcceptor          tracerEnv ekgConfig errorHandler, 1)
      , (runTraceObjectsAcceptor tracerEnv tfConfig  errorHandler, 2)
      , (runDataPointsAcceptor   tracerEnv dpfConfig errorHandler, 3)
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
  errorHandler connId = do
    removeDisconnectedNode tracerEnv connId
    notifyAboutNodeDisconnected tracerEnv connId

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
            makeLocalBearer
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
  :: TracerEnv
  -> EKGF.AcceptorConfiguration
  -> (ConnectionId LocalAddress -> IO ())
  -> ConnectionId LocalAddress
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runEKGAcceptor tracerEnv ekgConfig errorHandler connId =
  acceptEKGMetricsResp
    ekgConfig
    (prepareMetricsStores tracerEnv connId)
    (errorHandler connId)

runTraceObjectsAcceptor
  :: TracerEnv
  -> TF.AcceptorConfiguration TraceObject
  -> (ConnectionId LocalAddress -> IO ())
  -> ConnectionId LocalAddress
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runTraceObjectsAcceptor tracerEnv tfConfig errorHandler connId =
  acceptTraceObjectsResp
    tfConfig
    (traceObjectsHandler tracerEnv $ connIdToNodeId connId)
    (errorHandler connId)

runDataPointsAcceptor
  :: TracerEnv
  -> DPF.AcceptorConfiguration
  -> (ConnectionId LocalAddress -> IO ())
  -> ConnectionId LocalAddress
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runDataPointsAcceptor tracerEnv dpfConfig errorHandler connId =
  acceptDataPointsResp
    dpfConfig
    (prepareDataPointRequestor tracerEnv connId)
    (errorHandler connId)
