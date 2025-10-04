{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Tracer.Acceptors.Server
  ( runAcceptorsServer
  ) where

import "contra-tracer" Control.Tracer (nullTracer)

import           Cardano.Logging (TraceObject)
import qualified Cardano.Logging.Types as Net
import           Cardano.Tracer.Acceptors.Utils
import qualified Cardano.Tracer.Configuration as TC
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.TraceObjects (deregisterNodeId, traceObjectsHandler)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Utils (connIdToNodeId)
import           Ouroboros.Network.Context (ResponderContext (..))
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                   MiniProtocolNum (..), OuroborosApplication (..),
                   OuroborosApplicationWithMinimalCtx, RunMiniProtocol (..), miniProtocolLimits,
                   miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake (Handshake, HandshakeArguments (..))
import qualified Ouroboros.Network.Protocol.Handshake as Handshake
import           Ouroboros.Network.Snocket (LocalAddress, LocalSocket, Snocket,
                   localAddressFromPath, localSnocket, makeLocalBearer, makeSocketBearer,
                   socketSnocket)
import           Ouroboros.Network.Socket (ConnectionId (..),
                   SomeResponderApplication (..))
import qualified Ouroboros.Network.Server.Simple as Server

import           Codec.CBOR.Term (Term)
import           Control.Concurrent.Async (wait)
import qualified Data.ByteString.Lazy as LBS
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as Text
import           Data.Functor (void)
import           Data.Void (Void)
import           Data.Word (Word32)
import qualified Network.Mux as Mux
import qualified Network.Socket as Socket
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetricsResp)

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Run.DataPoint.Acceptor (acceptDataPointsResp)
import           Trace.Forward.Run.TraceObject.Acceptor (acceptTraceObjectsResp)
import           Trace.Forward.Utils.Version (ForwardingVersion (..), ForwardingVersionData (..),
                   forwardingCodecCBORTerm, forwardingVersionCodec)


runAcceptorsServer
  :: TracerEnv
  -> TracerEnvRTView
  -> Net.HowToConnect
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration TraceObject
     , DPF.AcceptorConfiguration
     )
  -> IO ()
runAcceptorsServer tracerEnv tracerEnvRTView howToConnect ( ekgConfig, tfConfig, dpfConfig) =
  withIOManager \iocp -> do
  traceWith (teTracer tracerEnv) $ TracerSockListen (Net.howToConnectString howToConnect)
  case howToConnect of
    Net.LocalPipe p ->
      doListenToForwarderLocal
        (localSnocket iocp)
        (localAddressFromPath p)
        (TC.networkMagic $ teConfig tracerEnv)
        Handshake.noTimeLimitsHandshake $
        -- Please note that we always run all the supported protocols,
        -- there is no mechanism to disable some of them.
        appResponder
          [ (runEKGAcceptor          tracerEnv ekgConfig errorHandler, 1)
          , (runTraceObjectsAcceptor tracerEnv tracerEnvRTView tfConfig errorHandler, 2)
          , (runDataPointsAcceptor   tracerEnv dpfConfig errorHandler, 3)
          ]

    Net.RemoteSocket host port -> do
      listenAddress:|_ <- Socket.getAddrInfo Nothing (Just (Text.unpack host)) (Just (show port))
      doListenToForwarderSocket
        (socketSnocket iocp)
        (Socket.addrAddress listenAddress)
        (TC.networkMagic $ teConfig tracerEnv)
        Handshake.timeLimitsHandshake $
        -- Please note that we always run all the supported protocols,
        -- there is no mechanism to disable some of them.
        appResponder
          [ (runEKGAcceptor          tracerEnv ekgConfig errorHandler, 1)
          , (runTraceObjectsAcceptor tracerEnv tracerEnvRTView tfConfig errorHandler, 2)
          , (runDataPointsAcceptor   tracerEnv dpfConfig errorHandler, 3)
          ]
 where
  appResponder protocolsWithNums =
    OuroborosApplication
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolStart  = Mux.StartEagerly
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = protocol
         }
      | (protocol, num) <- protocolsWithNums
      ]
  errorHandler :: Show addr => ConnectionId addr -> IO ()
  errorHandler connId = do
    deregisterNodeId tracerEnv (connIdToNodeId connId)
    removeDisconnectedNode tracerEnv connId
#if RTVIEW
    notifyAboutNodeDisconnected tracerEnvRTView connId
#endif

doListenToForwarderLocal
  :: Snocket IO LocalSocket LocalAddress
  -> LocalAddress
  -> Word32
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> OuroborosApplicationWithMinimalCtx Mux.ResponderMode LocalAddress LBS.ByteString IO Void ()
  -> IO ()
doListenToForwarderLocal snocket address netMagic timeLimits app = do
  void $ Server.with
      snocket
      makeLocalBearer
      mempty -- LocalSocket does not need to be configured
      address
      HandshakeArguments {
        haHandshakeTracer = nullTracer,
        haBearerTracer = nullTracer,
        haHandshakeCodec = Handshake.codecHandshake forwardingVersionCodec,
        haVersionDataCodec = Handshake.cborTermVersionDataCodec forwardingCodecCBORTerm,
        haAcceptVersion = Handshake.acceptableVersion,
        haQueryVersion = Handshake.queryVersion,
        haTimeLimits = timeLimits
      }
      (Handshake.simpleSingletonVersions
        ForwardingV_1
        (ForwardingVersionData $ NetworkMagic netMagic)
        (\_ -> SomeResponderApplication app)
      )
      $ \_ serverAsync -> wait serverAsync -- Block until async exception.


doListenToForwarderSocket
  :: Snocket IO Socket.Socket Socket.SockAddr
  -> Socket.SockAddr
  -> Word32
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> OuroborosApplicationWithMinimalCtx Mux.ResponderMode Socket.SockAddr LBS.ByteString IO Void ()
  -> IO ()
doListenToForwarderSocket snocket address netMagic timeLimits app = do
  void $ Server.with
      snocket
      makeSocketBearer
      mempty -- LocalSocket does not need to be configured
      address
      HandshakeArguments {
        haHandshakeTracer = nullTracer,
        haBearerTracer = nullTracer,
        haHandshakeCodec = Handshake.codecHandshake forwardingVersionCodec,
        haVersionDataCodec = Handshake.cborTermVersionDataCodec forwardingCodecCBORTerm,
        haAcceptVersion = Handshake.acceptableVersion,
        haQueryVersion = Handshake.queryVersion,
        haTimeLimits = timeLimits
      }
      (Handshake.simpleSingletonVersions
        ForwardingV_1
        (ForwardingVersionData $ NetworkMagic netMagic)
        (\_ -> SomeResponderApplication app)
      )
      $ \_ serverAsync -> wait serverAsync -- Block until async exception.


runEKGAcceptor
  :: Show addr
  => TracerEnv
  -> EKGF.AcceptorConfiguration
  -> (ConnectionId addr -> IO ())
  -> RunMiniProtocol 'Mux.ResponderMode initiatorCtx (ResponderContext addr) LBS.ByteString IO Void ()
runEKGAcceptor tracerEnv ekgConfig errorHandler =
  acceptEKGMetricsResp
    ekgConfig
    (prepareMetricsStores tracerEnv . rcConnectionId)
    (errorHandler . rcConnectionId)

runTraceObjectsAcceptor
  :: Show addr
  => TracerEnv
  -> TracerEnvRTView
  -> TF.AcceptorConfiguration TraceObject
  -> (ConnectionId addr -> IO ())
  -> RunMiniProtocol 'Mux.ResponderMode
                     initiatorCtx
                     (ResponderContext addr)
                     LBS.ByteString IO Void ()
runTraceObjectsAcceptor tracerEnv
  tracerEnvRTView
  tfConfig errorHandler =
  acceptTraceObjectsResp
    tfConfig
    (traceObjectsHandler tracerEnv tracerEnvRTView . connIdToNodeId . rcConnectionId)
    (errorHandler . rcConnectionId)

runDataPointsAcceptor
  :: Show addr
  => TracerEnv
  -> DPF.AcceptorConfiguration
  -> (ConnectionId addr -> IO ())
  -> RunMiniProtocol 'Mux.ResponderMode initiatorCtx (ResponderContext addr) LBS.ByteString IO Void ()
runDataPointsAcceptor tracerEnv dpfConfig errorHandler =
  acceptDataPointsResp
    dpfConfig
    (prepareDataPointRequestor tracerEnv . rcConnectionId)
    (errorHandler . rcConnectionId)
