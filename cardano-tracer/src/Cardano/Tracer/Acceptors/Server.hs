

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Tracer.Acceptors.Server
  ( runAcceptorsServer
  ) where

import           Cardano.Logging (TraceObject)
import           Cardano.Logging.Version (ForwardingVersion (..), ForwardingVersionData (..),
                   forwardingCodecCBORTerm, forwardingVersionCodec)
#if RTVIEW
import           Cardano.Tracer.Acceptors.Utils (notifyAboutNodeDisconnected,
                   prepareDataPointRequestor, prepareMetricsStores, removeDisconnectedNode)
#else
import           Cardano.Tracer.Acceptors.Utils (prepareDataPointRequestor, prepareMetricsStores,
                   removeDisconnectedNode)
#endif
import qualified Cardano.Tracer.Configuration as TC
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.TraceObjects (deregisterNodeId, traceObjectsHandler)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Utils (connIdToNodeId)
import qualified Network.Mux as Mux
import           Ouroboros.Network.Context (MinimalInitiatorContext (..), ResponderContext (..))
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                   MiniProtocolNum (..), OuroborosApplication (..),
                   RunMiniProtocol (..), miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                   codecHandshake, noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, queryVersion,
                   simpleSingletonVersions)
import           Ouroboros.Network.Snocket (LocalAddress, LocalSocket, Snocket,
                   localAddressFromPath, localSnocket, makeLocalBearer)
import           Ouroboros.Network.Socket (ConnectionId (..), SomeResponderApplication (..))

import           Codec.CBOR.Term (Term)
import           Control.Concurrent.Async (wait)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetricsResp)

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Run.DataPoint.Acceptor (acceptDataPointsResp)
import           Trace.Forward.Run.TraceObject.Acceptor (acceptTraceObjectsResp)
import qualified Ouroboros.Network.Server.Simple as Server
import Ouroboros.Network.Protocol.Handshake (HandshakeArguments(..))
import Data.Functor (void)
import           "contra-tracer" Control.Tracer (nullTracer)

runAcceptorsServer
  :: TracerEnv
  -> TracerEnvRTView
  -> FilePath
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration TraceObject
     , DPF.AcceptorConfiguration
     )
  -> IO ()
runAcceptorsServer tracerEnv tracerEnvRTView p ( ekgConfig, tfConfig, dpfConfig) =
  withIOManager \iocp -> do
  traceWith (teTracer tracerEnv) $ TracerSockListen p
  doListenToForwarder
    (localSnocket iocp)
    (localAddressFromPath p)
    (NetworkMagic $ TC.networkMagic $ teConfig tracerEnv)
    noTimeLimitsHandshake $
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
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = protocol
         , miniProtocolStart  = Mux.StartEagerly
         }
      | (protocol, num) <- protocolsWithNums
      ]
  errorHandler connId = do
    deregisterNodeId tracerEnv (connIdToNodeId connId)
    removeDisconnectedNode tracerEnv connId
#if RTVIEW
    notifyAboutNodeDisconnected tracerEnvRTView connId
#endif

doListenToForwarder
  :: Snocket IO LocalSocket LocalAddress
  -> LocalAddress
  -> NetworkMagic
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> OuroborosApplication 'Mux.ResponderMode
                          (MinimalInitiatorContext LocalAddress)
                          (ResponderContext LocalAddress)
                          LBS.ByteString IO Void ()
  -> IO ()
doListenToForwarder snocket address netMagic timeLimits app = do
  Server.with
    snocket
    makeLocalBearer
    mempty -- LocalSocket does not need to be configured
    address
    HandshakeArguments {
      haHandshakeTracer = nullTracer,
      haHandshakeCodec = codecHandshake forwardingVersionCodec,
      haVersionDataCodec = cborTermVersionDataCodec forwardingCodecCBORTerm,
      haAcceptVersion = acceptableVersion,
      haQueryVersion = queryVersion,
      haTimeLimits = timeLimits
    }
    (simpleSingletonVersions
      ForwardingV_1
      (ForwardingVersionData netMagic)
      (\_ -> SomeResponderApplication app)
    ) $ \_ serverAsync -> wait (void serverAsync) -- Block until async exception.

runEKGAcceptor
  :: TracerEnv
  -> EKGF.AcceptorConfiguration
  -> (ConnectionId LocalAddress -> IO ())
  -> RunMiniProtocol 'Mux.ResponderMode initiatorCtx (ResponderContext LocalAddress) LBS.ByteString IO Void ()
runEKGAcceptor tracerEnv ekgConfig errorHandler =
  acceptEKGMetricsResp
    ekgConfig
    (prepareMetricsStores tracerEnv . rcConnectionId)
    (errorHandler . rcConnectionId)

runTraceObjectsAcceptor
  :: TracerEnv
  -> TracerEnvRTView
  -> TF.AcceptorConfiguration TraceObject
  -> (ConnectionId LocalAddress -> IO ())
  -> RunMiniProtocol 'Mux.ResponderMode
                     initiatorCtx
                     (ResponderContext LocalAddress)
                     LBS.ByteString IO Void ()
runTraceObjectsAcceptor tracerEnv
  tracerEnvRTView
  tfConfig errorHandler =
  acceptTraceObjectsResp
    tfConfig
    (traceObjectsHandler tracerEnv tracerEnvRTView . connIdToNodeId . rcConnectionId)
    (errorHandler . rcConnectionId)

runDataPointsAcceptor
  :: TracerEnv
  -> DPF.AcceptorConfiguration
  -> (ConnectionId LocalAddress -> IO ())
  -> RunMiniProtocol 'Mux.ResponderMode initiatorCtx (ResponderContext LocalAddress) LBS.ByteString IO Void ()
runDataPointsAcceptor tracerEnv dpfConfig errorHandler =
  acceptDataPointsResp
    dpfConfig
    (prepareDataPointRequestor tracerEnv . rcConnectionId)
    (errorHandler . rcConnectionId)
