{-# LANGUAGE DataKinds #-}

module Cardano.Tracer.Acceptors.Client
  ( runAcceptorsClient
  ) where

import           Cardano.Logging (TraceObject)
import           Cardano.Logging.Version
#if RTVIEW
import           Cardano.Tracer.Acceptors.Utils (notifyAboutNodeDisconnected,
                   prepareDataPointRequestor, prepareMetricsStores, removeDisconnectedNode)
#else
import           Cardano.Tracer.Acceptors.Utils (
                   prepareDataPointRequestor, prepareMetricsStores, removeDisconnectedNode)
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
import           Ouroboros.Network.Socket (ConnectionId (..), ConnectToArgs (..),
                   HandshakeCallbacks (..), connectToNode, nullNetworkConnectTracers)

import           Codec.CBOR.Term (Term)
import           Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void, absurd)
import           Data.Word (Word32)
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetricsInit)

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Run.DataPoint.Acceptor (acceptDataPointsInit)
import           Trace.Forward.Run.TraceObject.Acceptor (acceptTraceObjectsInit)

runAcceptorsClient
  :: TracerEnv
  -> TracerEnvRTView
  -> FilePath
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration TraceObject
     , DPF.AcceptorConfiguration
     )
  -> IO ()
runAcceptorsClient tracerEnv tracerEnvRTView p (ekgConfig, tfConfig, dpfConfig) = withIOManager \iocp -> do
  traceWith (teTracer tracerEnv) $ TracerSockConnecting p
  doConnectToForwarder
    (localSnocket iocp)
    (localAddressFromPath p)
    (TC.networkMagic $ teConfig tracerEnv)
    noTimeLimitsHandshake $
    -- Please note that we always run all the supported protocols,
    -- there is no mechanism to disable some of them.
    appInitiator
      [ (runEKGAcceptorInit          tracerEnv ekgConfig errorHandler, 1)
      , (runTraceObjectsAcceptorInit tracerEnv tracerEnvRTView tfConfig errorHandler, 2)
      , (runDataPointsAcceptorInit   tracerEnv dpfConfig errorHandler, 3)
      ]
 where
  appInitiator protocolsWithNums =
    OuroborosApplication
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = protocol
         }
      | (protocol, num) <- protocolsWithNums
      ]
  errorHandler connId = do
    deregisterNodeId tracerEnv (connIdToNodeId connId)
    removeDisconnectedNode tracerEnv connId
#if RTVIEW
    notifyAboutNodeDisconnected tracerEnvRTView connId
#endif

doConnectToForwarder
  :: Snocket IO LocalSocket LocalAddress
  -> LocalAddress
  -> Word32
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> OuroborosApplication 'Mux.InitiatorMode
                          (MinimalInitiatorContext LocalAddress)
                          (ResponderContext LocalAddress)
                          LBS.ByteString IO () Void
  -> IO ()
doConnectToForwarder snocket address netMagic timeLimits app = do
  done <- connectToNode
    snocket
    makeLocalBearer
    args
    mempty -- LocalSocket does not require to be configured
    (simpleSingletonVersions
       ForwardingV_1
       (ForwardingVersionData (NetworkMagic netMagic) TraceSelectAll)
       (const app)
    )
    Nothing
    address
  case done of
    Left err -> throwIO err
    Right choice -> case choice of
      Left () -> return ()
      Right void -> absurd void
  where
    args = ConnectToArgs {
      ctaHandshakeCodec = codecHandshake forwardingVersionCodec,
      ctaHandshakeTimeLimits = timeLimits,
      ctaVersionDataCodec = cborTermVersionDataCodec forwardingCodecCBORTerm,
      ctaConnectTracers = nullNetworkConnectTracers,
      ctaHandshakeCallbacks = HandshakeCallbacks acceptableVersion queryVersion }

runEKGAcceptorInit
  :: TracerEnv
  -> EKGF.AcceptorConfiguration
  -> (ConnectionId LocalAddress -> IO ())
  -> RunMiniProtocol 'Mux.InitiatorMode
                     (MinimalInitiatorContext LocalAddress)
                     respoinderCtx
                     LBS.ByteString IO () Void
runEKGAcceptorInit tracerEnv ekgConfig errorHandler =
  acceptEKGMetricsInit
    ekgConfig
    (prepareMetricsStores tracerEnv . micConnectionId)
    (errorHandler . micConnectionId)

runTraceObjectsAcceptorInit
  :: TracerEnv
  -> TracerEnvRTView
  -> TF.AcceptorConfiguration TraceObject
  -> (ConnectionId LocalAddress -> IO ())
  -> RunMiniProtocol 'Mux.InitiatorMode
                     (MinimalInitiatorContext LocalAddress)
                     responderCtx
                     LBS.ByteString IO () Void
runTraceObjectsAcceptorInit tracerEnv tracerEnvRTView tfConfig errorHandler =
  acceptTraceObjectsInit
    tfConfig
    (traceObjectsHandler tracerEnv tracerEnvRTView . connIdToNodeId . micConnectionId)
    (errorHandler . micConnectionId)

runDataPointsAcceptorInit
  :: TracerEnv
  -> DPF.AcceptorConfiguration
  -> (ConnectionId LocalAddress -> IO ())
  -> RunMiniProtocol 'Mux.InitiatorMode
                     (MinimalInitiatorContext LocalAddress)
                     responderCtx
                     LBS.ByteString IO () Void
runDataPointsAcceptorInit tracerEnv dpfConfig errorHandler =
  acceptDataPointsInit
    dpfConfig
    (prepareDataPointRequestor tracerEnv . micConnectionId)
    (errorHandler . micConnectionId)
