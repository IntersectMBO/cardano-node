{-# LANGUAGE DataKinds #-}

module Cardano.Tracer.Acceptors.Client
  ( runAcceptorsClient
  ) where

import           Cardano.Logging (TraceObject)
import qualified Cardano.Logging.Types as Net
import           Cardano.Tracer.Acceptors.Utils (prepareDataPointRequestor, prepareMetricsStores,
                   removeDisconnectedNode)
import qualified Cardano.Tracer.Configuration as TC
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.TraceObjects (deregisterNodeId, traceObjectsHandler)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Utils (connIdToNodeId)
import           Ouroboros.Network.Context (MinimalInitiatorContext (..), ResponderContext (..))
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                   MiniProtocolNum (..), OuroborosApplication (..),
                   OuroborosApplicationWithMinimalCtx, RunMiniProtocol (..), miniProtocolLimits,
                   miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                   codecHandshake, noTimeLimitsHandshake, timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, queryVersion,
                   simpleSingletonVersions)
import           Ouroboros.Network.Snocket (LocalAddress, LocalSocket, Snocket,
                   localAddressFromPath, localSnocket, makeLocalBearer, makeSocketBearer,
                   socketSnocket)
import           Ouroboros.Network.Socket (ConnectToArgs (..), ConnectionId (..),
                   HandshakeCallbacks (..), connectToNode, nullNetworkConnectTracers)

import           Codec.CBOR.Term (Term)
import           Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as LBS
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as Text
import           Data.Void (Void, absurd)
import           Data.Word (Word32)
import qualified Network.Mux as Mux
import qualified Network.Socket as Socket
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetricsInit)

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Run.DataPoint.Acceptor (acceptDataPointsInit)
import           Trace.Forward.Run.TraceObject.Acceptor (acceptTraceObjectsInit)
import           Trace.Forward.Utils.Version (ForwardingVersion (..), ForwardingVersionData (..),
                   forwardingCodecCBORTerm, forwardingVersionCodec)

runAcceptorsClient
  :: TracerEnv
  -> TracerEnvRTView
  -> Net.HowToConnect
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration TraceObject
     , DPF.AcceptorConfiguration
     )
  -> IO ()
runAcceptorsClient tracerEnv tracerEnvRTView howToConnect (ekgConfig, tfConfig, dpfConfig) = withIOManager \iocp -> do
  traceWith (teTracer tracerEnv) $ TracerSockConnecting
    (case howToConnect of
       Net.LocalPipe p -> p
       Net.RemoteSocket host port -> Text.unpack host ++ ":" ++ show port)
  case howToConnect of
    Net.LocalPipe p -> do
      doConnectToForwarderLocal
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
    Net.RemoteSocket host port -> do
      listenAddress:|_ <- Socket.getAddrInfo Nothing (Just (Text.unpack host)) (Just (show port))
      doConnectToForwarderSocket
        (socketSnocket iocp)
        (Socket.addrAddress listenAddress)
        (TC.networkMagic $ teConfig tracerEnv)
        timeLimitsHandshake $
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

doConnectToForwarderLocal
  :: Snocket IO LocalSocket LocalAddress
  -> LocalAddress
  -> Word32
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> OuroborosApplication 'Mux.InitiatorMode
                          (MinimalInitiatorContext LocalAddress)
                          (ResponderContext LocalAddress)
                          LBS.ByteString IO () Void
  -> IO ()
doConnectToForwarderLocal snocket address netMagic timeLimits app = do
  done <- connectToNode
    snocket
    makeLocalBearer
    args
    mempty -- LocalSocket does not require to be configured
    (simpleSingletonVersions
       ForwardingV_1
       (ForwardingVersionData $ NetworkMagic netMagic)
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

doConnectToForwarderSocket
  :: Snocket IO Socket.Socket Socket.SockAddr
  -> Socket.SockAddr
  -> Word32
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> OuroborosApplicationWithMinimalCtx
                  Mux.InitiatorMode Socket.SockAddr LBS.ByteString IO () Void
  -> IO ()
doConnectToForwarderSocket snocket address netMagic timeLimits app = do
  done <- connectToNode
    snocket
    makeSocketBearer
    args
    mempty -- LocalSocket does not require to be configured
    (simpleSingletonVersions
       ForwardingV_1
       (ForwardingVersionData $ NetworkMagic netMagic)
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
  :: Show addr
  => TracerEnv
  -> EKGF.AcceptorConfiguration
  -> (ConnectionId addr -> IO ())
  -> RunMiniProtocol 'Mux.InitiatorMode
                     (MinimalInitiatorContext addr)
                     respoinderCtx
                     LBS.ByteString IO () Void
runEKGAcceptorInit tracerEnv ekgConfig errorHandler =
  acceptEKGMetricsInit
    ekgConfig
    (prepareMetricsStores tracerEnv . micConnectionId)
    (errorHandler . micConnectionId)

runTraceObjectsAcceptorInit
  :: Show addr
  => TracerEnv
  -> TracerEnvRTView
  -> TF.AcceptorConfiguration TraceObject
  -> (ConnectionId addr -> IO ())
  -> RunMiniProtocol 'Mux.InitiatorMode
                     (MinimalInitiatorContext addr)
                     responderCtx
                     LBS.ByteString IO () Void
runTraceObjectsAcceptorInit tracerEnv tracerEnvRTView tfConfig errorHandler =
  acceptTraceObjectsInit
    tfConfig
    (traceObjectsHandler tracerEnv tracerEnvRTView . connIdToNodeId . micConnectionId)
    (errorHandler . micConnectionId)

runDataPointsAcceptorInit
  :: Show addr
  => TracerEnv
  -> DPF.AcceptorConfiguration
  -> (ConnectionId addr -> IO ())
  -> RunMiniProtocol 'Mux.InitiatorMode
                     (MinimalInitiatorContext addr)
                     responderCtx
                     LBS.ByteString IO () Void
runDataPointsAcceptorInit tracerEnv dpfConfig errorHandler =
  acceptDataPointsInit
    dpfConfig
    (prepareDataPointRequestor tracerEnv . micConnectionId)
    (errorHandler . micConnectionId)
