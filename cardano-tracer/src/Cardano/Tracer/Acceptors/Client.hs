{-# LANGUAGE DataKinds #-}

module Cardano.Tracer.Acceptors.Client
  ( runAcceptorsClient
  ) where

import           Cardano.Logging (TraceObject)
import           Cardano.Logging.Version (ForwardingVersion (..), ForwardingVersionData (..),
                   forwardingCodecCBORTerm, forwardingVersionCodec)
import           Cardano.Tracer.Acceptors.Utils (notifyAboutNodeDisconnected,
                   prepareDataPointRequestor, prepareMetricsStores, removeDisconnectedNode)
import qualified Cardano.Tracer.Configuration as TC
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.TraceObjects (traceObjectsHandler)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Utils (connIdToNodeId)
import           Ouroboros.Network.Context (MinimalInitiatorContext (..), ResponderContext (..))
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                   MiniProtocolNum (..), MuxMode (..), OuroborosApplication (..),
                   RunMiniProtocol (..), miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                   codecHandshake, noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, queryVersion,
                   simpleSingletonVersions)
import           Ouroboros.Network.Snocket (LocalAddress, LocalSocket, Snocket,
                   localAddressFromPath, localSnocket, makeLocalBearer)
import           Ouroboros.Network.Socket (ConnectionId (..), HandshakeCallbacks (..),
                   connectToNode, nullNetworkConnectTracers)

import           Codec.CBOR.Term (Term)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import           Data.Word (Word32)
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetricsInit)

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Run.DataPoint.Acceptor (acceptDataPointsInit)
import           Trace.Forward.Run.TraceObject.Acceptor (acceptTraceObjectsInit)

runAcceptorsClient
  :: TracerEnv
  -> FilePath
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration TraceObject
     , DPF.AcceptorConfiguration
     )
  -> IO ()
runAcceptorsClient tracerEnv p (ekgConfig, tfConfig, dpfConfig) = withIOManager $ \iocp -> do
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
      , (runTraceObjectsAcceptorInit tracerEnv tfConfig  errorHandler, 2)
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
    removeDisconnectedNode tracerEnv connId
    notifyAboutNodeDisconnected tracerEnv connId

doConnectToForwarder
  :: Snocket IO LocalSocket LocalAddress
  -> LocalAddress
  -> Word32
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> OuroborosApplication 'InitiatorMode
                          (MinimalInitiatorContext LocalAddress)
                          (ResponderContext LocalAddress)
                          LBS.ByteString IO () Void
  -> IO ()
doConnectToForwarder snocket address netMagic timeLimits app =
  connectToNode
    snocket
    makeLocalBearer
    mempty -- LocalSocket does not require to be configured
    (codecHandshake forwardingVersionCodec)
    timeLimits
    (cborTermVersionDataCodec forwardingCodecCBORTerm)
    nullNetworkConnectTracers
    (HandshakeCallbacks acceptableVersion queryVersion)
    (simpleSingletonVersions
       ForwardingV_1
       (ForwardingVersionData $ NetworkMagic netMagic)
       app
    )
    Nothing
    address

runEKGAcceptorInit
  :: TracerEnv
  -> EKGF.AcceptorConfiguration
  -> (ConnectionId LocalAddress -> IO ())
  -> RunMiniProtocol 'InitiatorMode
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
  -> TF.AcceptorConfiguration TraceObject
  -> (ConnectionId LocalAddress -> IO ())
  -> RunMiniProtocol 'InitiatorMode
                     (MinimalInitiatorContext LocalAddress)
                     responderCtx
                     LBS.ByteString IO () Void
runTraceObjectsAcceptorInit tracerEnv tfConfig errorHandler =
  acceptTraceObjectsInit
    tfConfig
    (traceObjectsHandler tracerEnv . connIdToNodeId . micConnectionId)
    (errorHandler . micConnectionId)

runDataPointsAcceptorInit
  :: TracerEnv
  -> DPF.AcceptorConfiguration
  -> (ConnectionId LocalAddress -> IO ())
  -> RunMiniProtocol 'InitiatorMode
                     (MinimalInitiatorContext LocalAddress)
                     responderCtx
                     LBS.ByteString IO () Void
runDataPointsAcceptorInit tracerEnv dpfConfig errorHandler =
  acceptDataPointsInit
    dpfConfig
    (prepareDataPointRequestor tracerEnv . micConnectionId)
    (errorHandler . micConnectionId)
