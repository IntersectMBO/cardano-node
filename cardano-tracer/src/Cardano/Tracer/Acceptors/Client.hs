{-# LANGUAGE DataKinds #-}

module Cardano.Tracer.Acceptors.Client
  ( runAcceptorsClient
  ) where

import           Codec.CBOR.Term (Term)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import           Data.Word (Word32)

import           Cardano.Logging (TraceObject)
import           Cardano.Logging.Version (ForwardingVersion (..), ForwardingVersionData (..),
                   forwardingCodecCBORTerm, forwardingVersionCodec)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
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
                   localAddressFromPath, localSnocket)
import           Ouroboros.Network.Socket (ConnectionId (..), connectToNode,
                   nullNetworkConnectTracers)
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetricsInit)

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Run.DataPoint.Acceptor (acceptDataPointsInit)
import           Trace.Forward.Run.TraceObject.Acceptor (acceptTraceObjectsInit)

import           Cardano.Tracer.Acceptors.Utils (notifyAboutNodeDisconnected,
                   prepareDataPointRequestor, prepareMetricsStores, removeDisconnectedNode)
import qualified Cardano.Tracer.Configuration as TC
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Handlers.Logs.TraceObjects (traceObjectsHandler)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Utils (connIdToNodeId)

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

doConnectToForwarder
  :: Snocket IO LocalSocket LocalAddress
  -> LocalAddress
  -> Word32
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> OuroborosApplication 'InitiatorMode LocalAddress LBS.ByteString IO () Void
  -> IO ()
doConnectToForwarder snocket address netMagic timeLimits app =
  connectToNode
    snocket
    mempty -- LocalSocket does not require to be configured
    (codecHandshake forwardingVersionCodec)
    timeLimits
    (cborTermVersionDataCodec forwardingCodecCBORTerm)
    nullNetworkConnectTracers
    acceptableVersion
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
  -> ConnectionId LocalAddress
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
runEKGAcceptorInit tracerEnv ekgConfig errorHandler connId =
  acceptEKGMetricsInit
    ekgConfig
    (prepareMetricsStores tracerEnv connId)
    (errorHandler connId)

runTraceObjectsAcceptorInit
  :: TracerEnv
  -> TF.AcceptorConfiguration TraceObject
  -> (ConnectionId LocalAddress -> IO ())
  -> ConnectionId LocalAddress
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
runTraceObjectsAcceptorInit tracerEnv tfConfig errorHandler connId =
  acceptTraceObjectsInit
    tfConfig
    (traceObjectsHandler tracerEnv $ connIdToNodeId connId)
    (errorHandler connId)

runDataPointsAcceptorInit
  :: TracerEnv
  -> DPF.AcceptorConfiguration
  -> (ConnectionId LocalAddress -> IO ())
  -> ConnectionId LocalAddress
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
runDataPointsAcceptorInit tracerEnv dpfConfig errorHandler connId =
  acceptDataPointsInit
    dpfConfig
    (prepareDataPointRequestor tracerEnv connId)
    (errorHandler connId)
