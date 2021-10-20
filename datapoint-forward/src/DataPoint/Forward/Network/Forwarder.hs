{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module DataPoint.Forward.Network.Forwarder
  ( connectToAcceptor
  -- | Export this function for Mux purpose.
  , forwardDataPoints
  , forwardDataPointsResp
  ) where

import           Codec.CBOR.Term (Term)
import qualified Codec.Serialise as CBOR
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.IOManager (IOManager)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                                                             noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, simpleSingletonVersions)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket)
import           Ouroboros.Network.Socket (connectToNode, nullNetworkConnectTracers)

import           DataPoint.Forward.Configuration (ForwarderConfiguration (..), HowToConnect (..))
import           DataPoint.Forward.Utils
import qualified DataPoint.Forward.Protocol.Forwarder as Forwarder
import qualified DataPoint.Forward.Protocol.Codec as Forwarder

connectToAcceptor
  :: IOManager
  -> ForwarderConfiguration
  -> DataPointStore
  -> IO ()
connectToAcceptor iomgr config@ForwarderConfiguration{acceptorEndpoint} dpStore =
  doConnectToAcceptor (localSnocket iomgr) (localAddressFromPath localPipe) noTimeLimitsHandshake app
 where
  LocalPipe localPipe = acceptorEndpoint
  app =
    OuroborosApplication $ \_connectionId _shouldStopSTM ->
      [ MiniProtocol
          { miniProtocolNum    = MiniProtocolNum 1
          , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
          , miniProtocolRun    = forwardDataPoints config dpStore
          }
      ]

doConnectToAcceptor
  :: Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'InitiatorMode addr LBS.ByteString IO () Void
  -> IO ()
doConnectToAcceptor snocket address timeLimits app =
  connectToNode
    snocket
    unversionedHandshakeCodec
    timeLimits
    (cborTermVersionDataCodec unversionedProtocolDataCodec)
    nullNetworkConnectTracers
    acceptableVersion
    (simpleSingletonVersions
       UnversionedProtocol
       UnversionedProtocolData
       app)
    Nothing
    address

forwardDataPoints
  :: ForwarderConfiguration
  -> DataPointStore
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
forwardDataPoints config dpStore =
  InitiatorProtocolOnly $
    MuxPeerRaw $ \channel ->
      runPeer
        (forwarderTracer config)
        (Forwarder.codecDataPointForward CBOR.encode CBOR.decode
                                         CBOR.encode CBOR.decode)
        channel
        (Forwarder.dataPointForwarderPeer $ readFromStore dpStore)

forwardDataPointsResp
  :: ForwarderConfiguration
  -> DataPointStore
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
forwardDataPointsResp config dpStore =
  ResponderProtocolOnly $
    MuxPeerRaw $ \channel ->
      runPeer
        (forwarderTracer config)
        (Forwarder.codecDataPointForward CBOR.encode CBOR.decode
                                         CBOR.encode CBOR.decode)
        channel
        (Forwarder.dataPointForwarderPeer $ readFromStore dpStore)
