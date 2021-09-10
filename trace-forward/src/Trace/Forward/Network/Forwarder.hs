{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trace.Forward.Network.Forwarder
  ( connectToAcceptor
  -- | Export this function for Mux purpose.
  , forwardTraceObjects
  , forwardTraceObjectsResp
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
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Trace.Forward.Configuration (ForwarderConfiguration (..), HowToConnect (..))
import           Trace.Forward.Queue (readItems)
import           Trace.Forward.Utils
import qualified Trace.Forward.Protocol.Forwarder as Forwarder
import qualified Trace.Forward.Protocol.Codec as Forwarder

connectToAcceptor
  :: (CBOR.Serialise lo,
      ShowProxy lo)
  => IOManager
  -> ForwarderConfiguration lo
  -> ForwardSink lo
  -> IO ()
connectToAcceptor iomgr config@ForwarderConfiguration{acceptorEndpoint} sink = do
  let (LocalPipe localPipe) = acceptorEndpoint
      snocket = localSnocket iomgr localPipe
      address = localAddressFromPath localPipe
  doConnectToAcceptor snocket address noTimeLimitsHandshake app
 where
  app =
    OuroborosApplication $ \_connectionId _shouldStopSTM ->
      [ MiniProtocol
          { miniProtocolNum    = MiniProtocolNum 1
          , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
          , miniProtocolRun    = forwardTraceObjects config sink
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

forwardTraceObjects
  :: (CBOR.Serialise lo,
      ShowProxy lo)
  => ForwarderConfiguration lo
  -> ForwardSink lo
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
forwardTraceObjects config sink =
  InitiatorProtocolOnly $
    MuxPeerRaw $ \channel ->
      runPeer
        (forwarderTracer config)
        (Forwarder.codecTraceForward CBOR.encode CBOR.decode
                                     CBOR.encode CBOR.decode
                                     CBOR.encode CBOR.decode)
        channel
        (Forwarder.traceForwarderPeer $ readItems config sink)

forwardTraceObjectsResp
  :: (CBOR.Serialise lo,
      ShowProxy lo)
  => ForwarderConfiguration lo
  -> ForwardSink lo
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
forwardTraceObjectsResp config sink =
  ResponderProtocolOnly $
    MuxPeerRaw $ \channel ->
      runPeer
        (forwarderTracer config)
        (Forwarder.codecTraceForward CBOR.encode CBOR.decode
                                     CBOR.encode CBOR.decode
                                     CBOR.encode CBOR.decode)
        channel
        (Forwarder.traceForwarderPeer $ readItems config sink)
