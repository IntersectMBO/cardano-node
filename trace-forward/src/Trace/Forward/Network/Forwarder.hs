{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Trace.Forward.Network.Forwarder
  ( connectToAcceptor
  ) where

import           Codec.CBOR.Term (Term)
import qualified Codec.Serialise as CBOR
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import qualified Network.Socket as Socket
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                                                             noTimeLimitsHandshake,
                                                             timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, simpleSingletonVersions)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (connectToNode, nullNetworkConnectTracers)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Cardano.BM.Data.LogItem (LogObject)

import           Trace.Forward.Configuration (ForwarderConfiguration (..), HowToConnect (..))
import           Trace.Forward.Queue (readLogObjectsFromQueue)
import qualified Trace.Forward.Protocol.Forwarder as Forwarder
import qualified Trace.Forward.Protocol.Codec as Forwarder

connectToAcceptor
  :: (CBOR.Serialise a,
      ShowProxy a,
      Typeable a)
  => ForwarderConfiguration a
  -> TBQueue (LogObject a)
  -> IO ()
connectToAcceptor config@ForwarderConfiguration{..} loQueue = withIOManager $ \iocp -> do
  let app = forwarderApp config loQueue
  case acceptorEndpoint of
    LocalPipe localPipe -> do
      let snocket = localSnocket iocp localPipe
          address = localAddressFromPath localPipe
      doConnectToAcceptor snocket address noTimeLimitsHandshake app
    RemoteSocket host port -> do
      acceptorAddr:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      let snocket = socketSnocket iocp
          address = Socket.addrAddress acceptorAddr
      doConnectToAcceptor snocket address timeLimitsHandshake app

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

forwarderApp
  :: (CBOR.Serialise a,
      ShowProxy a,
      Typeable a)
  => ForwarderConfiguration a
  -> TBQueue (LogObject a)
  -> OuroborosApplication 'InitiatorMode addr LBS.ByteString IO () Void
forwarderApp config loQueue =
  OuroborosApplication $ \_connectionId _shouldStopSTM ->
    [ MiniProtocol
        { miniProtocolNum    = MiniProtocolNum 2
        , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
        , miniProtocolRun    = forwardLogObjects config loQueue
        }
    ]

forwardLogObjects
  :: (CBOR.Serialise a,
      ShowProxy a,
      Typeable a)
  => ForwarderConfiguration a
  -> TBQueue (LogObject a)
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
forwardLogObjects config loQueue =
  InitiatorProtocolOnly $
    MuxPeer
      (forwarderTracer config)
      (Forwarder.codecTraceForward CBOR.encode CBOR.decode
                                   CBOR.encode CBOR.decode)
      (Forwarder.traceForwarderPeer $ readLogObjectsFromQueue config loQueue)
