{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Trace.Forward.Network.Acceptor
  ( listenToForwarder
  -- , runAcceptorPeer
  -- | Export this function for Mux purpose.
  , acceptLogObjects
  ) where

import           Codec.CBOR.Term (Term)
import qualified Codec.Serialise as CBOR
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, wait, waitAnyCancel)
import           Control.Concurrent.STM.TBQueue (TBQueue)
import           Control.Monad (void)
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef (readIORef)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime)
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import qualified Network.Socket as Socket
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits, runPeerWithLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..),
                                           SomeResponderApplication (..),
                                           cleanNetworkMutableState, newNetworkMutableState,
                                           nullNetworkServerTracers, withServerNode)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                                                             noTimeLimitsHandshake,
                                                             timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, simpleSingletonVersions)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import qualified Trace.Forward.Protocol.Acceptor as Acceptor
import qualified Trace.Forward.Protocol.Codec as Acceptor
import           Trace.Forward.Protocol.Type
import           Trace.Forward.Queue (logObjectsFromReply, writeLogObjectsToQueue)
import           Trace.Forward.Configuration (AcceptorConfiguration (..), HowToConnect (..))

listenToForwarder
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> TBQueue lo
  -> IO ()
listenToForwarder config loQueue = withIOManager $ \iocp -> do
  let app = acceptorApp config loQueue
  case forwarderEndpoint config of
    LocalPipe localPipe -> do
      let snocket = localSnocket iocp localPipe
          address = localAddressFromPath localPipe
      doListenToForwarder snocket address noTimeLimitsHandshake app
    RemoteSocket host port -> do
      listenAddress:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      let snocket = socketSnocket iocp
          address = Socket.addrAddress listenAddress
      doListenToForwarder snocket address timeLimitsHandshake app

doListenToForwarder
  :: Ord addr
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
  -> IO ()
doListenToForwarder snocket address timeLimits app = do
  networkState <- newNetworkMutableState
  nsAsync <- async $ cleanNetworkMutableState networkState
  clAsync <- async . void $
    withServerNode
      snocket
      nullNetworkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      address
      unversionedHandshakeCodec
      timeLimits
      (cborTermVersionDataCodec unversionedProtocolDataCodec)
      acceptableVersion
      (simpleSingletonVersions
        UnversionedProtocol
        UnversionedProtocolData
        (SomeResponderApplication app))
      nullErrorPolicies
      $ \_ serverAsync -> wait serverAsync -- Block until async exception.
  void $ waitAnyCancel [nsAsync, clAsync]

acceptorApp
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> TBQueue lo
  -> OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
acceptorApp config loQueue =
  OuroborosApplication $ \_connectionId _shouldStopSTM -> [
    MiniProtocol
      { miniProtocolNum    = MiniProtocolNum 2
      , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
      , miniProtocolRun    = acceptLogObjects config loQueue
      }
  ]

acceptLogObjects
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> TBQueue lo
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
acceptLogObjects config loQueue =
  ResponderProtocolOnly $
    MuxPeer
      (acceptorTracer config)
      (Acceptor.codecTraceForward CBOR.encode CBOR.decode
                                  CBOR.encode CBOR.decode)
      (Acceptor.traceAcceptorPeer $ acceptorActions config loQueue False)

acceptorActions
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> TBQueue lo
  -> Bool
  -> Acceptor.TraceAcceptor lo IO ()
acceptorActions config@AcceptorConfiguration{..} loQueue False =
  Acceptor.SendMsgRequest TokBlocking whatToRequest $ \reply -> do
    writeLogObjectsToQueue reply loQueue
    actionOnReply $ logObjectsFromReply reply
    threadDelay $ toMicroSecs requestFrequency
    readIORef shouldWeStop >>= return . acceptorActions config loQueue
 where
  -- TODO: temporary function, should be rewritten
  -- (we have to take into account actual time of 'actionOnReply'
  -- as well as actual time of getting the reply from the forwarder).
  toMicroSecs :: NominalDiffTime -> Int
  toMicroSecs dt = fromEnum dt `div` 1000000

acceptorActions AcceptorConfiguration{..} _ True =
  Acceptor.SendMsgDone
    actionOnDone

{-
runAcceptorPeer
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> TBQueue lo
  -> IO ((), Maybe LBS.ByteString)
runAcceptorPeer config loQueue =
  runPeerWithLimits
    (acceptorTracer config)
    (Acceptor.codecTraceForward CBOR.encode CBOR.decode
                                CBOR.encode CBOR.decode)
    (Acceptor.byteLimitsTraceForward (fromIntegral . LBS.length))
    Acceptor.timeLimitsTraceForward
    (fromMaybe id (delayChannel <$> outboundDelay) outboundChannel)
    (Acceptor.traceAcceptorPeer $ acceptorActions config loQueue False)
-}
