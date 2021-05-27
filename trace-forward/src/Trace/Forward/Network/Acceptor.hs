{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Trace.Forward.Network.Acceptor
  ( listenToForwarder
  -- | Export this function for Mux purpose.
  , acceptLogObjects
  ) where

import           Codec.CBOR.Term (Term)
import qualified Codec.Serialise as CBOR
import           Control.Concurrent.Async (async, wait, waitAnyCancel)
import           Control.Concurrent.STM.TBQueue (TBQueue)
import           Control.Monad (void, unless)
import           Control.Monad.Class.MonadSTM.Strict (StrictTVar, atomically, modifyTVar,
                                                      newEmptyTMVarIO, newTVarIO, putTMVar,
                                                      readTVar, retry)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor ((<&>))
import           Data.IORef (atomicModifyIORef', readIORef)
import qualified Data.Text as T
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
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion,
                                                               simpleSingletonVersions)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import qualified Trace.Forward.Protocol.Acceptor as Acceptor
import qualified Trace.Forward.Protocol.Codec as Acceptor
import           Trace.Forward.Protocol.Limits (byteLimitsTraceForward, timeLimitsTraceForward)
import           Trace.Forward.Protocol.Type
import           Trace.Forward.Queue (logObjectsFromReply, writeLogObjectsToQueue)
import           Trace.Forward.Configuration (AcceptorConfiguration (..), HowToConnect (..))

listenToForwarder
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> TBQueue lo
  -> NodeInfoStore
  -> IO ()
listenToForwarder config@AcceptorConfiguration{..} loQueue niStore = withIOManager $ \iocp ->
  case forwarderEndpoint of
    LocalPipe localPipe -> do
      let snocket = localSnocket iocp localPipe
          address = localAddressFromPath localPipe
      doListenToForwarder snocket address noTimeLimitsHandshake app
    RemoteSocket host port -> do
      listenAddress:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      let snocket = socketSnocket iocp
          address = Socket.addrAddress listenAddress
      doListenToForwarder snocket address timeLimitsHandshake app
 where
  app = OuroborosApplication $ \_connectionId _shouldStopSTM ->
          [ MiniProtocol
              { miniProtocolNum    = MiniProtocolNum 1
              , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
              , miniProtocolRun    = acceptLogObjects config loQueue niStore
              }
          ]

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

acceptLogObjects
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> TBQueue lo
  -> NodeInfoStore
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
acceptLogObjects config loQueue niStore =
  ResponderProtocolOnly $
    MuxPeerRaw $ \channel -> do
      sv <- newEmptyTMVarIO
      siblingVar <- newTVarIO 2
      (r, trailing) <-
        runPeerWithLimits
          (acceptorTracer config)
          (Acceptor.codecTraceForward CBOR.encode CBOR.decode
                                      CBOR.encode CBOR.decode
                                      CBOR.encode CBOR.decode)
          (byteLimitsTraceForward (fromIntegral . LBS.length))
          timeLimitsTraceForward
          channel
          (Acceptor.traceAcceptorPeer $
            acceptorActions config loQueue niStore True False)
      atomically $ putTMVar sv r
      waitSibling siblingVar
      return ((), trailing)
 where
  waitSibling :: StrictTVar IO Int -> IO ()
  waitSibling cntVar = do
    atomically $ modifyTVar cntVar (\a -> a - 1)
    atomically $ do
      cnt <- readTVar cntVar
      unless (cnt == 0) retry

acceptorActions
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> TBQueue lo
  -> NodeInfoStore
  -> Bool
  -> Bool
  -> Acceptor.TraceAcceptor lo IO ()
acceptorActions config@AcceptorConfiguration{..} loQueue niStore askForNI False =
  -- We can send request for the node's basic info or for the new 'LogObject's.
  -- But request for node's info should be sent only once (in the beginning of session).
  if askForNI
    then
      Acceptor.SendMsgNodeInfoRequest $ \reply -> do
        atomicModifyIORef' niStore $ const $ (reply, ())
        readIORef shouldWeStop <&> acceptorActions config loQueue niStore False
    else
      Acceptor.SendMsgRequest TokBlocking whatToRequest $ \reply -> do
        writeLogObjectsToQueue reply loQueue
        actionOnReply $ logObjectsFromReply reply
        readIORef shouldWeStop <&> acceptorActions config loQueue niStore False

acceptorActions AcceptorConfiguration{..} _ _ _ True =
  Acceptor.SendMsgDone
    actionOnDone
