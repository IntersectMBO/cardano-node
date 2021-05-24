{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Tracer.Acceptors
  ( runAcceptors
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent (ThreadId, killThread, myThreadId)
import           Control.Concurrent.Async (async, asyncThreadId, wait, waitAnyCancel)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Control.Exception (SomeException, try)
import           Control.Monad (void)
import           Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef (IORef, newIORef, readIORef)
import           Data.HashMap.Strict ((!))
import           Data.Text (Text, pack)
import           Data.Time.Clock (secondsToNominalDiffTime)
import           Data.Void (Void)
import qualified Network.Socket as Socket
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath,
                                            localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..), ConnectionId (..),
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
import           System.IO.Unsafe (unsafePerformIO)

import           Cardano.BM.Data.LogItem (LogObject)

import qualified Trace.Forward.Configuration as TF
import qualified Trace.Forward.Protocol.Type as TF
import           Trace.Forward.LogObject ()
import           Trace.Forward.Network.Acceptor (acceptLogObjects)

import qualified System.Metrics.Configuration as EKGF
import qualified System.Metrics.ReqResp as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetrics)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types (AcceptedItems, LogObjects, Metrics,
                                       addressToNodeId, prepareAcceptedItems)

runAcceptors
  :: TracerConfig
  -> AcceptedItems
  -> IO ()
runAcceptors config@TracerConfig{..} acceptedItems = do
  stopEKG <- newIORef False
  stopTF  <- newIORef False

  -- Temporary fill 'tidVar' using current 'ThreadId'. Later it will be
  -- replaced by the real 'ThreadId' from 'serverAsync' (see below).
  tmpTId <- myThreadId
  tidVar :: TVar ThreadId <- newTVarIO tmpTId

  let configs = mkAcceptorsConfigs config stopEKG stopTF

  try (runAcceptors' acceptAt configs tidVar acceptedItems) >>= \case
    Left (e :: SomeException) -> do
      -- There is some problem (probably the connection was dropped).
      putStrLn $ "cardano-tracer, runAcceptors problem: " <> show e
      -- Explicitly stop 'serverAsync'.
      killThread =<< readTVarIO tidVar
      runAcceptors config acceptedItems
    Right _ -> return ()

mkAcceptorsConfigs
  :: TracerConfig
  -> IORef Bool
  -> IORef Bool
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration (LogObject Text)
     )
mkAcceptorsConfigs TracerConfig{..} stopEKG stopTF = (ekgConfig, tfConfig)
 where
  ekgConfig =
    EKGF.AcceptorConfiguration
      { EKGF.acceptorTracer    = nullTracer
      , EKGF.forwarderEndpoint = forEKGF acceptAt
      , EKGF.requestFrequency  = secondsToNominalDiffTime ekgRequestFreq
      , EKGF.whatToRequest     = EKGF.GetAllMetrics
      , EKGF.actionOnResponse  = print
      , EKGF.shouldWeStop      = stopEKG
      , EKGF.actionOnDone      = putStrLn "EKGF: we are done!"
      }

  tfConfig :: TF.AcceptorConfiguration (LogObject Text)
  tfConfig =
    TF.AcceptorConfiguration
      { TF.acceptorTracer    = nullTracer
      , TF.forwarderEndpoint = forTF acceptAt
      , TF.whatToRequest     = TF.GetLogObjects loRequestNum
      , TF.actionOnReply     = print
      , TF.shouldWeStop      = stopTF
      , TF.actionOnDone      = putStrLn "TF: we are done!"
      }

  forTF (LocalPipe p)        = TF.LocalPipe p
  forTF (RemoteSocket h p)   = TF.RemoteSocket (pack h) (fromIntegral p)

  forEKGF (LocalPipe p)      = EKGF.LocalPipe p
  forEKGF (RemoteSocket h p) = EKGF.RemoteSocket (pack h) (fromIntegral p)

runAcceptors'
  :: RemoteAddr
  -> (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration (LogObject Text))
  -> TVar ThreadId
  -> AcceptedItems
  -> IO ()
runAcceptors' endpoint configs tidVar acceptedItems = withIOManager $ \iocp -> do
  case endpoint of
    LocalPipe localPipe -> do
      let snock = localSnocket iocp localPipe
          addr  = localAddressFromPath localPipe
      doListenToForwarder snock addr noTimeLimitsHandshake configs tidVar acceptedItems
    RemoteSocket host port -> do
      listenAddress:_ <- Socket.getAddrInfo Nothing (Just host) (Just $ show port)
      let snock = socketSnocket iocp
          addr  = Socket.addrAddress listenAddress
      doListenToForwarder snock addr timeLimitsHandshake configs tidVar acceptedItems

doListenToForwarder
  :: (Ord addr, Show addr)
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration (LogObject Text))
  -> TVar ThreadId
  -> AcceptedItems
  -> IO ()
doListenToForwarder snocket
                    address
                    timeLimits
                    (ekgConfig, tfConfig)
                    tidVar
                    acceptedItems = do
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
        (SomeResponderApplication $ acceptorApp
          [ (runEKGAcceptor        ekgConfig acceptedItems, 1)
          , (runLogObjectsAcceptor tfConfig  acceptedItems, 2)
          ]
        )
      )
      nullErrorPolicies
      $ \_ serverAsync -> do
        -- Store 'serverAsync' to be able to kill it later.
        atomically $ modifyTVar' tidVar $ const (asyncThreadId serverAsync)
        wait serverAsync -- Block until async exception.
  void $ waitAnyCancel [nsAsync, clAsync]
 where
  acceptorApp protocols =
    OuroborosApplication $ \connectionId _shouldStopSTM ->
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = protocol connectionId
         }
      | (protocol, num) <- protocols
      ]

runEKGAcceptor
  :: Show addr
  => EKGF.AcceptorConfiguration
  -> AcceptedItems
  -> ConnectionId addr
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runEKGAcceptor ekgConfig acceptedItems connId = do
  let (_, _, (ekgStore, localStore)) =
        unsafePerformIO $ prepareStores acceptedItems connId
  acceptEKGMetrics ekgConfig ekgStore localStore

runLogObjectsAcceptor
  :: Show addr
  => TF.AcceptorConfiguration (LogObject Text)
  -> AcceptedItems
  -> ConnectionId addr
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runLogObjectsAcceptor tfConfig acceptedItems connId = do
  let (niStore, loQueue, _) =
        unsafePerformIO $ prepareStores acceptedItems connId
  acceptLogObjects tfConfig loQueue niStore

prepareStores
  :: Show addr
  => AcceptedItems
  -> ConnectionId addr
  -> IO (TF.NodeInfoStore, LogObjects, Metrics)
prepareStores acceptedItems ConnectionId{..} = do
  -- Remote address of the node is unique identifier, from the tracer's point of view.
  let nodeId = addressToNodeId $ show remoteAddress
  prepareAcceptedItems nodeId acceptedItems
  items <- readIORef acceptedItems
  return $ items ! nodeId

-- We need it for 'TF.AcceptorConfiguration a' (in this example it is 'LogObject Text').
instance ShowProxy (LogObject Text)
