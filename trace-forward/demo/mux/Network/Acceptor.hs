{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Acceptor
  ( HowToConnect (..)
  , launchAcceptors
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, wait)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO)
import           Control.Exception (SomeException, try)
import           Control.Monad (void)
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef (newIORef)
import           Data.Text (Text)
import           Data.Void (Void)
import           Data.Word (Word16)
import qualified Network.Socket as Socket
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
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
import qualified System.Metrics as EKG

import qualified Trace.Forward.Acceptor as TF
import qualified Trace.Forward.Configuration as TF
import qualified Trace.Forward.ReqResp as TF
import           Trace.Forward.Network.Acceptor (acceptLogObjects)

import qualified System.Metrics.Acceptor as EKGF
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetrics)
import qualified System.Metrics.ReqResp as EKGF
import           System.Metrics.Store.Acceptor (emptyMetricsLocalStore)

data HowToConnect
  = LocalPipe !FilePath
  | RemoteSocket !String !String

launchAcceptors
  :: HowToConnect
  -> (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration Text)
  -> IO ()
launchAcceptors endpoint configs =
  try (launchAcceptors' endpoint configs) >>= \case
    Left (_e :: SomeException) ->
      launchAcceptors endpoint configs
    Right _ -> return ()

launchAcceptors'
  :: HowToConnect
  -> (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration Text)
  -> IO ()
launchAcceptors' endpoint configs = withIOManager $ \iocp -> do
  case endpoint of
    LocalPipe localPipe -> do
      let snocket = localSnocket iocp localPipe
          address = localAddressFromPath localPipe
      void $ doListenToForwarder snocket address noTimeLimitsHandshake configs
    RemoteSocket host port -> do
      listenAddress:_ <- Socket.getAddrInfo Nothing (Just host) (Just port)
      let snocket = socketSnocket iocp
          address = Socket.addrAddress listenAddress
      void $ doListenToForwarder snocket address timeLimitsHandshake configs

doListenToForwarder
  :: Ord addr
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration Text)
  -> IO Void
doListenToForwarder snocket address timeLimits (ekgConfig, tfConfig) = do
  store <- EKG.newStore
  metricsStore <- newIORef emptyMetricsLocalStore
  logObjectsQueue <- newTBQueueIO 1000000

  networkState <- newNetworkMutableState
  _ <- async $ cleanNetworkMutableState networkState
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
      (SomeResponderApplication $
         acceptorApp [ (acceptEKGMetrics ekgConfig store metricsStore, 1)
                     , (acceptLogObjects tfConfig logObjectsQueue,     2)
                     ]
      )
    )
    nullErrorPolicies
    $ \_ serverAsync -> wait serverAsync -- Block until async exception.
 where
  acceptorApp
    :: [(RunMiniProtocol 'ResponderMode LBS.ByteString IO Void (), Word16)]
    -> OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
  acceptorApp protocols =
    OuroborosApplication $ \_connectionId _shouldStopSTM ->
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = prot
         }
      | (prot, num) <- protocols
      ]

-- We need it for 'TF.AcceptorConfiguration a' (in this example it is 'Text').
instance ShowProxy Text
