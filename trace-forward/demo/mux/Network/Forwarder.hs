{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Forwarder
  ( HowToConnect (..)
  , launchForwarders
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, writeTBQueue)
import           Control.Exception (SomeException, try)
import           Control.Monad (forever)
import           Control.Monad.STM (atomically)
import qualified Data.ByteString.Lazy as LBS
import           Data.Fixed (Pico)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           Data.Word (Word16)
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
import qualified System.Metrics as EKG

import           Cardano.BM.Data.LogItem (LogObject (..), LOContent (..), LOMeta (..),
                                          PrivacyAnnotation (..), mkLOMeta)
import           Cardano.BM.Data.Severity (Severity (..))

import qualified Trace.Forward.Configuration as TF
import qualified Trace.Forward.ReqResp as TF
import           Trace.Forward.Network.Forwarder (forwardLogObjects)

import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder (forwardEKGMetrics)
import qualified System.Metrics.ReqResp as EKGF

data HowToConnect
  = LocalPipe !FilePath
  | RemoteSocket !String !String

launchForwarders
  :: HowToConnect
  -> Maybe Pico
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration Text)
  -> IO ()
launchForwarders endpoint benchFillFreq configs =
  try (launchForwarders' endpoint benchFillFreq configs) >>= \case
    Left (_e :: SomeException) -> do
      threadDelay $ toMicroSecs howOftenToReconnect
      launchForwarders endpoint benchFillFreq configs
    Right _ -> return ()
 where
  toMicroSecs :: NominalDiffTime -> Int
  toMicroSecs dt = fromEnum dt `div` 1000000
  -- Actually we could take second frequency as well, they are equal in this demo.
  howOftenToReconnect = EKGF.reConnectFrequency $ fst configs

launchForwarders'
  :: HowToConnect
  -> Maybe Pico
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration Text)
  -> IO ()
launchForwarders' endpoint benchFillFreq configs = withIOManager $ \iocp -> do
  case endpoint of
    LocalPipe localPipe -> do
      let snocket = localSnocket iocp localPipe
          address = localAddressFromPath localPipe
      doConnectToAcceptor snocket address noTimeLimitsHandshake benchFillFreq configs
    RemoteSocket host port -> do
      acceptorAddr:_ <- Socket.getAddrInfo Nothing (Just host) (Just port)
      let snocket = socketSnocket iocp
          address = Socket.addrAddress acceptorAddr
      doConnectToAcceptor snocket address timeLimitsHandshake benchFillFreq configs

doConnectToAcceptor
  :: Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> Maybe Pico
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration Text)
  -> IO ()
doConnectToAcceptor snocket address timeLimits benchFillFreq (ekgConfig, tfConfig) = do
  tfQueue <- newTBQueueIO 1000000
  _ <- async $ loWriter tfQueue benchFillFreq
  store <- EKG.newStore
  EKG.registerGcMetrics store

  connectToNode
    snocket
    unversionedHandshakeCodec
    timeLimits
    (cborTermVersionDataCodec unversionedProtocolDataCodec)
    nullNetworkConnectTracers
    acceptableVersion
    (simpleSingletonVersions
       UnversionedProtocol
       UnversionedProtocolData $
         forwarderApp [ (forwardEKGMetrics ekgConfig store,  1)
                      , (forwardLogObjects tfConfig tfQueue, 2)
                      ]
    )
    Nothing
    address
 where
  forwarderApp
    :: [(RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void, Word16)]
    -> OuroborosApplication 'InitiatorMode addr LBS.ByteString IO () Void
  forwarderApp protocols =
    OuroborosApplication $ \_connectionId _shouldStopSTM ->
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = prot
         }
      | (prot, num) <- protocols
      ]

-- We need it for 'TF.ForwarderConfiguration a' (in this example it is 'Text').
instance ShowProxy Text

loWriter :: TBQueue (LogObject Text) -> Maybe Pico -> IO ()
loWriter queue benchFillFreq = forever $ do
  meta <- mkLOMeta Info Public
  atomically $ writeTBQueue queue (lo meta)
  threadDelay fillPause
 where
  lo :: LOMeta -> LogObject Text
  lo meta = LogObject "demo.forwarder.LO.1" meta $ LogMessage "demo.forwarder.LogMessage.1"

  fillPause = case benchFillFreq of
                Just ff -> toMicroSecs . secondsToNominalDiffTime $ ff
                Nothing -> 500000

  toMicroSecs :: NominalDiffTime -> Int
  toMicroSecs dt = fromEnum dt `div` 1000000
