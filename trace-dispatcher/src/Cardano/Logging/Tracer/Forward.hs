{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Logging.Tracer.Forward
  (
    forwardTracer
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO,
                     writeTBQueue)
import           Control.Exception (SomeException, try)
import           Control.Monad (forever)
import           Control.Monad.IO.Class
import           Control.Monad.STM (atomically)
import           Control.Tracer (contramap, stdoutTracer)
import qualified Control.Tracer as T
import qualified Data.ByteString.Lazy as LBS
import           Data.Fixed (Pico)
import           Data.IORef
import           Data.Text (Text, pack, unpack)
import           Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import           Data.Void (Void)
import           Data.Word (Word16)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux (MiniProtocol (..),
                     MiniProtocolLimits (..), MiniProtocolNum (..),
                     MuxMode (..), OuroborosApplication (..),
                     RunMiniProtocol (..), miniProtocolLimits, miniProtocolNum,
                     miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec
                     (cborTermVersionDataCodec, noTimeLimitsHandshake,
                     timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned
                     (UnversionedProtocol (..), UnversionedProtocolData (..),
                     unversionedHandshakeCodec, unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version
                     (acceptableVersion, simpleSingletonVersions)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath,
                     localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (connectToNode,
                     nullNetworkConnectTracers)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

import qualified System.Metrics as EKG
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder (forwardEKGMetrics)
import qualified Trace.Forward.Configuration as TF
import           Trace.Forward.Network.Forwarder (forwardLogObjects)

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types

newtype ForwardTracerState = ForwardTracerState {
    ftQueue   :: TBQueue TraceObject
  }

forwardTracer :: forall m. (MonadIO m)
  => TraceConfig
  -> m (Trace m FormattedMessage)
forwardTracer config = do
    tbQueue <- liftIO $ launchForwardersSimple (tcForwarder config)
    stateRef <- liftIO $ newIORef (ForwardTracerState tbQueue)
    pure $ Trace $ T.arrow $ T.emit $ uncurry3 (output stateRef)
  where
    output ::
         IORef ForwardTracerState
      -> LoggingContext
      -> Maybe TraceControl
      -> FormattedMessage
      -> m ()
    output stateRef LoggingContext {} Nothing (FormattedForwarder lo) = liftIO $ do
      st  <- readIORef stateRef
      atomically $ writeTBQueue (ftQueue st) lo
    output stateRef LoggingContext {} (Just Reset) _msg = liftIO $ do
      -- TODO discuss reconfiguration
      pure ()
    output _ lk (Just c@Document {}) (FormattedForwarder lo) = do
      case toHuman lo of
        Just hr -> docIt (Stdout HumanFormat) (FormattedHuman "") (lk, Just c, hr)
        Nothing -> pure ()
      case toMachine lo of
        Just mr -> docIt (Stdout MachineFormat) (FormattedMachine "") (lk, Just c, mr)
        Nothing -> pure ()
    output _stateRef LoggingContext {} _ _a = pure ()

launchForwardersSimple :: RemoteAddr -> IO (TBQueue TraceObject)
launchForwardersSimple endpoint =
  launchForwarders endpoint (ekgConfig, tfConfig)
 where
  ekgConfig :: EKGF.ForwarderConfiguration
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer    = contramap show stdoutTracer
                                    -- TODO: meaning of this
      , EKGF.acceptorEndpoint   = forEKGF endpoint
      , EKGF.reConnectFrequency = 1.0
      , EKGF.actionOnRequest    = const (return ())
      }

  tfConfig :: TF.ForwarderConfiguration TraceObject
  tfConfig =
    TF.ForwarderConfiguration
      { TF.forwarderTracer  = contramap show stdoutTracer
                                  -- TODO: meaning of this
      , TF.acceptorEndpoint = forTF endpoint
      , TF.nodeBasicInfo    = return [("NodeName", "node-1")]
      , TF.actionOnRequest  = const (return ())
      }

  forTF (LocalPipe p)      = TF.LocalPipe p
  forTF (RemoteSocket h p) = TF.RemoteSocket h p

  forEKGF (LocalPipe p)      = EKGF.LocalPipe p
  forEKGF (RemoteSocket h p) = EKGF.RemoteSocket h p

launchForwarders
  :: RemoteAddr
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> IO (TBQueue TraceObject)
launchForwarders endpoint configs =
  try (launchForwarders' endpoint configs) >>= \case
  -- TODO: Can't we get in an infinite loop here?
    Left (_e :: SomeException) ->
      launchForwarders endpoint configs
    Right res -> return res

launchForwarders'
  :: RemoteAddr
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> IO (TBQueue TraceObject)
launchForwarders' endpoint configs = withIOManager $ \iocp -> do
  case endpoint of
    LocalPipe localPipe -> do
      let snocket = localSnocket iocp localPipe
          address = localAddressFromPath localPipe
      doConnectToAcceptor snocket address noTimeLimitsHandshake configs
    RemoteSocket host port -> do
      acceptorAddr:_ <- Socket.getAddrInfo Nothing (Just (unpack host)) (Just (show port))
      let snocket = socketSnocket iocp
          address = Socket.addrAddress acceptorAddr
      doConnectToAcceptor snocket address timeLimitsHandshake configs

doConnectToAcceptor
  :: Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> IO (TBQueue TraceObject)
doConnectToAcceptor snocket address timeLimits (ekgConfig, tfConfig) = do
  tfQueue <- newTBQueueIO 1000000
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
  pure tfQueue  
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

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c

--
-- -- We need it for 'TF.ForwarderConfiguration lo' (in this example it is 'LogObject Text').
-- instance ShowProxy (LogObject Text)
--
-- loWriter :: TBQueue (LogObject Text) -> Maybe Pico -> IO ()
-- loWriter queue benchFillFreq = forever $ do
--   meta <- mkLOMeta Info Public
--   atomically $ writeTBQueue queue (lo meta)
--   threadDelay fillPause
--  where
--   lo :: LOMeta -> LogObject Text
--   lo meta = LogObject "demo.forwarder.LO.1" meta $ LogMessage "demo.forwarder.LogMessage.1"
--
--   fillPause = case benchFillFreq of
--                 Just ff -> toMicroSecs . secondsToNominalDiffTime $ ff
--                 Nothing -> 500000
--
--   toMicroSecs :: NominalDiffTime -> Int
--   toMicroSecs dt = fromEnum dt `div` 1000000
