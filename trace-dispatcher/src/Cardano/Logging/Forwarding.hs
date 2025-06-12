{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Forwarding
  (
    initForwarding
  , initForwardingDelayed
  ) where

import           Cardano.Logging.Types
import           Cardano.Logging.Utils (runInLoop)
import           Cardano.Logging.Version
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (IOManager)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                   MiniProtocolNum (..), OuroborosApplication (..), RunMiniProtocol (..),
                   miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake (HandshakeArguments (..))
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                   codecHandshake, noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, queryVersion,
                   simpleSingletonVersions)
import           Ouroboros.Network.Snocket (MakeBearer, Snocket, localAddressFromPath, localSnocket,
                   makeLocalBearer)
import           Ouroboros.Network.Socket (ConnectToArgs (..),
                   HandshakeCallbacks (..), SomeResponderApplication (..),
                   connectToNode, nullNetworkConnectTracers)
import qualified Ouroboros.Network.Server.Simple as Server

import           Codec.CBOR.Term (Term)
import           Control.Concurrent.Async (async, wait)
import           Control.Exception (throwIO)
import           Control.Monad (void)
import           Control.Monad.IO.Class
import           "contra-tracer" Control.Tracer (Tracer, contramap, nullTracer, stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void, absurd)
import           Data.Word (Word16)
import qualified Network.Mux as Mux
import           System.IO (hPutStrLn, stderr)
import qualified System.Metrics as EKG
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Run.DataPoint.Forwarder
import           Trace.Forward.Run.TraceObject.Forwarder
import           Trace.Forward.Utils.DataPoint
import           Trace.Forward.Utils.TraceObject

initForwarding :: forall m. (MonadIO m)
  => IOManager
  -> TraceOptionForwarder
  -> NetworkMagic
  -> Maybe EKG.Store
  -> Maybe (FilePath, ForwarderMode)
  -> m (ForwardSink TraceObject, DataPointStore)
initForwarding iomgr config magic ekgStore tracerSocketMode = do
  (a, b, kickoffForwarder) <- initForwardingDelayed iomgr config magic ekgStore tracerSocketMode
  liftIO kickoffForwarder
  pure (a, b)

-- We allow for delayed initialization of the forwarding connection by
-- returning an IO action to do so.
initForwardingDelayed :: forall m. (MonadIO m)
  => IOManager
  -> TraceOptionForwarder
  -> NetworkMagic
  -> Maybe EKG.Store
  -> Maybe (FilePath, ForwarderMode)
  -> m (ForwardSink TraceObject, DataPointStore, IO ())
initForwardingDelayed iomgr config magic ekgStore tracerSocketMode = liftIO $ do
  forwardSink <- initForwardSink tfConfig handleOverflow
  dpStore <- initDataPointStore
  let
    kickoffForwarder = launchForwarders
      iomgr
      magic
      ekgConfig
      tfConfig
      dpfConfig
      ekgStore
      forwardSink
      dpStore
      tracerSocketMode
      maxReconnectDelay
  pure (forwardSink, dpStore, kickoffForwarder)
 where
  p = maybe "" fst tracerSocketMode
  connSize = tofConnQueueSize config
  disconnSize = tofDisconnQueueSize config
  verbosity = tofVerbosity config
  maxReconnectDelay = tofMaxReconnectDelay config

  ekgConfig :: EKGF.ForwarderConfiguration
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer    = mkTracer verbosity
      , EKGF.acceptorEndpoint   = EKGF.LocalPipe p
      , EKGF.reConnectFrequency = 1.0
      , EKGF.actionOnRequest    = const $ pure ()
      , EKGF.useDummyForwarder  = False
      }

  tfConfig :: TF.ForwarderConfiguration TraceObject
  tfConfig =
    TF.ForwarderConfiguration
      { TF.forwarderTracer       = mkTracer verbosity
      , TF.acceptorEndpoint      = p
      , TF.disconnectedQueueSize = disconnSize
      , TF.connectedQueueSize    = connSize
      }

  dpfConfig :: DPF.ForwarderConfiguration
  dpfConfig =
    DPF.ForwarderConfiguration
      { DPF.forwarderTracer  = mkTracer verbosity
      , DPF.acceptorEndpoint = p
      }

  mkTracer :: Show a => Verbosity -> Tracer IO a
  mkTracer Maximum = contramap show stdoutTracer
  mkTracer Minimum = nullTracer

-- | this function is called when the queue is full.
--  It is called with the list of messages that were dropped.
-- It writes an error message on stderr
handleOverflow :: [TraceObject] -> IO ()
handleOverflow [] = pure ()
handleOverflow msgs =
    let lengthM = length msgs
        beginning = toTimestamp (head msgs)
        end = toTimestamp (last msgs)
        msg = "TraceObject queue overflowed. Dropped " <> show lengthM <>
                " messages from " <> show beginning <> " to " <> show end
    in hPutStrLn stderr msg

launchForwarders
  :: IOManager
  -> NetworkMagic
  -> EKGF.ForwarderConfiguration
  -> TF.ForwarderConfiguration TraceObject
  -> DPF.ForwarderConfiguration
  -> Maybe EKG.Store
  -> ForwardSink TraceObject
  -> DataPointStore
  -> Maybe (FilePath, ForwarderMode)
  -> Word
  -> IO ()
launchForwarders iomgr magic
                 ekgConfig tfConfig dpfConfig
                 ekgStore sink dpStore tracerSocketMode maxReconnectDelay =
  -- If 'tracerSocketMode' is not specified, it's impossible to establish
  -- network connection with acceptor application (for example, 'cardano-tracer').
  -- In this case, we should not lauch forwarders.
  case tracerSocketMode of
    Nothing -> return ()
    Just (socketPath, mode) ->
      void . async $
        runInLoop
          (launchForwardersViaLocalSocket
             iomgr
             magic
             ekgConfig
             tfConfig
             dpfConfig
             sink
             ekgStore
             dpStore
             socketPath
             mode)
          socketPath
          1
          maxReconnectDelay

launchForwardersViaLocalSocket
  :: IOManager
  -> NetworkMagic
  -> EKGF.ForwarderConfiguration
  -> TF.ForwarderConfiguration TraceObject
  -> DPF.ForwarderConfiguration
  -> ForwardSink TraceObject
  -> Maybe EKG.Store
  -> DataPointStore
  -> FilePath
  -> ForwarderMode
  -> IO ()
launchForwardersViaLocalSocket
  iomgr magic ekgConfig tfConfig dpfConfig sink ekgStore dpStore p mode =
  (case mode of
     Initiator -> doConnectToAcceptor magic (localSnocket iomgr) makeLocalBearer mempty
     Responder -> doListenToAcceptor magic (localSnocket iomgr) makeLocalBearer mempty)
  (localAddressFromPath p)
  noTimeLimitsHandshake ekgConfig tfConfig dpfConfig sink ekgStore dpStore

doConnectToAcceptor
  :: NetworkMagic
  -> Snocket IO fd addr
  -> MakeBearer IO fd
  -> (fd -> IO ())
  -> addr
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> EKGF.ForwarderConfiguration
  -> TF.ForwarderConfiguration TraceObject
  -> DPF.ForwarderConfiguration
  -> ForwardSink TraceObject
  -> Maybe EKG.Store
  -> DataPointStore
  -> IO ()
doConnectToAcceptor magic snocket makeBearer configureSocket address timeLimits
                    ekgConfig tfConfig dpfConfig sink ekgStore dpStore = do
  done <- connectToNode
    snocket
    makeBearer
    args
    configureSocket
    (simpleSingletonVersions
       ForwardingV_1
       (ForwardingVersionData magic)
       (const $ forwarderApp [ (forwardEKGMetricsRun,                      1)
                             , (forwardTraceObjectsInit tfConfig  sink,    2)
                             , (forwardDataPointsInit   dpfConfig dpStore, 3)
                             ]
       )
    )
    Nothing
    address
  case done of
    Left err -> throwIO err
    Right choice -> case choice of
      Left () -> return ()
      Right v -> absurd v
 where
  args = ConnectToArgs {
    ctaHandshakeCodec = codecHandshake forwardingVersionCodec,
    ctaHandshakeTimeLimits = timeLimits,
    ctaVersionDataCodec = cborTermVersionDataCodec forwardingCodecCBORTerm,
    ctaConnectTracers = nullNetworkConnectTracers,
    ctaHandshakeCallbacks = HandshakeCallbacks acceptableVersion queryVersion }
  forwarderApp
    :: [(RunMiniProtocol 'Mux.InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void, Word16)]
    -> OuroborosApplication 'Mux.InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
  forwarderApp protocols =
    OuroborosApplication
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolStart  = Mux.StartEagerly
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = prot
         }
      | (prot, num) <- protocols
      ]

  forwardEKGMetricsRun =
    case ekgStore of
      Just store -> forwardEKGMetrics ekgConfig store
      Nothing -> forwardEKGMetricsDummy

doListenToAcceptor
  :: NetworkMagic
  -> Snocket IO fd addr
  -> MakeBearer IO fd
  -> (fd -> addr -> IO ())
  -> addr
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> EKGF.ForwarderConfiguration
  -> TF.ForwarderConfiguration TraceObject
  -> DPF.ForwarderConfiguration
  -> ForwardSink TraceObject
  -> Maybe EKG.Store
  -> DataPointStore
  -> IO ()
doListenToAcceptor magic snocket makeBearer configureSocket address timeLimits
                   ekgConfig tfConfig dpfConfig sink ekgStore dpStore =
  void $ Server.with
    snocket
    makeBearer
    configureSocket
    address
    HandshakeArguments {
      haHandshakeTracer = nullTracer,
      haHandshakeCodec = codecHandshake forwardingVersionCodec,
      haVersionDataCodec = cborTermVersionDataCodec forwardingCodecCBORTerm,
      haAcceptVersion = acceptableVersion,
      haQueryVersion = queryVersion,
      haTimeLimits = timeLimits
    }
    (simpleSingletonVersions
       ForwardingV_1
       (ForwardingVersionData magic)
       (const $ SomeResponderApplication $
         forwarderApp [ (forwardEKGMetricsRespRun,                  1)
                      , (forwardTraceObjectsResp tfConfig  sink,    2)
                      , (forwardDataPointsResp   dpfConfig dpStore, 3)
                      ]
       )
    )
    $ \_ serverAsync ->
      wait serverAsync -- Block until async exception.
 where
  forwarderApp
    :: [(RunMiniProtocol 'Mux.ResponderMode initiatorCtx responderCtx LBS.ByteString IO Void (), Word16)]
    -> OuroborosApplication 'Mux.ResponderMode initiatorCtx responderCtx LBS.ByteString IO Void ()
  forwarderApp protocols =
    OuroborosApplication
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolStart  = Mux.StartEagerly
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = prot
         }
      | (prot, num) <- protocols
      ]

  forwardEKGMetricsRespRun =
    case ekgStore of
      Just store -> forwardEKGMetricsResp ekgConfig store
      Nothing -> forwardEKGMetricsRespDummy
