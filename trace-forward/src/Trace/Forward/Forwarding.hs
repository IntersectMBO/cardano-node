{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-partial-fields #-}

module Trace.Forward.Forwarding
  ( InitForwardingConfig(..)
  , initForwarding
  , initForwardingDelayed
  ) where

import           Cardano.Logging.Types
import           Cardano.Logging.Utils (runInLoop)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (IOManager)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                   MiniProtocolNum (..), OuroborosApplication (..), RunMiniProtocol (..),
                   miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake (HandshakeArguments (..))
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                   codecHandshake, noTimeLimitsHandshake, timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, queryVersion,
                   simpleSingletonVersions)
import qualified Ouroboros.Network.Server.Simple as Server
import           Ouroboros.Network.Snocket (LocalAddress, LocalSocket, MakeBearer, Snocket,
                   localAddressFromPath, localSnocket, makeLocalBearer, makeSocketBearer,
                   socketSnocket)
import           Ouroboros.Network.Socket (ConnectToArgs (..), HandshakeCallbacks (..),
                   SomeResponderApplication (..), connectToNode, nullNetworkConnectTracers)

import           Codec.CBOR.Term (Term)
import           Control.Concurrent.Async (async, wait)
import           Control.Exception (SomeException, throwIO)
import           Control.Monad.IO.Class
import           "contra-tracer" Control.Tracer (Tracer, contramap, nullTracer, stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import           Data.Void (Void, absurd)
import           Data.Word (Word16)
import qualified Network.Mux as Mux
import qualified Network.Socket as Socket
import           System.IO (hPutStrLn, stderr)
import qualified System.Metrics as EKG
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Run.DataPoint.Forwarder
import           Trace.Forward.Run.TraceObject.Forwarder
import           Trace.Forward.Utils.DataPoint
import           Trace.Forward.Utils.ForwardSink (ForwardSink)
import           Trace.Forward.Utils.TraceObject
import           Trace.Forward.Utils.Version


-- | Config record to initialise trace forwarding
data InitForwardingConfig
  = -- | Only construct relevant values, but do not actually run forwarding
    InitForwardingNone
  | -- | Run forwarding with the provided settings
    InitForwardingWith
    { initNetworkMagic          :: !NetworkMagic
    -- ^ Forwarding is always tied to a singular networkId
    , initEKGStore              :: !(Maybe EKG.Store)
    -- ^ A metrics store to be forwarded (optional)
    , initHowToConnect          :: !HowToConnect
    -- ^ A LocalPipe or RemoteSocket
    , initForwarderMode         :: !ForwarderMode
    -- ^ Run as Initiator or Responder
    , initOnForwardInterruption :: !(Maybe (SomeException -> IO ()))
    -- ^ Optional handler when forwarding connection is interrupted (may be temporary or permanent)
    --   default: no action
    , initOnQueueOverflow       :: !(Maybe ([TraceObject] -> IO ()))
    -- ^ Optional handler when forwarding queue overflows (argument are objects dropped from queue)
    --   default: print one-liner to stderr, indicating object count and timestamps of first and last object
    }


initForwarding :: forall m. (MonadIO m)
  => IOManager
  -> TraceOptionForwarder
  -> InitForwardingConfig
  -> m (ForwardSink TraceObject, DataPointStore)
initForwarding iomgr config forwarding = do
  (a, b, kickoffForwarder) <- initForwardingDelayed iomgr config forwarding
  liftIO kickoffForwarder
  pure (a, b)

-- We allow for delayed initialization of the forwarding connection by
-- returning an IO action to do so.
initForwardingDelayed :: forall m. ()
  => MonadIO m
  => IOManager
  -> TraceOptionForwarder
  -> InitForwardingConfig
  -> m (ForwardSink TraceObject, DataPointStore, IO ())
initForwardingDelayed iomgr config forwarding = liftIO $ do
  let onOverflow :: [TraceObject] -> IO ()
      onOverflow = case forwarding of
        InitForwardingNone                                      -> const $ pure ()
        InitForwardingWith{initOnQueueOverflow = Just handler}  -> handler
        InitForwardingWith{initOnQueueOverflow = Nothing}       -> handleOverflow
  forwardSink <- initForwardSink tfConfig onOverflow
  dpStore <- initDataPointStore
  let
    kickoffForwarder = launchForwarders
      iomgr
      forwarding
      ekgConfig
      tfConfig
      dpfConfig
      forwardSink
      dpStore
      maxReconnectDelay
  pure (forwardSink, dpStore, kickoffForwarder)
 where
  endpoint :: EKGF.HowToConnect
  endpoint =
    case forwarding of
      InitForwardingNone                                            -> EKGF.LocalPipe ""
      InitForwardingWith{initHowToConnect = LocalPipe str}          -> EKGF.LocalPipe str
      InitForwardingWith{initHowToConnect = RemoteSocket host port} -> EKGF.RemoteSocket host port
  queueSize         = tofQueueSize         config
  verbosity         = tofVerbosity         config
  maxReconnectDelay = tofMaxReconnectDelay config

  ekgConfig :: EKGF.ForwarderConfiguration
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer    = mkTracer verbosity
      , EKGF.acceptorEndpoint   = endpoint
      , EKGF.reConnectFrequency = 1.0
      , EKGF.actionOnRequest    = const $ pure ()
      , EKGF.useDummyForwarder  = False
      }

  tfConfig :: TF.ForwarderConfiguration TraceObject
  tfConfig =
    TF.ForwarderConfiguration
      { TF.forwarderTracer = mkTracer verbosity
      , TF.queueSize       = queueSize
      }

  dpfConfig :: DPF.ForwarderConfiguration
  dpfConfig =
    DPF.ForwarderConfiguration
      { DPF.forwarderTracer  = mkTracer verbosity
      }

  mkTracer :: Show a => Verbosity -> Tracer IO a
  mkTracer Maximum = contramap show stdoutTracer
  mkTracer Minimum = nullTracer

-- | this function is called when the queue is full.
--  It is called with the list of messages that were dropped.
-- It writes an error message on stderr
handleOverflow :: [TraceObject] -> IO ()
handleOverflow [] = pure ()
handleOverflow (msg : msgs) =
    let lengthM = 1 + length msgs
        beginning = toTimestamp msg
        end = toTimestamp (last (msg : msgs))
        str = "TraceObject queue overflowed. Dropped " <> show lengthM <>
                " messages from " <> show beginning <> " to " <> show end
    in hPutStrLn stderr str

launchForwarders
  :: IOManager
  -> InitForwardingConfig
  -> EKGF.ForwarderConfiguration
  -> TF.ForwarderConfiguration TraceObject
  -> DPF.ForwarderConfiguration
  -> ForwardSink TraceObject
  -> DataPointStore
  -> Word
  -> IO ()
launchForwarders iomgr forwarding
                 ekgConfig tfConfig dpfConfig
                 sink dpStore maxReconnectDelay =
  -- If InitForwardingNone is specified, it's impossible to establish
  -- a connection with an acceptor application (for example, 'cardano-tracer').
  -- In this case, we should not launch forwarders.
  case forwarding of
    InitForwardingNone     -> return ()
    InitForwardingWith{..} ->
      void . async $
        runInLoop
          (launchForwardersViaLocalSocket
             iomgr
             initNetworkMagic
             initHowToConnect
             initForwarderMode
             ekgConfig
             tfConfig
             dpfConfig
             sink
             initEKGStore
             dpStore)
          (fromMaybe (const $ pure ()) initOnForwardInterruption)
          1
          maxReconnectDelay

launchForwardersViaLocalSocket
  :: IOManager
  -> NetworkMagic
  -> HowToConnect
  -> ForwarderMode
  -> EKGF.ForwarderConfiguration
  -> TF.ForwarderConfiguration TraceObject
  -> DPF.ForwarderConfiguration
  -> ForwardSink TraceObject
  -> Maybe EKG.Store
  -> DataPointStore
  -> IO ()
launchForwardersViaLocalSocket
  iomgr magic howToConnect mode ekgConfig tfConfig dpfConfig sink ekgStore dpStore =
  case (mode, howToConnect) of
    (Initiator, LocalPipe localPipe) -> do
      doConnectToAcceptor @LocalSocket @LocalAddress
        magic (localSnocket iomgr) makeLocalBearer mempty (localAddressFromPath localPipe)
        noTimeLimitsHandshake ekgConfig tfConfig dpfConfig sink ekgStore dpStore
    (Initiator, RemoteSocket (Text.unpack -> host) (show -> port)) -> do
      listenAddress:|_ <- Socket.getAddrInfo Nothing (Just host) (Just port)
      doConnectToAcceptor @Socket.Socket @Socket.SockAddr
        magic (socketSnocket iomgr) makeSocketBearer mempty (Socket.addrAddress listenAddress)
        timeLimitsHandshake ekgConfig tfConfig dpfConfig sink ekgStore dpStore
    (Responder, LocalPipe localPipe) -> do
      doListenToAcceptor @LocalSocket @LocalAddress
        magic (localSnocket iomgr) makeLocalBearer mempty (localAddressFromPath localPipe)
        noTimeLimitsHandshake ekgConfig tfConfig dpfConfig sink ekgStore dpStore
    (Responder, RemoteSocket (Text.unpack -> host) (show -> port)) -> do
      listenAddress:_ <- Socket.getAddrInfo Nothing (Just host) (Just port)
      doListenToAcceptor @Socket.Socket @Socket.SockAddr
        magic (socketSnocket iomgr) makeSocketBearer mempty (Socket.addrAddress listenAddress)
        timeLimitsHandshake ekgConfig tfConfig dpfConfig sink ekgStore dpStore

doConnectToAcceptor
  :: forall fd addr. ()
  => NetworkMagic
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
    nullTracer
    Mux.nullTracers
    makeBearer
    configureSocket
    address
    HandshakeArguments {
      haBearerTracer = nullTracer,
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
