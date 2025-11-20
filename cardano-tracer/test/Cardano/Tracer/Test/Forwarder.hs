{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Tracer.Test.Forwarder
  ( ForwardersMode (..)
  , TestDataPoint (..)
  , launchForwardersSimple
  , mkTestDataPoint
  ) where

import           Cardano.Logging (DetailLevel (..), SeverityS (..), TraceObject (..))
import           Cardano.Logging.Types (HowToConnect)
import qualified Cardano.Logging.Types as Net
import           Cardano.Tracer.Configuration (Verbosity (..))
import           Cardano.Tracer.Test.TestSetup
import           Cardano.Tracer.Test.Utils
import           Cardano.Tracer.Utils
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (IOManager, withIOManager)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                   MiniProtocolNum (..), OuroborosApplication (..), RunMiniProtocol (..),
                   miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake (Handshake, HandshakeArguments (..))
import qualified Ouroboros.Network.Protocol.Handshake as Handshake
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                   codecHandshake, noTimeLimitsHandshake)
import qualified Ouroboros.Network.Server.Simple as Server
import           Ouroboros.Network.Snocket (MakeBearer, Snocket, localAddressFromPath, localSnocket,
                   makeLocalBearer, makeSocketBearer, socketSnocket)
import           Ouroboros.Network.Socket (ConnectToArgs (..), HandshakeCallbacks (..),
                   SomeResponderApplication (..), connectToNode, nullNetworkConnectTracers)

import           Codec.CBOR.Term (Term)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async hiding (async)
import           Control.DeepSeq (NFData)
import           Control.Exception (IOException, SomeException, catch, throwIO, try)
import           Control.Monad (forever)
import           "contra-tracer" Control.Tracer (contramap, nullTracer, stdoutTracer)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor (void)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as Text
import           Data.Time.Clock (getCurrentTime)
import           Data.Void (Void, absurd)
import           Data.Word (Word16)
import           GHC.Generics
import qualified Network.Mux as Mux
import qualified Network.Socket as Socket
import           System.Directory
import qualified System.Metrics as EKG
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TOF
import           Trace.Forward.Run.DataPoint.Forwarder
import           Trace.Forward.Run.TraceObject.Forwarder
import           Trace.Forward.Utils.DataPoint
import           Trace.Forward.Utils.ForwardSink (ForwardSink)
import           Trace.Forward.Utils.TraceObject
import           Trace.Forward.Utils.Version (ForwardingVersion (..), ForwardingVersionData (..),
                   forwardingCodecCBORTerm, forwardingVersionCodec)


data ForwardersMode = Initiator | Responder

data TestDataPoint = TestDataPoint
  { tdpName    :: !String
  , tdpCommit  :: !String
  , tdpVersion :: !Int
  } deriving (Generic, NFData, Eq, FromJSON, ToJSON)

mkTestDataPoint :: TestDataPoint
mkTestDataPoint = TestDataPoint
  { tdpName    = "tdpName for Tests"
  , tdpCommit  = "ab23c45"
  , tdpVersion = 32
  }

launchForwardersSimple
  :: TestSetup Identity
  -> ForwardersMode
  -> HowToConnect
  -> Word
  -> IO ()
launchForwardersSimple ts mode howToConnect queueSize = withIOManager \iomgr ->
  runInLoop (launchForwardersSimple' ts iomgr mode howToConnect queueSize) (Just Minimum) howToConnect 1

launchForwardersSimple'
  :: TestSetup Identity
  -> IOManager
  -> ForwardersMode
  -> HowToConnect
  -> Word
  -> IO ()
launchForwardersSimple' ts iomgr mode howToConnect queueSize =
  case (howToConnect, mode) of
    (Net.RemoteSocket (Text.unpack -> host) (show -> port), Initiator) -> do
      result <- try @IOException do
        Socket.getAddrInfo Nothing (Just host) (Just port)
      case result of
        Left exception -> do
          logTrace $ "launchForwardersSimple': Initiator: No address resolved for host: " ++ show exception
          throwIO exception
        Right (listenAddress :| _) -> do
          catch @SomeException
            do doConnectToAcceptor
                 ts
                 (socketSnocket iomgr)
                 makeSocketBearer
                 (Socket.addrAddress listenAddress)
                 Handshake.timeLimitsHandshake
                 (ekgConfig, tfConfig, dpfConfig)
            do \(exception :: SomeException) -> do
                  logTrace $ "launchForwardersSimple': doConnectToAcceptor failure: " ++ show exception
                  throwIO exception
    (Net.RemoteSocket (Text.unpack -> host) (show -> port), Responder) -> do
      result <- try @IOException do
        Socket.getAddrInfo Nothing (Just host) (Just port)
      case result of
        Left exception -> do
          logTrace $ "launchForwardersSimple': Responder: No address resolved for host: " ++ show exception
          throwIO exception
        Right (listenAddress :| _) -> do
          catch @SomeException
            do doListenToAcceptor
                 ts
                 (socketSnocket iomgr)
                 makeSocketBearer
                 (Socket.addrAddress listenAddress)
                 Handshake.timeLimitsHandshake
                 (ekgConfig, tfConfig, dpfConfig)
            do \(exception :: SomeException) -> do
                  logTrace $ "launchForwardersSimple': doListenToAcceptor failure: " ++ show exception
                  throwIO exception
    (Net.LocalPipe localSocket, Initiator) ->
      doConnectToAcceptor
        ts
        (localSnocket iomgr)
        makeLocalBearer
        (localAddressFromPath localSocket)
        noTimeLimitsHandshake
        (ekgConfig, tfConfig, dpfConfig)
    (Net.LocalPipe localSocket, Responder) ->
      doListenToAcceptor
        ts
        (localSnocket iomgr)
        makeLocalBearer
        (localAddressFromPath localSocket)
        noTimeLimitsHandshake
        (ekgConfig, tfConfig, dpfConfig)
 where
  ekgConfig :: EKGF.ForwarderConfiguration
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer = nullTracer -- contramap show stdoutTracer
      , EKGF.acceptorEndpoint = case howToConnect of
          Net.LocalPipe localSocket  -> EKGF.LocalPipe localSocket
          Net.RemoteSocket host port -> EKGF.RemoteSocket host port
      , EKGF.reConnectFrequency = 1.0
      , EKGF.actionOnRequest = const $ return ()
      , EKGF.useDummyForwarder = False
      }

  tfConfig :: TOF.ForwarderConfiguration TraceObject
  tfConfig =
    TOF.ForwarderConfiguration
      { TOF.forwarderTracer = nullTracer -- contramap show stdoutTracer
      , TOF.queueSize = queueSize
      }

  dpfConfig :: DPF.ForwarderConfiguration
  dpfConfig =
    DPF.ForwarderConfiguration
      { DPF.forwarderTracer = nullTracer -- contramap show stdoutTracer
      }

doConnectToAcceptor
  :: TestSetup Identity
  -> Snocket IO fd addr
  -> MakeBearer IO fd
  -> addr
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> ( EKGF.ForwarderConfiguration
     , TOF.ForwarderConfiguration TraceObject
     , DPF.ForwarderConfiguration
     )
  -> IO ()
doConnectToAcceptor TestSetup{..} snocket muxBearer address timeLimits (ekgConfig, tfConfig, dpfConfig) = do
  store <- EKG.newStore
  EKG.registerGcMetrics store
  sink <- initForwardSink tfConfig (\ _ -> pure ())
  dpStore <- initDataPointStore
  writeToStore dpStore "test.data.point" $ DataPoint mkTestDataPoint
  withAsync (traceObjectsWriter sink) \async -> do
    link async
    done <- connectToNode
      snocket
      muxBearer
      args
      mempty
      (Handshake.simpleSingletonVersions
         ForwardingV_1
         (ForwardingVersionData $ unI tsNetworkMagic)
         (const $ forwarderApp [ (forwardEKGMetrics ekgConfig store,       1)
                               , (forwardTraceObjectsInit tfConfig sink,   2)
                               , (forwardDataPointsInit dpfConfig dpStore, 3)
                               ]
         )
      )
      Nothing
      address
    case done of
      Left err -> throwIO err
      Right choice -> case choice of
        Left () -> return ()
        Right void_ -> absurd void_
 where
  args = ConnectToArgs {
    ctaHandshakeCodec = codecHandshake forwardingVersionCodec,
    ctaHandshakeTimeLimits = timeLimits,
    ctaVersionDataCodec = cborTermVersionDataCodec forwardingCodecCBORTerm,
    ctaConnectTracers = nullNetworkConnectTracers,
    ctaHandshakeCallbacks = HandshakeCallbacks Handshake.acceptableVersion Handshake.queryVersion }

  forwarderApp
    :: [(RunMiniProtocol 'Mux.InitiatorMode initCtx respCtx LBS.ByteString IO () Void, Word16)]
    -> OuroborosApplication 'Mux.InitiatorMode initCtx respCtx LBS.ByteString IO () Void
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

doListenToAcceptor
  :: TestSetup Identity
  -> Snocket IO fd addr
  -> MakeBearer IO fd
  -> addr
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> ( EKGF.ForwarderConfiguration
     , TOF.ForwarderConfiguration TraceObject
     , DPF.ForwarderConfiguration
     )
  -> IO ()
doListenToAcceptor TestSetup{..}
  snocket muxBearer address timeLimits (ekgConfig, tfConfig, dpfConfig) = do

  store <- EKG.newStore
  EKG.registerGcMetrics store
  sink <- initForwardSink tfConfig (\ _ -> pure ())
  dpStore <- initDataPointStore
  writeToStore dpStore "test.data.point" $ DataPoint mkTestDataPoint
  withAsync (traceObjectsWriter sink) $ \_ ->
    void $ Server.with
      snocket
      muxBearer
      mempty
      address
      HandshakeArguments {
        haHandshakeTracer = nullTracer,
        haBearerTracer = nullTracer,
        haHandshakeCodec = codecHandshake forwardingVersionCodec,
        haVersionDataCodec = cborTermVersionDataCodec forwardingCodecCBORTerm,
        haAcceptVersion = Handshake.acceptableVersion,
        haQueryVersion = Handshake.queryVersion,
        haTimeLimits = timeLimits
      }
      (Handshake.simpleSingletonVersions
         ForwardingV_1
         (ForwardingVersionData $ unI tsNetworkMagic)
         (const $ SomeResponderApplication $
            forwarderApp [ (forwardEKGMetricsResp ekgConfig store,   1)
                         , (forwardTraceObjectsResp tfConfig sink,   2)
                         , (forwardDataPointsResp dpfConfig dpStore, 3)
                         ]
         )
      )
      $ \_ serverAsync -> wait serverAsync -- Block until async exception.
 where
  forwarderApp
    :: [(RunMiniProtocol 'Mux.ResponderMode initCtx respCtx LBS.ByteString IO Void (), Word16)]
    -> OuroborosApplication 'Mux.ResponderMode initCtx respCtx LBS.ByteString IO Void ()
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

traceObjectsWriter :: ForwardSink TraceObject -> IO ()
traceObjectsWriter sink = forever do
  writeToSink sink . mkTraceObject =<< getCurrentTime
  threadDelay 40000
 where
  mkTraceObject now = TraceObject
    { toHuman     = Just "Human Message for testing if our mechanism works as we expect"
    , toMachine   = "{\"msg\": \"Very big message forMachine because we have to check if it works\"}"
    , toNamespace = ["demoNamespace"]
    , toSeverity  = Info
    , toDetails   = DNormal
    , toTimestamp = now
    , toHostname  = "nixos"
    , toThreadId  = "1"
    }
