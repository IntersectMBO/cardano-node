{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Tracer.Test.Forwarder
  ( ForwardersMode (..)
  , TestDataPoint (..)
  , launchForwardersSimple
  , mkTestDataPoint
  ) where

import           Cardano.Logging (DetailLevel (..), SeverityS (..), TraceObject (..))
import           Cardano.Logging.Version (ForwardingVersion (..), ForwardingVersionData (..),
                   forwardingCodecCBORTerm, forwardingVersionCodec)
import           Cardano.Tracer.Configuration (Verbosity (..))
import           Cardano.Tracer.Test.TestSetup
import           Cardano.Tracer.Test.Utils
import           Cardano.Tracer.Utils
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (IOManager, withIOManager)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                   MiniProtocolNum (..), MuxMode (..), OuroborosApplication (..),
                   RunMiniProtocol (..), miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                   codecHandshake, noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, queryVersion,
                   simpleSingletonVersions)
import           Ouroboros.Network.Snocket (MakeBearer, Snocket, localAddressFromPath, localSnocket,
                   makeLocalBearer)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..), HandshakeCallbacks (..),
                   SomeResponderApplication (..), cleanNetworkMutableState, connectToNode,
                   newNetworkMutableState, nullNetworkConnectTracers, nullNetworkServerTracers,
                   withServerNode)

import           Codec.CBOR.Term (Term)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.DeepSeq (NFData)
import           Control.Monad (forever)
import           "contra-tracer" Control.Tracer (contramap, nullTracer, stdoutTracer)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import           Data.Time.Clock (getCurrentTime)
import           Data.Void (Void)
import           Data.Word (Word16)
import           GHC.Generics
import           System.Directory
import qualified System.Metrics as EKG
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TOF
import           Trace.Forward.Run.DataPoint.Forwarder
import           Trace.Forward.Run.TraceObject.Forwarder
import           Trace.Forward.Utils.DataPoint
import           Trace.Forward.Utils.TraceObject

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
  -> FilePath
  -> Word
  -> Word
  -> IO ()
launchForwardersSimple ts mode p connSize disconnSize = withIOManager $ \iomgr ->
  runInLoop (launchForwardersSimple' ts iomgr mode p connSize disconnSize) (Just Minimum) p 1

launchForwardersSimple'
  :: TestSetup Identity
  -> IOManager
  -> ForwardersMode
  -> FilePath
  -> Word
  -> Word
  -> IO ()
launchForwardersSimple' ts iomgr mode p connSize disconnSize = do
  case mode of
    Initiator ->
      doConnectToAcceptor
        ts
        (localSnocket iomgr)
        makeLocalBearer
        (localAddressFromPath p)
        noTimeLimitsHandshake
        (ekgConfig, tfConfig, dpfConfig)
    Responder ->
      doListenToAcceptor
        ts
        (localSnocket iomgr)
        makeLocalBearer
        (localAddressFromPath p)
        noTimeLimitsHandshake
        (ekgConfig, tfConfig, dpfConfig)
 where
  ekgConfig :: EKGF.ForwarderConfiguration
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer = nullTracer -- contramap show stdoutTracer -- nullTracer
      , EKGF.acceptorEndpoint = EKGF.LocalPipe p
      , EKGF.reConnectFrequency = 1.0
      , EKGF.actionOnRequest = const $ return ()
      }

  tfConfig :: TOF.ForwarderConfiguration TraceObject
  tfConfig =
    TOF.ForwarderConfiguration
      { TOF.forwarderTracer = nullTracer -- contramap show stdoutTracer -- nullTracer
      , TOF.acceptorEndpoint = p
      , TOF.disconnectedQueueSize = disconnSize
      , TOF.connectedQueueSize = connSize
      }

  dpfConfig :: DPF.ForwarderConfiguration
  dpfConfig =
    DPF.ForwarderConfiguration
      { DPF.forwarderTracer = nullTracer -- contramap show stdoutTracer -- nullTracer
      , DPF.acceptorEndpoint = p
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
  withAsync (traceObjectsWriter sink) $ \_ -> do
    connectToNode
      snocket
      muxBearer
      mempty
      (codecHandshake forwardingVersionCodec)
      timeLimits
      (cborTermVersionDataCodec forwardingCodecCBORTerm)
      nullNetworkConnectTracers
      (HandshakeCallbacks acceptableVersion queryVersion)
      (simpleSingletonVersions
         ForwardingV_1
         (ForwardingVersionData $ unI tsNetworkMagic)
         (forwarderApp [ (forwardEKGMetrics ekgConfig store,       1)
                       , (forwardTraceObjectsInit tfConfig sink,   2)
                       , (forwardDataPointsInit dpfConfig dpStore, 3)
                       ]
         )
      )
      Nothing
      address
 where
  forwarderApp
    :: [(RunMiniProtocol 'InitiatorMode initCtx respCtx LBS.ByteString IO () Void, Word16)]
    -> OuroborosApplication 'InitiatorMode initCtx respCtx LBS.ByteString IO () Void
  forwarderApp protocols =
    OuroborosApplication
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = prot
         }
      | (prot, num) <- protocols
      ]

doListenToAcceptor
  :: Ord addr
  => TestSetup Identity
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
  withAsync (traceObjectsWriter sink) $ \_ -> do
    networkState <- newNetworkMutableState
    race_ (cleanNetworkMutableState networkState)
          $ withServerNode
              snocket
              muxBearer
              mempty
              nullNetworkServerTracers
              networkState
              (AcceptedConnectionsLimit maxBound maxBound 0)
              address
              (codecHandshake forwardingVersionCodec)
              timeLimits
              (cborTermVersionDataCodec forwardingCodecCBORTerm)
              (HandshakeCallbacks acceptableVersion queryVersion)
              (simpleSingletonVersions
                 ForwardingV_1
                 (ForwardingVersionData $ unI tsNetworkMagic) -- Taken from mainnet shelley genesis file.
                 (SomeResponderApplication $
                    forwarderApp [ (forwardEKGMetricsResp ekgConfig store,   1)
                                 , (forwardTraceObjectsResp tfConfig sink,   2)
                                 , (forwardDataPointsResp dpfConfig dpStore, 3)
                                 ]
                 )
              )
              nullErrorPolicies
              $ \_ serverAsync -> wait serverAsync -- Block until async exception.
 where
  forwarderApp
    :: [(RunMiniProtocol 'ResponderMode initCtx respCtx LBS.ByteString IO Void (), Word16)]
    -> OuroborosApplication 'ResponderMode initCtx respCtx LBS.ByteString IO Void ()
  forwarderApp protocols =
    OuroborosApplication
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = prot
         }
      | (prot, num) <- protocols
      ]

traceObjectsWriter :: ForwardSink TraceObject -> IO ()
traceObjectsWriter sink = forever $ do
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
