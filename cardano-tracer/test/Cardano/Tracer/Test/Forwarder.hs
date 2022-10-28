{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Tracer.Test.Forwarder
  ( ForwardersMode (..)
  , TestDataPoint (..)
  , launchForwardersSimple
  , mkTestDataPoint
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Monad (forever)
import           "contra-tracer" Control.Tracer (nullTracer, contramap, stdoutTracer)
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import           Data.Time.Clock (getCurrentTime)
import           Data.Void (Void)
import           Data.Word (Word16)
import           GHC.Generics
import qualified System.Metrics as EKG

import           Cardano.Logging (DetailLevel (..), SeverityS (..), TraceObject (..))
import           Cardano.Logging.Version (ForwardingVersion (..), ForwardingVersionData (..),
                   forwardingCodecCBORTerm, forwardingVersionCodec)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (IOManager, withIOManager)
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                   MiniProtocolNum (..), MuxMode (..), OuroborosApplication (..),
                   RunMiniProtocol (..), miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (codecHandshake,
                   cborTermVersionDataCodec, noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion,
                   simpleSingletonVersions)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..),
                   SomeResponderApplication (..), cleanNetworkMutableState,
                   connectToNode, newNetworkMutableState, nullNetworkConnectTracers,
                   nullNetworkServerTracers, withServerNode)
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder

import qualified Trace.Forward.Configuration.DataPoint as DPF
import qualified Trace.Forward.Configuration.TraceObject as TOF
import           Trace.Forward.Run.DataPoint.Forwarder
import           Trace.Forward.Run.TraceObject.Forwarder
import           Trace.Forward.Utils.DataPoint
import           Trace.Forward.Utils.TraceObject

import           Cardano.Tracer.Configuration (Verbosity (..))
import           Cardano.Tracer.Utils

data ForwardersMode = Initiator | Responder

data TestDataPoint = TestDataPoint
  { tdpName    :: !String
  , tdpCommit  :: !String
  , tdpVersion :: !Int
  } deriving (Generic, Eq, FromJSON, ToJSON)

mkTestDataPoint :: TestDataPoint
mkTestDataPoint = TestDataPoint
  { tdpName    = "tdpName for Tests"
  , tdpCommit  = "ab23c45"
  , tdpVersion = 32
  }

launchForwardersSimple
  :: ForwardersMode
  -> FilePath
  -> Word
  -> Word
  -> IO ()
launchForwardersSimple mode p connSize disconnSize = withIOManager $ \iomgr ->
  runInLoop (launchForwardersSimple' iomgr mode p connSize disconnSize) (Just Minimum) p 1

launchForwardersSimple'
  :: IOManager
  -> ForwardersMode
  -> FilePath
  -> Word
  -> Word
  -> IO ()
launchForwardersSimple' iomgr mode p connSize disconnSize =
  case mode of
    Initiator ->
      doConnectToAcceptor
        (localSnocket iomgr)
        (localAddressFromPath p)
        noTimeLimitsHandshake
        (ekgConfig, tfConfig, dpfConfig)
    Responder ->
      doListenToAcceptor
        (localSnocket iomgr)
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
      { DPF.forwarderTracer = contramap show stdoutTracer -- nullTracer
      , DPF.acceptorEndpoint = p
      }

doConnectToAcceptor
  :: Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> ( EKGF.ForwarderConfiguration
     , TOF.ForwarderConfiguration TraceObject
     , DPF.ForwarderConfiguration
     )
  -> IO ()
doConnectToAcceptor snocket address timeLimits (ekgConfig, tfConfig, dpfConfig) = do
  store <- EKG.newStore
  EKG.registerGcMetrics store
  sink <- initForwardSink tfConfig
  dpStore <- initDataPointStore
  writeToStore dpStore "test.data.point" $ DataPoint mkTestDataPoint
  withAsync (traceObjectsWriter sink) $ \_ -> do
    connectToNode
      snocket
      mempty
      (codecHandshake forwardingVersionCodec)
      timeLimits
      (cborTermVersionDataCodec forwardingCodecCBORTerm)
      nullNetworkConnectTracers
      acceptableVersion
      (simpleSingletonVersions
         ForwardingV_1
         (ForwardingVersionData $ NetworkMagic 764824073) -- Taken from mainnet shelley genesis file.
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

doListenToAcceptor
  :: Ord addr
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake ForwardingVersion Term)
  -> ( EKGF.ForwarderConfiguration
     , TOF.ForwarderConfiguration TraceObject
     , DPF.ForwarderConfiguration
     )
  -> IO ()
doListenToAcceptor snocket address timeLimits (ekgConfig, tfConfig, dpfConfig) = do
  store <- EKG.newStore
  EKG.registerGcMetrics store
  sink <- initForwardSink tfConfig
  dpStore <- initDataPointStore
  writeToStore dpStore "test.data.point" $ DataPoint mkTestDataPoint
  withAsync (traceObjectsWriter sink) $ \_ -> do
    networkState <- newNetworkMutableState
    race_ (cleanNetworkMutableState networkState)
          $ withServerNode
              snocket
              mempty
              nullNetworkServerTracers
              networkState
              (AcceptedConnectionsLimit maxBound maxBound 0)
              address
              (codecHandshake forwardingVersionCodec)
              timeLimits
              (cborTermVersionDataCodec forwardingCodecCBORTerm)
              acceptableVersion
              (simpleSingletonVersions
                 ForwardingV_1
                 (ForwardingVersionData $ NetworkMagic 764824073) -- Taken from mainnet shelley genesis file.
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
    :: [(RunMiniProtocol 'ResponderMode LBS.ByteString IO Void (), Word16)]
    -> OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
  forwarderApp protocols =
    OuroborosApplication $ \_connectionId _shouldStopSTM ->
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
    , toMachine   = Just "{\"msg\": \"Very big message forMachine because we have to check if it works\"}"
    , toNamespace = ["demoNamespace"]
    , toSeverity  = Info
    , toDetails   = DNormal
    , toTimestamp = now
    , toHostname  = "nixos"
    , toThreadId  = "1"
    }
