{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Test.Forwarder
  ( ForwardersMode (..)
  , launchForwardersSimple
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Monad (forever)
import "contra-tracer" Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Void (Void)
import           Data.Word (Word16)

import           Ouroboros.Network.IOManager (IOManager, withIOManager)

import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.Mux (MiniProtocol (..),
                     MiniProtocolLimits (..), MiniProtocolNum (..),
                     MuxMode (..), OuroborosApplication (..),
                     RunMiniProtocol (..), miniProtocolLimits, miniProtocolNum,
                     miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec
                     (cborTermVersionDataCodec, noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned
                     (UnversionedProtocol (..), UnversionedProtocolData (..),
                     unversionedHandshakeCodec, unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version
                     (acceptableVersion, simpleSingletonVersions)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath,
                     localSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..),
                     SomeResponderApplication (..), cleanNetworkMutableState,
                     connectToNode, newNetworkMutableState, nullNetworkConnectTracers,
                     nullNetworkServerTracers, withServerNode)

import qualified System.Metrics as EKG

import           Cardano.Logging (DetailLevel (..), SeverityS (..), TraceObject (..))

import qualified Trace.Forward.Configuration as TF
import           Trace.Forward.Network.Forwarder
import           Trace.Forward.Protocol.Type (NodeInfo (..))
import           Trace.Forward.Utils

import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder

data ForwardersMode = Initiator | Responder

launchForwardersSimple
  :: ForwardersMode
  -> FilePath
  -> Word
  -> Word
  -> IO ()
launchForwardersSimple mode p connSize disconnSize = withIOManager $ \iomgr ->
  runActionInLoop
    (launchForwardersSimple' iomgr mode p connSize disconnSize)
    (TF.LocalPipe p)
    1

launchForwardersSimple'
  :: IOManager
  -> ForwardersMode
  -> FilePath
  -> Word
  -> Word
  -> IO ()
launchForwardersSimple' iomgr mode p connSize disconnSize = do
  now <- getCurrentTime
  case mode of
    Initiator ->
      doConnectToAcceptor
        (localSnocket iomgr)
        (localAddressFromPath p)
        noTimeLimitsHandshake
        (ekgConfig, tfConfig now)
    Responder ->
      doListenToAcceptor
        (localSnocket iomgr)
        (localAddressFromPath p)
        noTimeLimitsHandshake
        (ekgConfig, tfConfig now)
 where
  ekgConfig :: EKGF.ForwarderConfiguration
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer = nullTracer
      , EKGF.acceptorEndpoint = EKGF.LocalPipe p
      , EKGF.reConnectFrequency = 1.0
      , EKGF.actionOnRequest = const $ return ()
      }

  tfConfig :: UTCTime -> TF.ForwarderConfiguration TraceObject
  tfConfig now =
    TF.ForwarderConfiguration
      { TF.forwarderTracer = nullTracer
      , TF.acceptorEndpoint = TF.LocalPipe p
      , TF.getNodeInfo =
          return NodeInfo
            { niName            = "core-1"
            , niProtocol        = "Shelley"
            , niVersion         = "1.28.0"
            , niCommit          = "abcdefg"
            , niStartTime       = now
            , niSystemStartTime = now
            }
      , TF.disconnectedQueueSize = disconnSize
      , TF.connectedQueueSize    = connSize
      }

doConnectToAcceptor
  :: Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> IO ()
doConnectToAcceptor snocket address timeLimits (ekgConfig, tfConfig) = do
  store <- EKG.newStore
  EKG.registerGcMetrics store
  sink <- initForwardSink tfConfig
  withAsync (traceObjectsWriter sink) $ \_ -> do
    connectToNode
      snocket
      unversionedHandshakeCodec
      timeLimits
      (cborTermVersionDataCodec unversionedProtocolDataCodec)
      nullNetworkConnectTracers
      acceptableVersion
      (simpleSingletonVersions
         UnversionedProtocol
         UnversionedProtocolData
           (forwarderApp [ (forwardEKGMetrics ekgConfig store, 1)
                         , (forwardTraceObjects tfConfig sink, 2)
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
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> IO ()
doListenToAcceptor snocket address timeLimits (ekgConfig, tfConfig) = do
  store <- EKG.newStore
  EKG.registerGcMetrics store
  sink <- initForwardSink tfConfig
  withAsync (traceObjectsWriter sink) $ \_ -> do
    networkState <- newNetworkMutableState
    race_ (cleanNetworkMutableState networkState)
          $ withServerNode
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
                  forwarderApp [ (forwardEKGMetricsResp ekgConfig store, 1)
                               , (forwardTraceObjectsResp tfConfig sink, 2)
                               ]
                )
              )
              nullErrorPolicies
              $ \_ serverAsync ->
                wait serverAsync -- Block until async exception.
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
  threadDelay 50000
 where
  mkTraceObject now = TraceObject
    { toHuman     = Just "Human Message"
    , toMachine   = Just "{\"msg\": \"forMachine\"}"
    , toNamespace = ["demoNamespace"]
    , toSeverity  = Info
    , toDetails   = DNormal
    , toTimestamp = now
    , toHostname  = "nixos"
    , toThreadId  = "1"
    }
