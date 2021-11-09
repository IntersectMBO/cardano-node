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
import           Data.Time.Clock (getCurrentTime)
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

import qualified Trace.Forward.Configuration.TraceObject as TF
import           Trace.Forward.Run.TraceObject.Forwarder
import           Trace.Forward.Utils.TraceObject

import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder

import           Cardano.Tracer.Utils

data ForwardersMode = Initiator | Responder

launchForwardersSimple
  :: ForwardersMode
  -> FilePath
  -> Word
  -> Word
  -> IO ()
launchForwardersSimple mode p connSize disconnSize = withIOManager $ \iomgr ->
  runInLoop (launchForwardersSimple' iomgr mode p connSize disconnSize) p 1

launchForwardersSimple'
  :: IOManager
  -> ForwardersMode
  -> FilePath
  -> Word
  -> Word
  -> IO ()
launchForwardersSimple' iomgr mode p connSize disconnSize = do
  _now <- getCurrentTime
  case mode of
    Initiator ->
      doConnectToAcceptor
        (localSnocket iomgr)
        (localAddressFromPath p)
        noTimeLimitsHandshake
        (ekgConfig, tfConfig)
    Responder ->
      doListenToAcceptor
        (localSnocket iomgr)
        (localAddressFromPath p)
        noTimeLimitsHandshake
        (ekgConfig, tfConfig)
 where
  ekgConfig :: EKGF.ForwarderConfiguration
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer = nullTracer
      , EKGF.acceptorEndpoint = EKGF.LocalPipe p
      , EKGF.reConnectFrequency = 1.0
      , EKGF.actionOnRequest = const $ return ()
      }

  tfConfig :: TF.ForwarderConfiguration TraceObject
  tfConfig =
    TF.ForwarderConfiguration
      { TF.forwarderTracer = nullTracer
      , TF.acceptorEndpoint = p
      , TF.disconnectedQueueSize = disconnSize
      , TF.connectedQueueSize = connSize
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
                         , (forwardTraceObjectsInit tfConfig sink, 2)
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
