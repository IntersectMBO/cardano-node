{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Forwarding
  (
    initForwarding
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent.Async (race_, wait, withAsync)
import           Control.Monad.IO.Class

import           "contra-tracer" Control.Tracer (contramap, stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import           Data.Word (Word16)

import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (IOManager)
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
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder
import qualified Trace.Forward.Configuration as TF
import           Trace.Forward.Network.Forwarder
import           Trace.Forward.Protocol.Type (NodeInfo (..))
import           Trace.Forward.Utils

import           Cardano.Logging.Types


initForwarding :: forall m. (MonadIO m)
  => IOManager
  -> TraceConfig
  -> EKG.Store
  -> NodeInfo
  -> m (ForwardSink TraceObject)
initForwarding iomgr config ekgstore nodeInfo = liftIO $ do
  forwardSink <- initForwardSink tfConfig
  launchForwarders iomgr config ekgstore ekgConfig tfConfig forwardSink
  pure forwardSink
 where
  LocalSocket p = tofAddress $ tcForwarder config

  ekgConfig :: EKGF.ForwarderConfiguration
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer    = contramap show stdoutTracer
      , EKGF.acceptorEndpoint   = EKGF.LocalPipe p
      , EKGF.reConnectFrequency = 1.0
      , EKGF.actionOnRequest    = const $ pure ()
      }

  tfConfig :: TF.ForwarderConfiguration TraceObject
  tfConfig =
    TF.ForwarderConfiguration
      { TF.forwarderTracer       = contramap show stdoutTracer
      , TF.acceptorEndpoint      = TF.LocalPipe p
      , TF.getNodeInfo           = pure nodeInfo
      , TF.disconnectedQueueSize = 200000
      , TF.connectedQueueSize    = 2000
      }

launchForwarders
  :: IOManager
  -> TraceConfig
  -> EKG.Store
  -> EKGF.ForwarderConfiguration
  -> TF.ForwarderConfiguration TraceObject
  -> ForwardSink TraceObject
  -> IO ()
launchForwarders iomgr TraceConfig{tcForwarder} store ekgConfig tfConfig sink = flip
  withAsync
    wait
    $ runActionInLoop
        (launchForwardersViaLocalSocket iomgr tcForwarder (ekgConfig, tfConfig) sink store)
        (TF.LocalPipe p)
        1
 where
  LocalSocket p = tofAddress tcForwarder

launchForwardersViaLocalSocket
  :: IOManager
  -> TraceOptionForwarder
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> ForwardSink TraceObject
  -> EKG.Store
  -> IO ()
launchForwardersViaLocalSocket iomgr
  TraceOptionForwarder {tofAddress=(LocalSocket p), tofMode=Initiator}
  configs sink store =
    doConnectToAcceptor (localSnocket iomgr) (localAddressFromPath p)
      noTimeLimitsHandshake configs sink store
launchForwardersViaLocalSocket iomgr
  TraceOptionForwarder {tofAddress=(LocalSocket p), tofMode=Responder}
  configs sink store =
    doListenToAcceptor (localSnocket iomgr) (localAddressFromPath p)
      noTimeLimitsHandshake configs sink store

doConnectToAcceptor
  :: Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> ForwardSink TraceObject
  -> EKG.Store
  -> IO ()
doConnectToAcceptor snocket address timeLimits (ekgConfig, tfConfig) sink store = do
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
  -> ForwardSink TraceObject
  -> EKG.Store
  -> IO ()
doListenToAcceptor snocket address timeLimits (ekgConfig, tfConfig) sink store = do
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
