{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.Forward
  (
    forwardTracer
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent.Async (race_, wait, withAsync)
import           Control.Monad.IO.Class

import qualified Control.Tracer as T
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

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types
import           Cardano.Logging.Utils(uncurry3)

---------------------------------------------------------------------------

forwardTracer :: forall m. (MonadIO m)
  => IOManager
  -> TraceConfig
  -> NodeInfo
  -> m (Trace m FormattedMessage)
forwardTracer iomgr config nodeInfo = liftIO $ do
  forwardSink <- initForwardSink tfConfig
  store <- EKG.newStore
  EKG.registerGcMetrics store
  launchForwarders iomgr config store ekgConfig tfConfig forwardSink
  pure $ Trace $ T.arrow $ T.emit $ uncurry3 (output forwardSink)
 where
  output ::
       ForwardSink TraceObject
    -> LoggingContext
    -> Maybe TraceControl
    -> FormattedMessage
    -> m ()
  output sink LoggingContext {} Nothing (FormattedForwarder lo) = liftIO $
    writeToSink sink lo
  output _sink LoggingContext {} (Just Reset) _msg = liftIO $ do
    pure ()
  output _sink lk (Just c@Document {}) (FormattedForwarder lo) = do
    docIt Forwarder (FormattedHuman False "") (lk, Just c, lo)
  output _sink LoggingContext {} _ _a = pure ()

  LocalSocket p = tcForwarder config

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
launchForwarders iomgr TraceConfig{tcForwarder, tcForwarderMode} store ekgConfig tfConfig sink = flip
  withAsync
    wait
    $ runActionInLoop
        (launchForwardersViaLocalSocket iomgr tcForwarder tcForwarderMode (ekgConfig, tfConfig) sink store)
        (TF.LocalPipe p)
        1
 where
  LocalSocket p = tcForwarder

launchForwardersViaLocalSocket
  :: IOManager
  -> ForwarderAddr
  -> ForwarderMode
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> ForwardSink TraceObject
  -> EKG.Store
  -> IO ()
launchForwardersViaLocalSocket iomgr (LocalSocket p) Initiator configs sink store =
  doConnectToAcceptor (localSnocket iomgr) (localAddressFromPath p) noTimeLimitsHandshake configs sink store
launchForwardersViaLocalSocket iomgr (LocalSocket p) Responder configs sink store =
  doListenToAcceptor (localSnocket iomgr) (localAddressFromPath p) noTimeLimitsHandshake configs sink store

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
