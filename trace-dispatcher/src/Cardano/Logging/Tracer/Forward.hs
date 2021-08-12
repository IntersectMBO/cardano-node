{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Logging.Tracer.Forward
  (
    forwardTracer
  ) where

import           Codec.CBOR.Term (Term)
import           Codec.Serialise (Serialise (..))
import           Control.Concurrent.Async (race_, wait, withAsync)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO,
                     writeTBQueue)
import           Control.Monad.IO.Class
import           Control.Monad.STM (atomically)
import           GHC.Generics (Generic)

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
                     newNetworkMutableState, nullNetworkServerTracers,
                     withServerNode)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

import qualified System.Metrics as EKG
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder (forwardEKGMetricsResp)
import qualified Trace.Forward.Configuration as TF
import           Trace.Forward.Network.Forwarder (forwardTraceObjectsResp)
import           Trace.Forward.Protocol.Type (NodeInfo (..))
import           Trace.Forward.Utils (runActionInLoop)

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types

-- Instances for 'TraceObject' to forward it using 'trace-forward' library.

deriving instance Generic Privacy
deriving instance Generic SeverityS
deriving instance Generic LoggingContext
deriving instance Generic TraceObject

instance Serialise DetailLevel
instance Serialise Privacy
instance Serialise SeverityS
instance Serialise LoggingContext
instance Serialise TraceObject

instance ShowProxy TraceObject

---------------------------------------------------------------------------

-- newtype ForwardTracerState = ForwardTracerState {
--     ftQueue   :: TBQueue TraceObject
--   }

forwardTracer :: forall m. (MonadIO m)
  => IOManager
  -> TraceConfig
  -> NodeInfo
  -> m (Trace m FormattedMessage)
forwardTracer iomgr config nodeInfo = liftIO $ do
    tbQueue <- newTBQueueIO (fromIntegral (tcForwarderQueueSize config))
    store <- EKG.newStore
    EKG.registerGcMetrics store
    launchForwarders iomgr (tcForwarder config) nodeInfo tbQueue store
--    stateRef <- liftIO $ newIORef (ForwardTracerState tbQueue)
    pure $ Trace $ T.arrow $ T.emit $ uncurry3 (output tbQueue)
  where
    output ::
         TBQueue TraceObject
      -> LoggingContext
      -> Maybe TraceControl
      -> FormattedMessage
      -> m ()
    output tbQueue LoggingContext {} Nothing (FormattedForwarder lo) = liftIO $ do
      atomically $ writeTBQueue tbQueue lo
    output _tbQueue LoggingContext {} (Just Reset) _msg = liftIO $ do
      pure ()
    output _tbQueue lk (Just c@Document {}) (FormattedForwarder lo) = do
      docIt Forwarder (FormattedHuman False "") (lk, Just c, lo)
    output _tbQueue LoggingContext {} _ _a = pure ()

launchForwarders
  :: IOManager
  -> RemoteAddr
  -> NodeInfo
  -> TBQueue TraceObject
  -> EKG.Store
  -> IO ()
launchForwarders iomgr ep@(LocalSocket p) nodeInfo tbQueue store = flip
  withAsync
    wait
    $ runActionInLoop
        (launchForwardersViaLocalSocket iomgr ep (ekgConfig, tfConfig) tbQueue store)
        (TF.LocalPipe p)
        1
 where
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
      { TF.forwarderTracer  = contramap show stdoutTracer
      , TF.acceptorEndpoint = TF.LocalPipe p
      , TF.getNodeInfo      = pure nodeInfo
      }

launchForwardersViaLocalSocket
  :: IOManager
  -> RemoteAddr
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> TBQueue TraceObject
  -> EKG.Store
  -> IO ()
launchForwardersViaLocalSocket iomgr (LocalSocket localSock) configs tbQueue store = do
  let snocket = localSnocket iomgr localSock
      address = localAddressFromPath localSock
  doListenToAcceptor snocket address noTimeLimitsHandshake configs tbQueue store

doListenToAcceptor
  :: Ord addr
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> TBQueue TraceObject
  -> EKG.Store
  -> IO ()
doListenToAcceptor snocket address timeLimits (ekgConfig, tfConfig) tbQueue store = do
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
                forwarderApp [ (forwardEKGMetricsResp   ekgConfig store,  1)
                             , (forwardTraceObjectsResp tfConfig tbQueue, 2)
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

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c
