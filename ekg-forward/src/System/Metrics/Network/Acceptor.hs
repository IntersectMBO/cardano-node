{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module System.Metrics.Network.Acceptor
  ( listenToForwarder
  -- | Export this function for Mux purpose.
  , acceptEKGMetricsInit
  , acceptEKGMetricsResp
  , acceptMetricsInit
  , acceptMetricsResp
  ) where

import           Codec.CBOR.Term (Term)
import qualified Codec.Serialise as CBOR
import           Control.Exception (finally)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (wait)
import           Control.Concurrent.STM.TVar (TVar, readTVarIO)
import           Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime)
import           Data.Void (Void)
import qualified Network.Mux as Mux
import qualified Network.Socket as Socket
import           Ouroboros.Network.Context (MinimalInitiatorContext, ResponderContext)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..),
                                        MiniProtocolLimits (..),
                                        MiniProtocolNum (..),
                                        OuroborosApplication (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Snocket (LocalAddress, MakeBearer, Snocket,
                                            localAddressFromPath, localSnocket, socketSnocket,
                                            makeLocalBearer, makeSocketBearer)
import           Ouroboros.Network.Socket (SomeResponderApplication (..))

import qualified Ouroboros.Network.Server.Simple as OServer
import           Ouroboros.Network.Protocol.Handshake (HandshakeArguments(..),
                                                       noTimeLimitsHandshake,
                                                       simpleSingletonVersions,
                                                       Acceptable(..),
                                                       Queryable(..), Handshake)
import           Ouroboros.Network.Protocol.Handshake.Codec (timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Limits (ProtocolTimeLimits)

import qualified System.Metrics.Protocol.Acceptor as Acceptor
import qualified System.Metrics.Protocol.Codec as Acceptor
import           System.Metrics.ReqResp (Request (..), Response (..))
import           System.Metrics.Configuration (AcceptorConfiguration (..), HowToConnect (..))
import qualified System.Metrics as EKG
import System.Metrics.Store.Acceptor (MetricsLocalStore, storeMetrics)

listenToForwarder
  :: AcceptorConfiguration
  -> (ResponderContext (Either LocalAddress Socket.SockAddr) -> IO store)
  -> (ResponderContext (Either LocalAddress Socket.SockAddr) -> store -> Response -> IO ())
  -> (ResponderContext (Either LocalAddress Socket.SockAddr) -> IO ())
  -> IO Void
listenToForwarder config mkStore insertStore peerErrorHandler = withIOManager $ \iocp -> do
  case forwarderEndpoint config of
    LocalPipe localPipe -> do
      let app = acceptorApp config (mkStore . fmap Left) (insertStore . fmap Left) (peerErrorHandler . fmap Left)
          snocket = localSnocket iocp
          address = localAddressFromPath localPipe
          configureSocket = mempty
      doListenToForwarder snocket makeLocalBearer configureSocket address noTimeLimitsHandshake app
    RemoteSocket host port -> do
      listenAddress:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      let app = acceptorApp config (mkStore . fmap Right) (insertStore . fmap Right) (peerErrorHandler . fmap Right)
          snocket = socketSnocket iocp
          address = Socket.addrAddress listenAddress
          configureSocket fd _addr =
            Socket.setSocketOption fd Socket.ReuseAddr 1
      doListenToForwarder snocket makeSocketBearer configureSocket address timeLimitsHandshake app

doListenToForwarder
  :: Snocket IO fd addr
  -> MakeBearer IO fd
  -> (fd -> addr -> IO ()) -- ^ configure socket
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'Mux.ResponderMode
                          (MinimalInitiatorContext addr)
                          (ResponderContext addr)
                          LBS.ByteString IO Void ()
  -> IO Void
doListenToForwarder snocket makeBearer configureSocket address timeLimits app = do
  OServer.with
    snocket
    nullTracer
    Mux.nullTracers
    makeBearer
    configureSocket
    address
    HandshakeArguments {
      haBearerTracer     = nullTracer,
      haHandshakeTracer  = nullTracer,
      haHandshakeCodec   = unversionedHandshakeCodec,
      haVersionDataCodec = unversionedProtocolDataCodec,
      haAcceptVersion    = acceptableVersion,
      haQueryVersion     = queryVersion,
      haTimeLimits       = timeLimits
    }
    (simpleSingletonVersions
      UnversionedProtocol
      UnversionedProtocolData
      (\_ -> SomeResponderApplication app))
    $ \_ serverAsync -> wait serverAsync -- Block until async exception.

acceptorApp
  :: AcceptorConfiguration
  -> (ResponderContext addr -> IO store)
  -> (ResponderContext addr -> store -> Response -> IO ())
  -> (ResponderContext addr -> IO ())
  -> OuroborosApplication 'Mux.ResponderMode
                          (MinimalInitiatorContext addr)
                          (ResponderContext addr)
                          LBS.ByteString IO Void ()
acceptorApp config mkStore insertStore peerErrorHandler =
  OuroborosApplication [
    MiniProtocol
      { miniProtocolNum    = MiniProtocolNum 2
      , miniProtocolStart  = Mux.StartEagerly
      , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
      , miniProtocolRun    = acceptMetricsResp config mkStore insertStore peerErrorHandler
      }
  ]

{-# INLINE acceptMetricsResp #-}
acceptMetricsResp
  :: AcceptorConfiguration
  -> (responderCtx -> IO store)
  -> (responderCtx -> store -> Response -> IO ())
  -> (responderCtx -> IO ())
  -> RunMiniProtocol 'Mux.ResponderMode initiatorCtx responderCtx LBS.ByteString IO Void ()
acceptMetricsResp config mkStore insertStore peerErrorHandler =
  ResponderProtocolOnly $ runPeerWithStores config mkStore insertStore peerErrorHandler

{-# INLINE acceptMetricsInit #-}
acceptMetricsInit
  :: AcceptorConfiguration
  -> (initiatorCtx -> IO store)
  -> (initiatorCtx -> store -> Response -> IO ())
  -> (initiatorCtx -> IO ())
  -> RunMiniProtocol 'Mux.InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
acceptMetricsInit config mkStore insertStore peerErrorHandler =
  InitiatorProtocolOnly $ runPeerWithStores config mkStore insertStore peerErrorHandler

acceptEKGMetricsResp
  :: AcceptorConfiguration
  -> (responderCtx -> IO (EKG.Store, TVar MetricsLocalStore))
  -> (responderCtx -> IO ())
  -> RunMiniProtocol 'Mux.ResponderMode initiatorCtx responderCtx LBS.ByteString IO Void ()
acceptEKGMetricsResp config mkStore =
  acceptMetricsResp config mkStore insertStore where
    insertStore _ (a, b) resp = storeMetrics resp a b

acceptEKGMetricsInit
  :: AcceptorConfiguration
  -> (initiatorCtx -> IO (EKG.Store, TVar MetricsLocalStore))
  -> (initiatorCtx -> IO ())
  -> RunMiniProtocol 'Mux.InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
acceptEKGMetricsInit config mkStore =
  acceptMetricsInit config mkStore insertStore where
    insertStore _ (a, b) resp = storeMetrics resp a b

runPeerWithStores
  :: AcceptorConfiguration
  -> (ctx -> IO store)
  -> (ctx -> store -> Response -> IO ())
  -> (ctx -> IO ())
  -> MiniProtocolCb ctx LBS.ByteString IO ()
runPeerWithStores config mkStore insertStore peerErrorHandler =
  MiniProtocolCb $ \ctx channel -> do
    st <- mkStore ctx
    runPeer
      (acceptorTracer config)
      (Acceptor.codecEKGForward CBOR.encode CBOR.decode
                                CBOR.encode CBOR.decode)
      channel
      (Acceptor.ekgAcceptorPeer $ acceptorActions True config (insertStore ctx st))
    `finally` peerErrorHandler ctx

acceptorActions
  :: Bool
  -> AcceptorConfiguration
  -> (Response -> IO ())
  -> Acceptor.EKGAcceptor Request Response IO ()
acceptorActions True config@AcceptorConfiguration{..} insertStore =
  Acceptor.SendMsgReq whatToRequest $ \response -> do
    insertStore response
    threadDelay $ toMicroSecs requestFrequency
    weAreDone <- readTVarIO shouldWeStop
    if weAreDone
      then return $ acceptorActions False config insertStore
      else return $ acceptorActions True  config insertStore
 where
  toMicroSecs :: NominalDiffTime -> Int
  toMicroSecs dt = fromEnum dt `div` 1000000

acceptorActions False _ _ =
  Acceptor.SendMsgDone $ return ()
