{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Metrics.Network.Forwarder
  ( connectToAcceptor
    -- | Export this function for Mux purpose.
  , forwardEKGMetrics
  , forwardEKGMetricsDummy
  , forwardEKGMetricsResp
  , forwardEKGMetricsRespDummy
  ) where

import           Codec.CBOR.Term (Term)
import qualified Codec.Serialise as CBOR
import           "contra-tracer" Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Data.Void (Void, absurd)
import qualified Network.Mux as Mux
import qualified Network.Socket as Socket
import           Network.TypedProtocol.Codec
import           Control.Exception (throwIO)
import           Ouroboros.Network.Context (MinimalInitiatorContext, ResponderContext)
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..), MiniProtocolLimits (..), MiniProtocolNum (..),
                                        OuroborosApplication (..), RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (VersionDataCodec, noTimeLimitsHandshake,
                                                             timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)

import           Ouroboros.Network.Protocol.Handshake.Version (Versions, acceptableVersion, queryVersion, simpleSingletonVersions)
import           Ouroboros.Network.Snocket (MakeBearer, Snocket,
                                            localAddressFromPath, localSnocket, socketSnocket,
                                            makeLocalBearer, makeSocketBearer)
import           Ouroboros.Network.Socket (NetworkConnectTracers(..), HandshakeCallbacks (..), ConnectToArgs (..), connectToNode, nullNetworkConnectTracers)
import qualified System.Metrics as EKG

import           System.Metrics.Configuration (ForwarderConfiguration (..), HowToConnect (..))
import           System.Metrics.Store.Forwarder (mkResponse, mkResponseDummy)
import qualified System.Metrics.Protocol.Forwarder as Forwarder
import qualified System.Metrics.Protocol.Codec as Forwarder
import           System.Metrics.Store.Deltify


connectToAcceptor
  :: ForwarderConfiguration
  -> EKG.Store
  -> IO ()
connectToAcceptor config@ForwarderConfiguration{..} ekgStore = withIOManager \iocp ->
  let app = forwarderApp config ekgStore
  in case acceptorEndpoint of
    LocalPipe localPipe -> do
      let snocket = localSnocket iocp
          address = localAddressFromPath localPipe
      doConnectToAcceptor snocket makeLocalBearer mempty address noTimeLimitsHandshake app
    RemoteSocket host port -> do
      acceptorAddr:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      let snocket = socketSnocket iocp
          address = Socket.addrAddress acceptorAddr
      doConnectToAcceptor snocket makeSocketBearer mempty address timeLimitsHandshake app

doConnectToAcceptor
  :: forall fd addr. ()
  => Snocket IO fd addr
  -> MakeBearer IO fd
  -> (fd -> IO ()) -- ^ configure socket
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'Mux.InitiatorMode
                          (MinimalInitiatorContext addr)
                          (ResponderContext addr)
                          LBS.ByteString IO () Void
  -> IO ()
doConnectToAcceptor snocket makeBearer configureSocket address timeLimits app =

  let
    connectToArgs :: ConnectToArgs fd addr UnversionedProtocol UnversionedProtocolData
    connectToArgs = ConnectToArgs
      { ctaHandshakeCodec = unversionedHandshakeCodec
     :: Codec (Handshake UnversionedProtocol Term) CBOR.DeserialiseFailure IO LBS.ByteString
      , ctaHandshakeTimeLimits = timeLimits
     :: ProtocolTimeLimits (Handshake UnversionedProtocol Term)
      , ctaVersionDataCodec = unversionedProtocolDataCodec
     :: VersionDataCodec Term UnversionedProtocol UnversionedProtocolData
      , ctaConnectTracers = nullNetworkConnectTracers
     :: NetworkConnectTracers addr UnversionedProtocol
      , ctaHandshakeCallbacks = HandshakeCallbacks acceptableVersion queryVersion
     :: HandshakeCallbacks UnversionedProtocolData
      }

    versions :: Versions UnversionedProtocol UnversionedProtocolData
      (OuroborosApplication 'Mux.InitiatorMode (MinimalInitiatorContext addr) (ResponderContext addr) LBS.ByteString IO () Void)
    versions = simpleSingletonVersions
       UnversionedProtocol
       UnversionedProtocolData
       (const app)

    localAddress  :: Maybe addr
    remoteAddress :: addr
    (localAddress, remoteAddress) = (Nothing, address)

  in do
    res <- connectToNode
      snocket
      makeBearer
      connectToArgs
      configureSocket
      versions
      localAddress
      remoteAddress

    case res of
      Left err -> throwIO err
      Right (Left ()) -> pure ()
      Right (Right void) -> absurd void

forwarderApp
  :: ForwarderConfiguration
  -> EKG.Store
  -> OuroborosApplication 'Mux.InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
forwarderApp config ekgStore =
  OuroborosApplication
    [ MiniProtocol
        { miniProtocolNum    = MiniProtocolNum 2
        , miniProtocolStart  = Mux.StartEagerly
        , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
        , miniProtocolRun    = if useDummyForwarder config then forwardEKGMetricsDummy else forwardEKGMetrics config ekgStore
        }
    ]

forwardEKGMetrics
  :: ForwarderConfiguration
  -> EKG.Store
  -> RunMiniProtocol 'Mux.InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
forwardEKGMetrics config ekgStore =
  InitiatorProtocolOnly $ MiniProtocolCb \_ctx channel -> do
    deltify <- mkDeltify
    runPeer
      (forwarderTracer config)
      (Forwarder.codecEKGForward CBOR.encode CBOR.decode
                                 CBOR.encode CBOR.decode)
      channel
      (Forwarder.ekgForwarderPeer $ mkResponse config deltify ekgStore)

forwardEKGMetricsResp
  :: ForwarderConfiguration
  -> EKG.Store
  -> RunMiniProtocol 'Mux.ResponderMode initiatorCtx responderCtx LBS.ByteString IO Void ()
forwardEKGMetricsResp config ekgStore =
  ResponderProtocolOnly $ MiniProtocolCb \_ctx channel -> do
    deltify <- mkDeltify
    runPeer
      (forwarderTracer config)
      (Forwarder.codecEKGForward CBOR.encode CBOR.decode
                                 CBOR.encode CBOR.decode)
      channel
      (Forwarder.ekgForwarderPeer $ mkResponse config deltify ekgStore)

forwardEKGMetricsDummy
  :: RunMiniProtocol 'Mux.InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
forwardEKGMetricsDummy =
  InitiatorProtocolOnly $ MiniProtocolCb \_ctx channel ->
    runPeer
      nullTracer
      (Forwarder.codecEKGForward CBOR.encode CBOR.decode
                                 CBOR.encode CBOR.decode)
      channel
      (Forwarder.ekgForwarderPeer mkResponseDummy)

forwardEKGMetricsRespDummy
  :: RunMiniProtocol 'Mux.ResponderMode initiatorCtx responderCtx LBS.ByteString IO Void ()
forwardEKGMetricsRespDummy =
  ResponderProtocolOnly $ MiniProtocolCb \_ctx channel ->
    runPeer
      nullTracer
      (Forwarder.codecEKGForward CBOR.encode CBOR.decode
                                 CBOR.encode CBOR.decode)
      channel
      (Forwarder.ekgForwarderPeer mkResponseDummy)
