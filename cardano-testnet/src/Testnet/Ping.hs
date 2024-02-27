{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Ping
  ( pingNode
  , checkSprocket
  , waitForSprocket
  , TestnetMagic
  , PingClientError(..)
  ) where

import           Cardano.Api (Error (..))

import           Cardano.Network.Ping (HandshakeFailure, NodeVersion (..), handshakeDec,
                   handshakeReq, isSameVersionAndMagic, supportedNodeToClientVersions)

import qualified Codec.CBOR.Read as CBOR
import           Control.Exception.Safe
import           Control.Monad (when)
import           Control.Monad.Class.MonadTime.SI (Time)
import qualified Control.Monad.Class.MonadTimer.SI as MT
import           Control.Monad.IO.Class
import           Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.Either (isLeft)
import           Data.IORef
import qualified Data.List as L
import           Data.Word (Word32)
import           Network.Mux.Bearer (MakeBearer (..), makeSocketBearer)
import           Network.Mux.Timeout (TimeoutFn, withTimeoutSerial)
import           Network.Mux.Types (MiniProtocolDir (InitiatorDir), MiniProtocolNum (..),
                   MuxBearer (read, write), MuxSDU (..), MuxSDUHeader (..),
                   RemoteClockModel (RemoteClockModel))
import           Network.Socket (AddrInfo (..), StructLinger (..))
import qualified Network.Socket as Socket
import           Prettyprinter

import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO

type TestnetMagic = Word32

-- | Mini protocol number. We're only sending ping, so 0.
handshakeNum ::  MiniProtocolNum
handshakeNum = MiniProtocolNum 0

-- | Timeout for reading a multiplexer service data unit, in seconds.
sduTimeout :: MT.DiffTime
sduTimeout = 30

-- | Perform handshake query to obtain supported version numbers by node.
doHandshakeQuery :: Bool
doHandshakeQuery = True

-- | Ping the node once
pingNode :: MonadIO m
         => TestnetMagic -- ^ testnet magic
         -> IO.Sprocket  -- ^ node sprocket
         -> m (Either PingClientError ()) -- ^ '()' means success
pingNode networkMagic sprocket = liftIO $ bracket
  (Socket.socket (Socket.addrFamily peer) Socket.Stream Socket.defaultProtocol)
  Socket.close
  (\sd -> withTimeoutSerial $ \timeoutfn -> do
    when (Socket.addrFamily peer /= Socket.AF_UNIX) $ do
      Socket.setSocketOption sd Socket.NoDelay 1
      Socket.setSockOpt sd Socket.Linger
        StructLinger
          { sl_onoff  = 1
          , sl_linger = 0
          }

    Socket.connect sd (Socket.addrAddress peer)
    peerStr <- peerString

    bearer <- getBearer makeSocketBearer sduTimeout nullTracer sd

    let versions = supportedNodeToClientVersions networkMagic
    !_ <- write bearer timeoutfn $ wrap handshakeNum InitiatorDir (handshakeReq versions doHandshakeQuery)
    (msg, !_) <- nextMsg bearer timeoutfn handshakeNum

    pure $ case CBOR.deserialiseFromBytes handshakeDec msg of
      Left err -> Left $ PceDecodingError peerStr err
      Right (_, Left err) -> Left $ PceProtocolError peerStr err
      Right (_, Right recVersions)
        | areVersionsAccepted versions recVersions -> pure ()
        | otherwise -> Left $ PceVersionNegotiationError peerStr versions recVersions
  )
  where
    peer = sprocketToAddrInfo sprocket :: AddrInfo

    -- | Wrap a message in a mux service data unit.
    wrap :: MiniProtocolNum -> MiniProtocolDir -> LBS.ByteString -> MuxSDU
    wrap mhNum mhDir msBlob = MuxSDU
      { msHeader = MuxSDUHeader
        { mhTimestamp = RemoteClockModel 0
        , mhNum
        , mhDir
        , mhLength    = fromIntegral $ LBS.length msBlob
        }
      , msBlob
      }

    areVersionsAccepted :: [NodeVersion] -> [NodeVersion] -> Bool
    areVersionsAccepted accVersions recVersions =
      let intersects = L.intersectBy isSameVersionAndMagic recVersions accVersions in
      not $ null intersects

    peerString :: IO String
    peerString =
      case Socket.addrFamily peer of
        Socket.AF_UNIX -> pure . show $ Socket.addrAddress peer
        _ -> do
          (Just host, Just port) <-
            Socket.getNameInfo
              [Socket.NI_NUMERICHOST, Socket.NI_NUMERICSERV]
              True True (Socket.addrAddress peer)
          pure $ host <> ":" <> port

    -- | Fetch next message from mux bearer. Ignores messages not matching handshake protocol number.
    nextMsg :: MuxBearer IO -- ^ a mux bearer
            -> TimeoutFn IO -- ^ timeout function, for reading messages
            -> MiniProtocolNum -- ^ handshake protocol number
            -> IO (LBS.ByteString, Time) -- ^ raw message and timestamp
    nextMsg bearer timeoutfn ptclNum = do
      (sdu, t_e) <- Network.Mux.Types.read bearer timeoutfn
      if mhNum (msHeader sdu) == ptclNum
        then pure (msBlob sdu, t_e)
        else nextMsg bearer timeoutfn ptclNum

-- | Wait for 'sprocket' to become ready. Periodically tries to connect to 'sprocket', with the provided interval.
-- If there was no success within 'timeout' period, return the last exception thrown during a connection
-- attempt.
waitForSprocket :: MonadIO m
                => MT.DiffTime -- ^ timeout
                -> MT.DiffTime -- ^ interval
                -> IO.Sprocket
                -> m (Either IOException ())
waitForSprocket timeout interval sprocket = liftIO $ do
  lastResult <- newIORef (Right ())
  _ <- MT.timeout timeout $ loop lastResult
  readIORef lastResult
  where
    loop lastResult = do
      r <- checkSprocket sprocket
      writeIORef lastResult r
      when (isLeft r) $ do
        -- repeat on error
        MT.threadDelay interval
        loop lastResult

-- | Check if the sprocket can be connected to. Returns an exception thrown during the connection attempt.
checkSprocket :: MonadIO m => IO.Sprocket -> m (Either IOException ())
checkSprocket sprocket = liftIO $ do
  let AddrInfo{addrFamily, addrSocketType, addrProtocol, addrAddress} = sprocketToAddrInfo sprocket
  bracket (Socket.socket addrFamily addrSocketType addrProtocol) Socket.close $ \sock -> do
    -- Capture only synchronous exceptions from the connection attempt.
    catch (Socket.connect sock addrAddress >> pure (pure ())) $ \e ->
      pure (Left e)

sprocketToAddrInfo :: IO.Sprocket -> AddrInfo
sprocketToAddrInfo sprocket = do
  let socketAbsPath = IO.sprocketSystemName sprocket
  Socket.AddrInfo
    [] Socket.AF_UNIX Socket.Stream
    Socket.defaultProtocol (Socket.SockAddrUnix socketAbsPath) Nothing


data PingClientError
  = PceDecodingError
      !String -- ^ peer string
      !CBOR.DeserialiseFailure -- ^ deserialization exception
  | PceProtocolError
      !String -- ^ peer string
      !HandshakeFailure -- ^ handshake exception
  | PceVersionNegotiationError
      !String -- ^ peer string
      ![NodeVersion] -- ^ requested versions
      ![NodeVersion] -- ^ received node versions

instance Error PingClientError where
  prettyError = \case
    PceDecodingError peerStr exception -> pretty peerStr <+> "Decoding error:" <+> pretty (displayException exception)
    PceProtocolError peerStr exception -> pretty peerStr <+> "Protocol error:" <+> viaShow exception
    PceVersionNegotiationError peerStr requestedVersions receivedVersions -> vsep
      [ pretty peerStr <+> "Version negotiation error: No overlapping versions with" <+> viaShow requestedVersions
      , "Received versions:" <+> viaShow receivedVersions
      ]


