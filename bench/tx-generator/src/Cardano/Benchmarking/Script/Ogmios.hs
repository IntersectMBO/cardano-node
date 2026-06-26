{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Cardano.Benchmarking.Script.Ogmios
Description : Ogmios backend for the transaction submission transport.

An <https://ogmios.dev Ogmios> WebSocket backend for the generic
'Cardano.Benchmarking.Script.Submission.SubmitTransport': transactions are
submitted as JSON-RPC 2.0 @submitTransaction@ calls.

== What this is (and is not)

This is a __functional submission transport, not a benchmarking one__:

* one transaction per round trip, over a single WebSocket connection;
* TPS pacing is ignored;
* no submission metrics — only a sent/failed count, traced at debug level.

__Using a submission endpoint requires @debugMode: true@__ — the high-level
config compiler rejects a @submissionEndpointURI@ config that does not also
set it.

== Throughput

Throughput is bounded by the round-trip time to the endpoint: at most one
request is in flight at a time.

Ogmios /can/ pipeline many requests per connection, correlated by the
JSON-RPC @id@. A paced sender/receiver pair with an in-flight window is the
natural next step, should this transport ever need to carry benchmark-grade
load. Until then, __do not draw throughput conclusions from runs submitted
this way__.

== What still uses the local node

Only transaction /submission/ goes through Ogmios. Everything else talks to
the local node directly:

* protocol-parameter and era queries;
* protocol startup.

So tx-generator still needs local node access (socket + config file) even
when submitting to a remote endpoint.

== Rejected transactions

A rejected transaction __fails the run__: the process exits non-zero, so
exit codes can be trusted in scripts. /How/ it fails depends on the phase:

* __Setup__ (genesis import, splitting) — chained transactions, so it aborts
  at the first rejection; everything after it would be doomed anyway.
* __Benchmark__ — independent transactions, so the whole stream is submitted,
  then the action fails afterwards if anything was rejected.
-}
module Cardano.Benchmarking.Script.Ogmios
  ( parseOgmiosUrl
  , withOgmiosTransport
  ) where

import           Cardano.Api (IsShelleyBasedEra, Tx, serialiseToCBOR)

import           Cardano.Benchmarking.Script.Submission (SubmitTransport (..))
import           Cardano.TxGenerator.Types (TxGenError (..))

import           Prelude

import           Control.Exception (Exception, Handler (..), IOException, catches, throwIO)
import           Control.Monad (unless, when)
import           Data.Aeson (Value (..), object, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import           Data.Either.Extra (maybeToEither)
import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.URI (parseURI, uriAuthority, uriPath, uriPort, uriRegName, uriScheme)
import qualified Network.WebSockets as WS
import           Prettyprinter (Pretty (..), parens, (<+>))
import           System.Timeout (timeout)
import           Text.Read (readMaybe)

-- | The port Ogmios listens on by default.
defaultOgmiosPort :: Int
defaultOgmiosPort = 1337

-- | Per-request response timeout in microseconds. Generous, because the
-- node may hold a submission back while its mempool is saturated.
responseTimeout :: Int
responseTimeout = 90_000_000

-- | A transaction rejection reported by Ogmios, carried as the error type of
-- the 'SubmitTransport' and rendered for tracing via 'Pretty'. Deliberately
-- not exported: the submission loop only needs to render it, so no other
-- module should depend on its shape.
data OgmiosRejection = OgmiosRejection !Int !Text !Value

instance Pretty OgmiosRejection where
  pretty (OgmiosRejection code msg dat) =
    "Ogmios submit failed:" <+> pretty msg <+> parens ("code" <+> pretty code)
      <> case dat of
           Null -> mempty
           _    -> ", details:" <+> pretty (Text.decodeUtf8 (LBS.toStrict (Aeson.encode dat)))

-- | A protocol-level fault: no or late response, an unparseable response, or a
-- mismatched JSON-RPC id. The connection is out of sync and unusable, so this
-- aborts the whole run rather than counting as a transaction rejection.
newtype OgmiosProtocolError = OgmiosProtocolError String
  deriving Show

instance Exception OgmiosProtocolError

-- | Open a WebSocket connection to an Ogmios endpoint and run an action with a
-- 'SubmitTransport' backed by it. Connection-level and protocol faults are
-- surfaced as a 'Left' 'TxGenError'.
withOgmiosTransport
  :: forall era a. IsShelleyBasedEra era
  => String
  -> (SubmitTransport era OgmiosRejection -> IO (Either TxGenError a))
  -> IO (Either TxGenError a)
withOgmiosTransport url use =
  case parseOgmiosUrl url of
    Left err -> return $ Left $ TxGenError err
    Right (host, port, path) ->
      WS.runClient host port path runWithConn
        `catches`
          [ Handler $ \(e :: WS.HandshakeException)  -> connectionFailure e
          , Handler $ \(e :: WS.ConnectionException) -> connectionFailure e
          , Handler $ \(e :: IOException)            -> connectionFailure e
          , Handler $ \(OgmiosProtocolError m)       -> return $ Left $ TxGenError $ "Ogmios: " ++ m
          ]
 where
  runWithConn conn = do
    -- a per-connection counter mirrors each request's JSON-RPC id, so a
    -- response can be matched back to the request that produced it
    reqIdRef <- newIORef 0
    use SubmitTransport { submitOne = ogmiosSubmitOne conn reqIdRef }
  connectionFailure :: Show e => e -> IO (Either TxGenError a)
  connectionFailure e = return $ Left $ TxGenError $ "Ogmios connection failure: " ++ show e

-- | Submit a single transaction and await its response on the same connection.
-- A transaction rejection is returned as 'Left'; a protocol-level fault throws
-- 'OgmiosProtocolError' (caught by 'withOgmiosTransport').
ogmiosSubmitOne
  :: IsShelleyBasedEra era
  => WS.Connection -> IORef Int -> Tx era -> IO (Either OgmiosRejection ())
ogmiosSubmitOne conn reqIdRef tx = do
  reqId <- atomicModifyIORef' reqIdRef $ \n -> (n + 1, n)
  WS.sendTextData conn $ Aeson.encode (mkSubmitRequest tx reqId)
  mResp <- timeout responseTimeout $ WS.receiveData conn
  case mResp of
    Nothing -> throwIO $ OgmiosProtocolError $
      "no response to request " ++ show reqId
        ++ " within " ++ show (responseTimeout `div` 1_000_000) ++ "s"
    Just resp -> case parseOgmiosResponse resp of
      Left parseErr -> throwIO $ OgmiosProtocolError $ "response parse error: " ++ parseErr
      Right (respId, result)
        -- a mismatched (or null) id means the connection is out of sync
        | respId /= Just reqId -> throwIO $ OgmiosProtocolError $
            "response id mismatch: expected " ++ show reqId
              ++ ", got " ++ show respId ++ " (" ++ describe result ++ ")"
        | OgmiosError code errMsg errData <- result ->
            return $ Left $ OgmiosRejection code errMsg errData
        | otherwise -> return $ Right ()
 where
  describe (OgmiosSuccess txId)  = "success, tx " ++ Text.unpack txId
  describe (OgmiosError _ msg _) = "error: " ++ Text.unpack msg

parseOgmiosUrl :: String -> Either String (String, Int, String)
parseOgmiosUrl urlStr = do
  uri <- maybeToEither ("Invalid Ogmios URL: " ++ urlStr) $ parseURI urlStr
  -- WS.runClient speaks plaintext TCP only, so accepting wss:// (or any
  -- other scheme) here would silently drop the security the URL asks for.
  unless (uriScheme uri == "ws:") $
    Left $ "Unsupported scheme in Ogmios URL (only plain ws:// is supported): " ++ urlStr
  auth <- maybeToEither ("No authority in Ogmios URL: " ++ urlStr) $ uriAuthority uri
  when (null $ uriRegName auth) $
    Left $ "No host in Ogmios URL: " ++ urlStr
  port <- case uriPort auth of
    ""    -> Right defaultOgmiosPort
    ":"   -> Right defaultOgmiosPort
    ':':p -> parsePort p
    p     -> parsePort p
  let path = case uriPath uri of
        "" -> "/"
        p  -> p
  return (uriRegName auth, port, path)
 where
  parsePort p = case readMaybe p of
    Just n | n >= 1 && n <= 65_535 -> Right n
    _ -> Left $ "Invalid port in Ogmios URL: " ++ urlStr

mkSubmitRequest :: IsShelleyBasedEra era => Tx era -> Int -> Value
mkSubmitRequest tx reqId = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "method"  .= ("submitTransaction" :: Text)
  , "params"  .= object
      [ "transaction" .= object
          [ "cbor" .= Text.decodeUtf8 (Base16.encode (serialiseToCBOR tx))
          ]
      ]
  , "id"      .= reqId
  ]

data OgmiosResult
  = OgmiosSuccess Text
  | OgmiosError Int Text Value
  deriving (Eq, Show)

-- | Parse a JSON-RPC 2.0 response to @submitTransaction@, returning the
-- mirrored request id alongside the result.
parseOgmiosResponse :: LBS.ByteString -> Either String (Maybe Int, OgmiosResult)
parseOgmiosResponse bs =
  case Aeson.eitherDecode bs of
    Left err -> Left err
    Right val -> Aeson.parseEither parseResponse val
 where
  parseResponse = Aeson.withObject "OgmiosResponse" $ \obj -> do
    respId <- obj .:? "id"
    mResult <- obj .:? "result"
    result <- case mResult of
      Just resultVal -> do
        txId <- Aeson.withObject "result" (\r -> do
          txObj <- r .: "transaction"
          Aeson.withObject "transaction" (.: "id") txObj
          ) resultVal
        return $ OgmiosSuccess txId
      Nothing -> do
        errVal <- obj .: "error"
        Aeson.withObject "error" (\errObj -> do
          code <- errObj .: "code"
          msg  <- errObj .: "message"
          dat  <- errObj .:? "data"
          return $ OgmiosError code msg (fromMaybe Null dat)
          ) errVal
    return (respId, result)
