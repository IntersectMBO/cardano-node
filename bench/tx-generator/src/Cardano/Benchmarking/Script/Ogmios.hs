{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Cardano.Benchmarking.Script.Ogmios
Description : Submit transactions through an Ogmios endpoint.

This is a functional submission transport, not a benchmarking one: it
submits strictly one transaction per round trip over a single WebSocket
connection, ignores any TPS pacing, and reports no submission metrics
beyond a sent/failed count traced at debug level. The high-level config
compiler therefore only selects it together with @debugMode: true@.

Throughput is bounded by the round-trip time to the endpoint, since at
most one request is in flight at a time. Ogmios itself supports
pipelining many requests per connection, correlated by the JSON-RPC
@id@ — a paced sender/receiver pair with an in-flight window is the
natural next step should this transport ever need to carry
benchmark-grade load. Until then, do not draw throughput conclusions
from runs submitted this way.

Note that only transaction submission goes through Ogmios. Everything
else still talks to the local node directly: protocol-parameter and era
queries as well as protocol startup use the node socket and config
file, so tx-generator keeps requiring local node access even when
submitting through a remote endpoint.

Rejected transactions make the run fail. Streams of chained setup
transactions (genesis import, splitting) abort at the first rejection —
everything after it would be doomed anyway — while the benchmarking
phase's stream of independent transactions is submitted to the end and
the action fails afterwards if anything was rejected. Either way the
process exits non-zero, so exit codes can be trusted in scripts.
-}
module Cardano.Benchmarking.Script.Ogmios
  ( OgmiosResult (..)
  , OnRejection (..)
  , onRejectionFor
  , parseOgmiosResponse
  , parseOgmiosUrl
  , submitAllOgmios
  ) where

import           Cardano.Api (IsShelleyBasedEra, Tx, serialiseToCBOR)

import           Cardano.Benchmarking.LogTypes (BenchTracers (..), TraceBenchTxSubmit (..))
import           Cardano.Benchmarking.Script.Env (ActionM, getBenchTracers, liftTxGenError,
                   traceDebug)
import           Cardano.Benchmarking.Script.Types (Generator (..))
import           Cardano.Benchmarking.Wallet (TxStream)
import           Cardano.Logging (traceWith)
import           Cardano.TxGenerator.Types (TxGenError (..))

import           Prelude

import           Control.Exception (Handler (..), IOException, catches)
import           Control.Monad (unless, when)
import           Data.Aeson (Value (..), object, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.URI (parseURI, uriAuthority, uriPath, uriPort, uriRegName, uriScheme)
import qualified Network.WebSockets as WS
import           System.Timeout (timeout)
import           Text.Read (readMaybe)

import           Streaming

-- | The port Ogmios listens on by default.
defaultOgmiosPort :: Int
defaultOgmiosPort = 1337

-- | Per-request response timeout in microseconds. Generous, because the
-- node may hold a submission back while its mempool is saturated.
responseTimeout :: Int
responseTimeout = 90_000_000

-- | How to proceed when Ogmios rejects a transaction.
data OnRejection
  = AbortOnRejection    -- ^ stop the stream at the first rejection
  | ContinueOnRejection -- ^ submit the whole stream, then fail if anything was rejected
  deriving (Eq, Show)

-- | Setup-phase generators (genesis import, splitting) emit chains of
-- interdependent transactions: once one is rejected, everything after it
-- is doomed, so abort right away. The benchmarking phase's 'NtoM' stream
-- consists of mutually independent transactions, so submit it to the end
-- and let the final tally decide.
onRejectionFor :: Generator -> OnRejection
onRejectionFor generator = case generator of
  NtoM {}        -> ContinueOnRejection
  Take _ g       -> onRejectionFor g
  Cycle g        -> onRejectionFor g
  Sequence gs
    | any ((ContinueOnRejection ==) . onRejectionFor) gs -> ContinueOnRejection
  _              -> AbortOnRejection

submitAllOgmios :: forall era. IsShelleyBasedEra era
  => OnRejection -> String -> TxStream IO era -> ActionM ()
submitAllOgmios onRejection url txStream =
  case parseOgmiosUrl url of
    Left err -> liftTxGenError $ TxGenError err
    Right (host, port, path) -> do
      tracers <- getBenchTracers
      -- per-transaction rejections must reach the trace stream like every
      -- other submission event, not stdout; the loop itself runs in plain
      -- IO under the WebSocket client, so hand it a prebuilt trace action
      let traceSubmitFail :: Int -> Text -> Value -> IO ()
          traceSubmitFail code msg errData =
            traceWith (btTxSubmit_ tracers) $ TraceBenchTxSubError $ Text.concat
              [ "Ogmios submit failed: ", msg
              , " (code ", Text.pack (show code), ")"
              , case errData of
                  Null -> ""
                  dat  -> ", details: "
                            <> Text.decodeUtf8 (LBS.toStrict (Aeson.encode dat))
              ]
      traceDebug $ "Ogmios: connecting to " ++ url
      result <- liftIO $
        WS.runClient host port path (\conn -> submitLoop onRejection traceSubmitFail conn txStream 0 0 0)
          `catches`
            [ Handler $ \(e :: WS.HandshakeException)  -> connectionFailure e
            , Handler $ \(e :: WS.ConnectionException) -> connectionFailure e
            , Handler $ \(e :: IOException)            -> connectionFailure e
            ]
      case result of
        Left err -> liftTxGenError err
        Right (sent, failed) -> do
          traceDebug $ "Ogmios: done, " ++ show sent ++ " sent, " ++ show failed ++ " failed"
          -- a rejected transaction is a functional failure; report it as
          -- such so the run's exit code can be trusted
          when (failed > 0) $ liftTxGenError $ TxGenError $
            "Ogmios: " ++ show failed ++ " of " ++ show (sent + failed)
              ++ " transactions were rejected"
 where
  connectionFailure :: Show e => e -> IO (Either TxGenError (Int, Int))
  connectionFailure e = return $ Left $ TxGenError $ "Ogmios connection failure: " ++ show e

submitLoop :: IsShelleyBasedEra era
  => OnRejection
  -> (Int -> Text -> Value -> IO ())
  -> WS.Connection -> TxStream IO era -> Int -> Int -> Int -> IO (Either TxGenError (Int, Int))
submitLoop onRejection traceSubmitFail conn stream reqId sent failed = do
  step <- Streaming.inspect stream
  case step of
    Left () -> return $ Right (sent, failed)
    Right (Left err :> _rest) -> return $ Left err
    Right (Right tx :> rest) -> do
      WS.sendTextData conn $ Aeson.encode (mkSubmitRequest tx reqId)
      mResp <- timeout responseTimeout $ WS.receiveData conn
      case mResp of
        Nothing -> return $ Left $ TxGenError $
          "Ogmios: no response to request " ++ show reqId
            ++ " within " ++ show (responseTimeout `div` 1_000_000) ++ "s"
        Just resp -> case parseOgmiosResponse resp of
          Left parseErr ->
            return $ Left $ TxGenError $ "Ogmios response parse error: " ++ parseErr
          Right (respId, result)
            -- a mismatched (or null) id is a protocol-level fault, not a
            -- transaction rejection: the connection is out of sync, so stop
            | respId /= Just reqId -> return $ Left $ TxGenError $
                "Ogmios: response id mismatch: expected " ++ show reqId
                  ++ ", got " ++ show respId ++ " (" ++ describe result ++ ")"
            | OgmiosError code errMsg errData <- result -> do
                traceSubmitFail code errMsg errData
                case onRejection of
                  AbortOnRejection -> return $ Left $ TxGenError $
                    "Ogmios: transaction rejected: " ++ Text.unpack errMsg
                      ++ " (code " ++ show code ++ ")"
                  ContinueOnRejection ->
                    submitLoop onRejection traceSubmitFail conn rest (reqId + 1) sent (failed + 1)
            | otherwise ->
                submitLoop onRejection traceSubmitFail conn rest (reqId + 1) (sent + 1) failed
 where
  describe (OgmiosSuccess txId)  = "success, tx " ++ Text.unpack txId
  describe (OgmiosError _ msg _) = "error: " ++ Text.unpack msg

parseOgmiosUrl :: String -> Either String (String, Int, String)
parseOgmiosUrl urlStr = do
  uri <- note ("Invalid Ogmios URL: " ++ urlStr) $ parseURI urlStr
  -- WS.runClient speaks plaintext TCP only, so accepting wss:// (or any
  -- other scheme) here would silently drop the security the URL asks for.
  unless (uriScheme uri == "ws:") $
    Left $ "Unsupported scheme in Ogmios URL (only plain ws:// is supported): " ++ urlStr
  auth <- note ("No authority in Ogmios URL: " ++ urlStr) $ uriAuthority uri
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
  note msg = maybe (Left msg) Right
  parsePort p = case readMaybe p of
    Just n | n >= 1 && n <= 65535 -> Right n
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
          Aeson.withObject "transaction" (\t -> t .: "id") txObj
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
