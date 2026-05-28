{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Benchmarking.Script.Ogmios
  ( submitAllOgmios
  ) where

import           Cardano.Api (IsShelleyBasedEra, Tx, serialiseToCBOR)

import           Cardano.Benchmarking.Script.Env (ActionM, liftTxGenError, traceDebug)
import           Cardano.Benchmarking.Wallet (TxStream)
import           Cardano.TxGenerator.Types (TxGenError (..))

import           Prelude

import           Data.Aeson (Value (..), object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.URI (parseURI, uriAuthority, uriPath, uriPort, uriRegName)
import qualified Network.WebSockets as WS

import           Streaming


submitAllOgmios :: forall era. IsShelleyBasedEra era
  => String -> TxStream IO era -> ActionM ()
submitAllOgmios url txStream =
  case parseOgmiosUrl url of
    Left err -> liftTxGenError $ TxGenError err
    Right (host, port, path) -> do
      traceDebug $ "Ogmios: connecting to " ++ url
      result <- liftIO $ WS.runClient host port path $ \conn ->
        submitLoop conn txStream 0 0 0
      case result of
        Left err -> liftTxGenError err
        Right (sent, failed) ->
          traceDebug $ "Ogmios: done, " ++ show sent ++ " sent, " ++ show failed ++ " failed"

submitLoop :: IsShelleyBasedEra era
  => WS.Connection -> TxStream IO era -> Int -> Int -> Int -> IO (Either TxGenError (Int, Int))
submitLoop conn stream reqId sent failed = do
  step <- Streaming.inspect stream
  case step of
    Left () -> return $ Right (sent, failed)
    Right (Left err :> _rest) -> return $ Left err
    Right (Right tx :> rest) -> do
      let msg = Aeson.encode (mkSubmitRequest tx reqId)
      WS.sendTextData conn msg
      resp <- WS.receiveData conn
      case parseOgmiosResponse resp of
        Left parseErr ->
          return $ Left $ TxGenError $ "Ogmios response parse error: " ++ parseErr
        Right (OgmiosError _code errMsg _) -> do
          putStrLn $ "Ogmios submit failed: " ++ Text.unpack errMsg
          submitLoop conn rest (reqId + 1) sent (failed + 1)
        Right (OgmiosSuccess _txId) ->
          submitLoop conn rest (reqId + 1) (sent + 1) failed


parseOgmiosUrl :: String -> Either String (String, Int, String)
parseOgmiosUrl urlStr =
  case parseURI urlStr of
    Nothing  -> Left $ "Invalid Ogmios URL: " ++ urlStr
    Just uri -> case uriAuthority uri of
      Nothing   -> Left $ "No authority in Ogmios URL: " ++ urlStr
      Just auth ->
        let host = uriRegName auth
            port = case uriPort auth of
              ""    -> 1337
              ':':p -> read p
              p     -> read p
            path = case uriPath uri of
              "" -> "/"
              p  -> p
        in Right (host, port, path)


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

parseOgmiosResponse :: LBS.ByteString -> Either String OgmiosResult
parseOgmiosResponse bs =
  case Aeson.eitherDecode bs of
    Left err -> Left err
    Right val -> Aeson.parseEither parseResult val
 where
  parseResult = Aeson.withObject "OgmiosResponse" $ \obj -> do
    mResult <- obj .:? "result"
    case mResult of
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
          return $ OgmiosError code msg (maybe Null id dat)
          ) errVal
