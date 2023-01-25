{-# OPTIONS_GHC -Wno-unused-matches #-}
{-#LANGUAGE OverloadedStrings#-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.CLI.Shelley.Run.Rpc
  ( runRpcCmd
  ) where
import Control.Monad.IO.Class
import Cardano.CLI.Shelley.Parsers (RpcCommand(..))
import Prelude
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Network.HTTP.Client.Conduit (parseRequest, RequestBody (RequestBodyLBS))
import Network.HTTP.Simple (httpLBS, setRequestBody, addRequestHeader, setRequestMethod, getResponseStatusCode, getResponseBody, setRequestBasicAuth)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Data.List (foldl')
import qualified Data.Text.Encoding as T
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Conduit (Response(..))
import Cardano.Api (FromJSON)
import Data.Aeson ((.:), (.:?))
import Cardano.Api.Shelley (prettyPrintJSON)
import qualified Data.ByteString.Char8 as BS8
import Data.Aeson.Encode.Pretty (encodePretty)


data RpcId = RpcIdNumber Integer
            | RpcIdString T.Text
            | RpcIdNull

instance FromJSON RpcId where
  parseJSON (A.Number  n) =  pure $ RpcIdNumber $ round n
  parseJSON (A.String t) = pure $ RpcIdString t
  parseJSON A.Null     = pure RpcIdNull
  parseJSON  _           = fail "Not integer or string or null"

data RpcError = RpcError {
    rpcErrCode    :: Integer
  , rpcErrMessage :: T.Text
}
instance FromJSON RpcError where
  parseJSON (A.Object obj) =  RpcError <$> obj .: "code" <*> obj .: "message"
  parseJSON _              =  fail "RpcError is not object"

data RpcResponse  = RpcErrorResponse{
        rpcVersion :: String
      , rpcId :: RpcId
      , rpcError :: RpcError
    } |
    RpcSuccessResponse {
          rpcVersion :: String
      , rpcId :: RpcId
      , rpcResult :: A.Value
    }

instance FromJSON RpcResponse where
  parseJSON (A.Object obj) =  do
    errObj <- obj .:? "error"
    case errObj of
      Just err -> RpcErrorResponse
                  <$> obj .: "jsonrpc"
                  <*> obj .: "id"
                  <*> pure err
      Nothing -> RpcSuccessResponse
                  <$> obj .: "jsonrpc"
                  <*> obj .: "id"
                  <*> obj .: "result"

  parseJSON _              =  fail "RpcError is not object"

runRpcCmd :: MonadIO m => RpcCommand -> m ()
runRpcCmd (RpcCommand url auth headers method params) =do
  liftIO $ do
    baseReq<-parseRequest (T.unpack url)
    let req = setRequestMethod "POST" 
                $ flip (foldr (\(k,v) accum->addRequestHeader (CI.mk k) v accum)) headerList
                $ addRequestHeader "content-type" "application/json" baseReq
    withBasicAuth <- addAuthIfPresent auth req
    response <- httpLBS (setRequestBody (RequestBodyLBS  makeBody) withBasicAuth)
    let status = responseStatus response
    case getResponseStatusCode response of
      200 -> do
        let result=  A.eitherDecode (getResponseBody response)
        case result of
          Left s -> fail $ "Invalid response :"  ++ s ++ "\n" ++ BSL8.unpack  (getResponseBody response)
          Right responseData -> case responseData of
            RpcErrorResponse _ _ (RpcError code msg) ->   fail (T.unpack msg)
            RpcSuccessResponse _ _ resultData ->  BSL8.putStrLn $  encodePretty resultData
      _ -> do
        putStrLn $  "Rpc Error:" ++ show status
        BSL8.putStrLn $ responseBody response
  where
    addAuthIfPresent mAuth req = do 
      case mAuth of
        Nothing -> pure req
        Just txt -> case T.split  (== ':') txt of 
            [k,v] ->  pure $ setRequestBasicAuth  (T.encodeUtf8 k) (T.encodeUtf8 v) req
            _     -> fail $ "Invalid BasicAuth value" ++ T.unpack txt
    headerList = map (\v -> case T.break (== ':') v of { (key, val) -> ( T.encodeUtf8 key, T.encodeUtf8 val) } ) headers
    makeBody = BSL.concat  ["{\"jsonrpc\":\"2.0\",\"method\":", A.encode method,",\"params\":",  A.encode params , ",\"id\":1}"]
