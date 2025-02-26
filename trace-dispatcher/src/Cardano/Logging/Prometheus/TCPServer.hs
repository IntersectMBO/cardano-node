{-# LANGUAGE LambdaCase #-}

module Cardano.Logging.Prometheus.TCPServer (runPrometheusSimple) where

import           Cardano.Logging.Prometheus.Exposition (renderExpositionFromSample)

import           Control.Concurrent.Async (async)
import           Control.Monad (unless)
import           Control.Monad.Class.MonadAsync (link)
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Int (Int64)
import           Data.List (find)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8Builder)
import           GHC.Conc (labelThread, myThreadId)
import           Network.Run.TCP.Timeout
import           Network.Socket (PortNumber)
import qualified Network.Socket.ByteString as Strict (recv)
import qualified Network.Socket.ByteString.Lazy as Lazy (sendAll)
import           System.Metrics as EKG (Store, sampleAll)
import           System.TimeManager (tickle)


runPrometheusSimple :: EKG.Store -> PortNumber -> IO ()
runPrometheusSimple ekgStore portNo =
  async serveListener >>= link
  where
    getCurrentExposition = renderExpositionFromSample <$> sampleAll ekgStore
    serveListener = do
      myThreadId >>= flip labelThread "PrometheusSimple server"
      runTCPServer 300 (Just "0.0.0.0") (show portNo) (serveAccepted getCurrentExposition)   -- hardcoded: 300 seconds inactivity, and socket resource will be released

-- serves an incoming connection; will terminate upon remote socket close or be reaped by inactivity timeout
serveAccepted :: IO Text -> TimeoutServer ()
serveAccepted getCurrentExposition _ timeoutHandle sock = go
  where
    go = do
      msg <- Strict.recv sock 2048
      unless (BC.null msg) $ do
        response <- buildResponse getCurrentExposition $ pseudoParse msg
        Lazy.sendAll sock $ toLazyByteString $ T.encodeUtf8Builder response
        tickle timeoutHandle
        go


data Method = GET | HEAD | UNSUPPORTED deriving Eq

data Accept = TextLike | OpenMetrics | All | Unsupported deriving Eq

-- "parses" a buffer read via TCP into a minimal viable HTTP request (TM): route, HTTP verb, Accept: header
pseudoParse :: ByteString -> Maybe (ByteString, Method, Accept)
pseudoParse request =
  case BC.lines request of
    requestLine : headers
      | method : route : _ <- BC.words requestLine
        -> Just (route, readMethod method, readAccept headers)
    _ -> Nothing
  where
    readMethod :: ByteString -> Method
    readMethod "GET"  = GET
    readMethod "HEAD" = HEAD
    readMethod _      = UNSUPPORTED

    readAccept :: [ByteString] -> Accept
    readAccept headers =
      case find (\h -> any (`BC.isPrefixOf` h) caseInsensitive) headers of
        Nothing -> All
        Just accept
          | "application/openmetrics-text" `BC.isInfixOf` accept  -> OpenMetrics
          | "text/" `BC.isInfixOf`                        accept  -> TextLike
          | "*/*" `BC.isInfixOf`                          accept  -> All
          | otherwise                                             -> Unsupported
      where
        caseInsensitive = ["Accept:", "accept:", "ACCEPT:"]

-- builds a minimal complete HTTP response based on route, HTTP verb and requested content type
buildResponse :: IO Text -> Maybe (ByteString, Method, Accept) -> IO Text
buildResponse getCurrentExposition = \case
  Nothing -> pure $ responseError errorBadRequest
  Just (route, method, accept)
    | route /= "/metrics"   -> pure $ responseError errorNotFound
    | method == UNSUPPORTED -> pure $ responseError errorBadMethod
    | accept == Unsupported -> pure $ responseError errorBadContent
    | otherwise ->
        let content = if accept == OpenMetrics then hdrContentTypeOpenMetrics else hdrContentTypeText
        in responseMessage (method == GET) content <$> getCurrentExposition

hdrContentType :: [Text] -> Text
hdrContentType values = "Content-Type: " <> T.intercalate ";" values

hdrContentTypeText, hdrContentTypeOpenMetrics :: Text
hdrContentTypeText        = hdrContentType ["text/plain", "charset=utf-8"]
hdrContentTypeOpenMetrics = hdrContentType ["application/openmetrics-text", "version=1.0.0", "charset=utf-8"]

hdrContentLength :: Int64 -> Text
hdrContentLength len = "Content-Length: " <> T.pack (show len)

errorBadRequest, errorNotFound, errorBadMethod, errorBadContent :: (Text, Text)
errorBadRequest = ("400", "Bad Request")
errorNotFound   = ("404", "Not Found")
errorBadMethod  = ("405", "Method Not Allowed")
errorBadContent = ("415", "Unsupported Media Type")

responseError :: (Text, Text) -> Text
responseError (errCode, errMsg) = T.unlines
  [ "HTTP/1.1 " <> errCode
  , hdrContentTypeText
  , hdrContentLength (T.length msg)
  , ""
  , msg
  ]
  where msg = errCode <> " " <> errMsg

responseMessage :: Bool -> Text -> Text -> Text
responseMessage withBody contentType msg = T.unlines $
  [ "HTTP/1.1 200 OK"
  , contentType
  , hdrContentLength (T.length msg)
  , ""
  ] ++ [ msg | withBody ]
