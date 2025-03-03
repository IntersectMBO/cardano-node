-- | Run a simple Prometheus TCP server, responding *only* to the '/metrics' URL with current Node metrics
module Cardano.Logging.Prometheus.TCPServer (runPrometheusSimple) where

import           Cardano.Logging.Prometheus.Exposition (renderExpositionFromSample)
import           Cardano.Logging.Prometheus.NetworkRun

import           Control.Concurrent.Async (async)
import           Control.Monad (when)
import           Control.Monad.Class.MonadAsync (link)
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BC
import           Data.Int (Int64)
import           Data.List (find, intersperse)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T (encodeUtf8Builder)
import           Network.HTTP.Date (epochTimeToHTTPDate, formatHTTPDate)
import           Network.Socket (HostName, PortNumber)
import qualified Network.Socket.ByteString as Strict (recv)
import qualified Network.Socket.ByteString.Lazy as Lazy (sendAll)
import           System.Metrics as EKG (Store, sampleAll)
import           System.Posix.Types (EpochTime)
import           System.PosixCompat.Time (epochTime)


runPrometheusSimple :: EKG.Store -> Maybe HostName -> PortNumber -> IO ()
runPrometheusSimple ekgStore mHost portNo =
  async serveListener >>= link
  where
    getCurrentExposition = renderExpositionFromSample <$> sampleAll ekgStore
    serveListener =
      runTCPServer (defaultRunParams "PrometheusSimple") mHost portNo (serveAccepted getCurrentExposition)

-- serves an incoming connection; will release socket upon remote close, inactivity timeout or runRecvMaxSize bytes received
serveAccepted :: IO Text -> TimeoutServer ()
serveAccepted getCurrentExposition NetworkRunParams{runRecvMaxSize} resetTimeout sock = go
  where
    go = do
      msg <- Strict.recv sock runRecvMaxSize
      let len = BC.length msg
      when (0 < len && len < runRecvMaxSize) $ do
        response <- buildResponse getCurrentExposition $ pseudoParse msg
        Lazy.sendAll sock $ toLazyByteString response
        resetTimeout
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
buildResponse :: IO Text -> Maybe (ByteString, Method, Accept) -> IO Builder
buildResponse getCurrentExposition = \case
  Nothing -> pure $ responseError False errorBadRequest
  Just (route, method, accept)
    | route /= "/metrics"   -> pure $ responseError withBody errorNotFound
    | method == UNSUPPORTED -> pure $ responseError withBody errorBadMethod
    | accept == Unsupported -> pure $ responseError withBody errorBadContent
    | otherwise ->
        let content = if accept == OpenMetrics then hdrContentTypeOpenMetrics else hdrContentTypeText
        in responseMessage withBody content <$> getCurrentExposition <*> epochTime
    where withBody = method == GET

hdrContentType :: [ByteString] -> Builder
hdrContentType = mconcat . ("Content-Type: " :) . intersperse (char8 ';') . map byteString

hdrContentTypeText, hdrContentTypeOpenMetrics :: Builder
hdrContentTypeText        = hdrContentType ["text/plain", "charset=utf-8"]
hdrContentTypeOpenMetrics = hdrContentType ["application/openmetrics-text", "version=1.0.0", "charset=utf-8"]

hdrContentLength :: Int64 -> Builder
hdrContentLength len = "Content-Length: " <> int64Dec len

errorBadRequest, errorNotFound, errorBadMethod, errorBadContent :: (ByteString, ByteString)
errorBadRequest = ("400", "Bad Request")
errorNotFound   = ("404", "Not Found")
errorBadMethod  = ("405", "Method Not Allowed")
errorBadContent = ("415", "Unsupported Media Type")

-- HTTP header line break
nl :: Builder
nl = char8 '\r' <> char8 '\n'

responseError :: Bool -> (ByteString, ByteString) -> Builder
responseError withBody (errCode, errMsg) =
  mconcat $ intersperse nl $
    "HTTP/1.1 " <> byteString errCode :
    if withBody
      then  [ hdrContentLength (fromIntegral $ BC.length msg)
            , hdrContentTypeText
            , ""
            , byteString msg
            ]
      else  [ hdrContentLength 0
            , nl
            ]
  where
    msg = errCode <> " " <> errMsg

responseMessage :: Bool -> Builder -> Text -> EpochTime -> Builder
responseMessage withBody contentType msg now =
  mconcat $ intersperse nl
    [ "HTTP/1.1 200 OK"
    , hdrContentLength (T.length msg)
    , contentType
    , "Date: " <> byteString httpDate
    , ""
    , if withBody then T.encodeUtf8Builder msg else ""
    ]
    where
      httpDate = formatHTTPDate $ epochTimeToHTTPDate now
