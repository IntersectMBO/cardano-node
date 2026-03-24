{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Tracer.Handlers.Metrics.TimeseriesServer(runTimeseriesServer) where
import           Cardano.Timeseries.AsText
import           Cardano.Timeseries.Component
import           Cardano.Tracer.Configuration (Certificate (..), Endpoint, TracerConfig (..),
                   epForceSSL, setEndpoint)
import           Cardano.Tracer.Handlers.Metrics.Utils (contentHdrUtf8Text)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Time (getTimeMs)

import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Text.Encoding as T
import           Data.Text.Read (decimal)
import           Data.Word (Word64)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp hiding (run)
import           Network.Wai.Handler.WarpTLS
import           System.Time.Extra (sleep)

ok :: Response
ok = responseLBS status200 [] ""

malformed :: Response
malformed = responseLBS status400 contentHdrUtf8Text "Malformed input"

notFound :: Response
notFound = responseLBS status404 [] ""

expectEmptyQuery :: Request
                -> (Response -> IO ResponseReceived)
                -> IO ResponseReceived
                -> IO ResponseReceived
expectEmptyQuery request send kont =
  case queryToQueryText request.queryString of
    [] -> kont
    _ -> send malformed

readTimeunit :: Text -> Maybe (Word64 -> Word64)
readTimeunit key | key == "milliseconds" = Just id
                 | key == "seconds"      = Just (* 1000)
                 | key == "minutes"      = Just (* (60 * 1000))
                 | key == "hours"        = Just (* (60 * 60 * 1000))
                 | key == "days"         = Just (* (24 * 60 * 60 * 1000))
                 | otherwise             = Nothing


oneOptionalDurationItemQuery :: Request
                             -> Maybe (Maybe Word64)
oneOptionalDurationItemQuery request =
  case queryToQueryText request.queryString of
    [(readTimeunit -> Just toMillis, Just (decimal -> Right (v, "")))] -> do
      Just (Just (toMillis v))
    [] -> Just Nothing
    _ -> Nothing

oneDurationItemQuery :: Request
                     -> Maybe Word64
oneDurationItemQuery request =
  case queryToQueryText request.queryString of
    [(readTimeunit -> Just toMillis, Just (decimal -> Right (v, "")))] -> Just (toMillis v)
    _ -> Nothing

encodeUtf8 :: Text -> BL.ByteString
encodeUtf8 = BL.fromStrict . T.encodeUtf8

data InputSanitationConfig = InputSanitationConfig {
  minimumRetentionMillis :: Word64,
  minimumPruningPeriodMillis :: Word64
}

timeseriesApp :: InputSanitationConfig -> TimeseriesHandle -> Application
timeseriesApp inputSanCfg handle request send = case request.pathInfo of
  ["timeseries", "query"] | request.requestMethod == methodPost ->
    expectEmptyQuery request send $ do
      bs <- consumeRequestBodyStrict request
      let query = decodeUtf8Lenient (BL.toStrict bs)
      at <- getTimeMs
      execute handle (fromIntegral at) query >>= \case
        Left err -> send $
          responseLBS status400 contentHdrUtf8Text (encodeUtf8 (asText err))
        Right v -> send $ responseLBS status200 contentHdrUtf8Text (encodeUtf8 (showT v))
  ["timeseries", "prune"] | request.requestMethod == methodPost ->
    expectEmptyQuery request send $ do
      prune handle
      send ok
  ["timeseries", "config", "retention"] | request.requestMethod == methodPost, Just v <- oneDurationItemQuery request ->
    if v >= inputSanCfg.minimumRetentionMillis then do
      modifyConfig handle (\cfg -> Just cfg{retentionMillis = v})
      send ok
    else
      send malformed
  ["timeseries", "config", "pruning"] | request.requestMethod == methodPost, Just v <- oneOptionalDurationItemQuery request ->
    if maybe True (>= inputSanCfg.minimumPruningPeriodMillis) v then do
      modifyConfig handle (\cfg -> Just cfg{pruningPeriodMillis = v})
      send ok
    else
      send malformed
  ["timeseries", "config", "retention"] | request.requestMethod == methodGet ->
    expectEmptyQuery request send $ do
      v <- (.retentionMillis) <$> readConfig handle
      send $ responseLBS status200 contentHdrUtf8Text (encodeUtf8 (showT v))
  ["timeseries", "config", "pruning"] | request.requestMethod == methodGet ->
    expectEmptyQuery request send $ do
      v <- (.pruningPeriodMillis) <$> readConfig handle
      send $ responseLBS status200 contentHdrUtf8Text (encodeUtf8 (showT v))
  _ -> send notFound

runTimeseriesServer :: Trace IO TracerTrace -> TracerConfig -> Endpoint -> TimeseriesHandle -> IO ()
runTimeseriesServer tr tracerConfig endpoint handle = do

  -- Pause to prevent collision between "Listening"-notifications from servers.
  sleep 0.1

  traceWith tr TracerStartedTimeseries
    { ttTimeseriesEndpoint = endpoint
    }


  let
    settings :: Settings
    settings = setEndpoint endpoint defaultSettings

    tls_settings :: Certificate -> TLSSettings
    tls_settings Certificate {..} =
      tlsSettingsChain certificateFile (fromMaybe [] certificateChain) certificateKeyFile

    inputSanCfg = InputSanitationConfig {
        minimumRetentionMillis = truncate (fromMaybe 1000 tracerConfig.ekgRequestFreq)
      , minimumPruningPeriodMillis = 1
    }

    application :: Application
    application = timeseriesApp inputSanCfg handle

    run :: IO ()
    run | Just True <- epForceSSL endpoint , Just cert <- tlsCertificate tracerConfig
        = runTLS (tls_settings cert) settings application
        -- Trace, if we expect SSL without getting certificates.
        | Just True <- epForceSSL endpoint
        = do traceWith tr TracerMissingCertificate
               { ttMissingCertificateEndpoint = endpoint }
             runSettings settings application
        | otherwise
        = runSettings settings application
  run
