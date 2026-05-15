{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Tracer.Handlers.Metrics.TimeseriesServer(runTimeseriesServer) where

import           Cardano.Logging.Types.NodeInfo (NodeInfo (..))
import           Cardano.Logging.Types.NodeStartupInfo (NodeStartupInfo)
import           Cardano.Timeseries.API (ExecutionError (..))
import           Cardano.Timeseries.AsText
import           Cardano.Timeseries.Component
import           Cardano.Timeseries.JSON ()
import           Cardano.Tracer.Configuration (Certificate (..), Endpoint, TracerConfig (..),
                   epForceSSL, setEndpoint)
import           Cardano.Tracer.Environment (TracerEnv (..))
import           Cardano.Tracer.Handlers.Metrics.Utils (contentHdrJSON, contentHdrUtf8Text)
import           Cardano.Tracer.Handlers.Utils (askDataPoint)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Time (getTimeMs)
import           Cardano.Tracer.Types (NodeId (..))
#if RTVIEW
import           Cardano.Tracer.Handlers.RTView.Update.NodeState (NodeStateWrapper (..))
#endif

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (join)
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
import           Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Text.Read (decimal, double)
import           Data.Word (Word64)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp hiding (run)
import           Network.Wai.Handler.WarpTLS
import           System.Time.Extra (sleep)

errorType :: ExecutionError -> Text
errorType ParsingErrorWhileExecuting{} = "parse"
errorType ElabErrorWhileExecuting{}    = "bad_data"
errorType InterpErrorWhileExecuting{}  = "execution"

ok :: Response
ok = responseLBS status200 [] ""

malformed :: Response
malformed = responseLBS status400 contentHdrUtf8Text "Malformed input"

notFound :: Response
notFound = responseLBS status404 [] ""

readTimeunit :: Text -> Maybe (Word64 -> Word64)
readTimeunit key | key == "milliseconds" = Just id
                 | key == "seconds"      = Just (* 1000)
                 | key == "minutes"      = Just (* (60 * 1000))
                 | key == "hours"        = Just (* (60 * 60 * 1000))
                 | key == "days"         = Just (* (24 * 60 * 60 * 1000))
                 | otherwise             = Nothing

readDurationQueryItem :: (Text, Maybe Text) -> Maybe Word64
readDurationQueryItem (readTimeunit -> Just toMillis, Just (decimal -> Right (v, ""))) = Just (toMillis v)
readDurationQueryItem _ = Nothing

oneDurationQueryItem :: Request
                     -> Maybe Word64
oneDurationQueryItem request =
  asum (readDurationQueryItem <$> queryToQueryText request.queryString)

encodeUtf8 :: Text -> BL.ByteString
encodeUtf8 = BL.fromStrict . T.encodeUtf8

data InputSanitationConfig = InputSanitationConfig {
  minimumRetentionMillis :: Word64,
  minimumPruningPeriodMillis :: Word64
}

timeseriesApp :: InputSanitationConfig -> TracerEnv -> TimeseriesHandle -> Application
timeseriesApp inputSanCfg tracerEnv handle request send = do
  let handleQuery params = case join (lookup "query" params) of
        Nothing -> send malformed
        Just q  -> do
          let mbTime = do
                t <- join (lookup "time" params)
                case double t of { Right (v, "") -> Just v; _ -> Nothing }
          at <- maybe getTimeMs (\t -> pure (round (t * 1000))) mbTime
          execute handle (fromIntegral at) q >>= \case
            Left err -> send $ responseLBS status400 contentHdrJSON $ encode $ object
              [ "status"    .= ("error"   :: Text)
              , "errorType" .= errorType err
              , "error"     .= asText err
              ]
            Right v -> send $ responseLBS status200 contentHdrJSON $ encode $ object
              [ "status" .= ("success" :: Text)
              , "data"   .= v
              ]
  case request.pathInfo of
    ["timeseries", "query"]
      | request.requestMethod == methodPost -> do
          bs <- consumeRequestBodyStrict request
          handleQuery (queryToQueryText (parseQuery (BL.toStrict bs)))
      | request.requestMethod == methodGet ->
          handleQuery (queryToQueryText request.queryString)
    ["timeseries", "prune"] | request.requestMethod == methodPost -> do
      prune handle
      send ok
    ["timeseries", "config", "retention"]
      | request.requestMethod == methodPost, Just v <- oneDurationQueryItem request ->
        if v >= inputSanCfg.minimumRetentionMillis then do
          modifyConfig handle (\cfg -> Just cfg{retentionMillis = v})
          send ok
        else
          send malformed
      | request.requestMethod == methodGet -> do
        v <- (.retentionMillis) <$> readConfig handle
        send $ responseLBS status200 contentHdrUtf8Text (encodeUtf8 (showT v))
    ["timeseries", "config", "pruning"]
      | request.requestMethod == methodPost ->
        let v = oneDurationQueryItem request in
        if maybe True (>= inputSanCfg.minimumPruningPeriodMillis) v then do
          modifyConfig handle (\cfg -> Just cfg{pruningPeriodMillis = v})
          send ok
        else
          send malformed
      | request.requestMethod == methodGet -> do
        v <- (.pruningPeriodMillis) <$> readConfig handle
        send $ responseLBS status200 contentHdrUtf8Text (encodeUtf8 (showT v))
    ["timeseries", "nodes"]
      | request.requestMethod == methodGet -> do
          nodes <- readTVarIO tracerEnv.teConnectedNodes
          send $ responseLBS status200 contentHdrJSON $ encode (Set.toList nodes)
    ["timeseries", "node", rawId, "info"]
      | request.requestMethod == methodGet -> do
          let nodeId = NodeId rawId
          mInfo <- askDataPoint tracerEnv.teDPRequestors tracerEnv.teCurrentDPLock nodeId "NodeInfo"
          case (mInfo :: Maybe NodeInfo) of
            Nothing -> send notFound
            Just ni -> do
              now <- getCurrentTime
              let uptimeSecs = round (now `diffUTCTime` ni.niStartTime) :: Int
              send $ responseLBS status200 contentHdrJSON $ encode $
                object [ "niName"            .= ni.niName
                       , "niProtocol"        .= ni.niProtocol
                       , "niVersion"         .= ni.niVersion
                       , "niCommit"          .= ni.niCommit
                       , "niStartTime"       .= ni.niStartTime
                       , "niSystemStartTime" .= ni.niSystemStartTime
                       , "uptimeSeconds"     .= uptimeSecs
                       ]
    ["timeseries", "node", rawId, "startup"]
      | request.requestMethod == methodGet -> do
          let nodeId = NodeId rawId
          mNsi <- askDataPoint tracerEnv.teDPRequestors tracerEnv.teCurrentDPLock nodeId "NodeStartupInfo"
          case (mNsi :: Maybe NodeStartupInfo) of
            Nothing  -> send notFound
            Just nsi -> send $ responseLBS status200 contentHdrJSON (encode nsi)
#if RTVIEW
    ["timeseries", "node", rawId, "state"]
      | request.requestMethod == methodGet -> do
          let nodeId = NodeId rawId
          mState <- askDataPoint tracerEnv.teDPRequestors tracerEnv.teCurrentDPLock nodeId "NodeAddBlock"
          case mState of
            Nothing                     -> send notFound
            Just (NodeStateWrapper pct) -> send $ responseLBS status200 contentHdrJSON $ encode $
              object ["syncProgress" .= pct]
#endif
    _ -> send notFound

runTimeseriesServer :: TracerEnv -> Endpoint -> TimeseriesHandle -> IO ()
runTimeseriesServer tracerEnv endpoint handle = do

  -- Pause to prevent collision between "Listening"-notifications from servers.
  sleep 0.1

  traceWith tracerEnv.teTracer TracerStartedTimeseries
    { ttTimeseriesEndpoint = endpoint
    }

  let
    settings :: Settings
    settings = setEndpoint endpoint defaultSettings

    tls_settings :: Certificate -> TLSSettings
    tls_settings Certificate {..} =
      tlsSettingsChain certificateFile (fromMaybe [] certificateChain) certificateKeyFile

    inputSanCfg = InputSanitationConfig {
        minimumRetentionMillis = truncate (fromMaybe 1000 tracerEnv.teConfig.ekgRequestFreq)
      , minimumPruningPeriodMillis = 1
    }

    application :: Application
    application = timeseriesApp inputSanCfg tracerEnv handle

    run :: IO ()
    run | Just True <- epForceSSL endpoint, Just cert <- tracerEnv.teConfig.tlsCertificate
        = runTLS (tls_settings cert) settings application
        -- Trace, if we expect SSL without getting certificates.
        | Just True <- epForceSSL endpoint
        = do traceWith tracerEnv.teTracer TracerMissingCertificate
               { ttMissingCertificateEndpoint = endpoint }
             runSettings settings application
        | otherwise
        = runSettings settings application
  run
