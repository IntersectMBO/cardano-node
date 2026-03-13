{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Tracer.Handlers.Metrics.TimeseriesServer(runTimeseriesServer) where
import           Cardano.Timeseries.AsText
import           Cardano.Timeseries.Interface (ExecutionError (..))
import           Cardano.Tracer.Acceptors.Utils (getTimeMs)
import           Cardano.Tracer.Configuration (Certificate (..), Endpoint, TracerConfig (..),
                   epForceSSL, setEndpoint)
import           Cardano.Tracer.Handlers.Metrics.Utils (contentHdrUtf8Text)
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Timeseries

import           Control.Monad (guard)
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromMaybe)
import           Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Text.Encoding as T
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp hiding (run)
import           Network.Wai.Handler.WarpTLS
import           System.Time.Extra (sleep)

-- | GET timeseries/query
parseTimeseriesQuery :: Request -> Maybe ()
parseTimeseriesQuery request = do
  guard (request.pathInfo == ["timeseries", "query"])
  guard (request.requestMethod == methodPost)

timeseriesApp :: TimeseriesHandle -> Application
timeseriesApp handle request@(parseTimeseriesQuery -> Just ()) send = do
  bs <- consumeRequestBodyStrict request
  let query = decodeUtf8Lenient (BL.toStrict bs)
  now <- getTimeMs
  execute handle (fromIntegral now) query >>= \case
    Left err -> send $
      responseLBS status contentHdrUtf8Text (BL.fromStrict (T.encodeUtf8 (asText err)))
      where
        status = case err of
          ParsingError _ -> status400
          ElabError _    -> status400
          InterpError _  -> status500
    Right v -> send $ responseLBS status200 contentHdrUtf8Text (BL.fromStrict (T.encodeUtf8 $ showT v))
timeseriesApp _ _ send = send $ responseLBS status404 contentHdrUtf8Text ""

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

    application :: Application
    application = timeseriesApp handle

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
