{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The alarm HTTP surface: producer ingress and history. A hand-rolled
-- 'wai' 'Application', modeled directly on
-- "Cardano.Tracer.Handlers.Metrics.TimeseriesServer" (no @servant@
-- anywhere in this codebase).
module Cardano.Tracer.Handlers.Alarms.Server
  ( runAlarms
  ) where

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Environment (TracerEnv (..))
import           Cardano.Tracer.Handlers.Alarms.Auth
import           Cardano.Tracer.Handlers.Alarms.Registry
import           Cardano.Tracer.Handlers.Alarms.Types
import           Cardano.Tracer.Handlers.Metrics.Utils (contentHdrJSON)
import           Cardano.Tracer.MetaTrace

import           Control.Monad (join)
import           Data.Aeson (encode, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (for_)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Read (decimal)
import           Data.Word (Word64)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp hiding (run)
import           Network.Wai.Handler.WarpTLS
import           System.Time.Extra (sleep)

--------------------------------------------------------------------------------
-- Response helpers
--------------------------------------------------------------------------------

jsonResponse :: Status -> Aeson.Value -> Response
jsonResponse st v = responseLBS st contentHdrJSON (encode v)

okResponse, createdResponse :: Aeson.Value -> Response
okResponse      = jsonResponse status200
createdResponse = jsonResponse status201

badRequest :: Text -> Response
badRequest msg = jsonResponse status400 (object ["error" .= msg])

unauthorized :: Response
unauthorized = jsonResponse status401 (object ["error" .= ("missing or invalid bearer token" :: Text)])

forbidden :: Text -> Response
forbidden msg = jsonResponse status403 (object ["error" .= msg])

notFound :: Response
notFound = responseLBS status404 [] ""

--------------------------------------------------------------------------------
-- Config helpers
--------------------------------------------------------------------------------

maxEventBytes :: AlarmsConfig -> Word64
maxEventBytes cfg = fromMaybe 262144 (alLimits cfg >>= alcMaxEventBytes) -- 256 KiB default, matching the design doc's example config

--------------------------------------------------------------------------------
-- Query-string / header helpers
--------------------------------------------------------------------------------

lookupParam :: Text -> [(Text, Maybe Text)] -> Maybe Text
lookupParam key params = join (lookup key params)

parseCursorText :: Text -> Maybe AlarmCursor
parseCursorText t = case decimal t of
  Right (n, "") -> Just (AlarmCursor n)
  _             -> Nothing

parseIntText :: Text -> Maybe Int
parseIntText t = case decimal t of
  Right (n, "") -> Just n
  _             -> Nothing

cursorToText :: AlarmCursor -> Text
cursorToText (AlarmCursor n) = Text.pack (show n)

requestedFilterFromQuery :: [(Text, Maybe Text)] -> AlarmFilter
requestedFilterFromQuery params = AlarmFilter
  { afSource      = AlarmSource <$> lookupParam "source" params
  , afRuleId      = RuleId <$> lookupParam "ruleId" params
  , afMinSeverity = lookupParam "minSeverity" params >>= parseAlarmSeverityText
  -- Scope/label query-string filtering isn't implemented in this sketch;
  -- only source/ruleId/minSeverity are selectable via GET requests.
  , afScope       = mempty
  , afLabels      = mempty
  }

--------------------------------------------------------------------------------
-- Body-size-limited read (checked before decoding, per the design doc's
-- ingress limits -- never fully materialise an attacker-controlled oversized
-- payload)
--------------------------------------------------------------------------------

readLimitedBody :: Word64 -> Request -> IO (Maybe BL.ByteString)
readLimitedBody limit req = case requestBodyLength req of
  KnownLength n | n > limit -> pure Nothing
  _                         -> go 0 mempty
 where
  go total acc = do
    chunk <- getRequestBodyChunk req
    let total' = total + fromIntegral (BS.length chunk)
    if BS.null chunk
      then pure (Just (BB.toLazyByteString acc))
      else if total' > limit
             then pure Nothing
             else go total' (acc <> BB.byteString chunk)

--------------------------------------------------------------------------------
-- Routes
--------------------------------------------------------------------------------

alarmsApp :: AlarmsConfig -> AlarmRegistry -> Application
alarmsApp cfg registry request send = case (requestMethod request, pathInfo request) of
  ("POST", ["alarms", "v1", "events"]) -> handleIngress cfg registry request send
  ("GET",  ["alarms", "v1", "events"]) -> handleHistory registry request send
  _                                    -> send notFound

handleIngress :: AlarmsConfig -> AlarmRegistry -> Application
handleIngress cfg registry request send =
  case bearerToken request >>= lookupProducerCredential registry of
    Nothing -> send unauthorized
    Just (ProducerCredential src) -> do
      readLimitedBody (maxEventBytes cfg) request >>= \case
        Nothing -> do
          rejectEvent registry "request body missing or exceeds the configured size limit"
          send (badRequest "request body missing or exceeds the configured size limit")
        Just bs -> case Aeson.eitherDecode bs of
          Left err -> do
            rejectEvent registry (Text.pack ("invalid alarm event: " <> err))
            send (badRequest (Text.pack ("invalid alarm event: " <> err)))
          Right ingressReq -> do
            (ev, wasCreated) <- acceptEvent registry src ingressReq
            send $ (if wasCreated then createdResponse else okResponse) $ object
              [ "eventId"    .= eventId ev
              , "receivedAt" .= receivedAt ev
              , "created"    .= wasCreated
              ]

handleHistory :: AlarmRegistry -> Application
handleHistory registry request send =
  case bearerToken request >>= lookupReaderCredential registry of
    Nothing -> send unauthorized
    Just credential
      | not (rcrAllowHistory credential) -> send (forbidden "history access is not permitted for this credential")
      | otherwise ->
          let params = queryToQueryText (queryString request)
              requested = requestedFilterFromQuery params
              after = lookupParam "after" params >>= parseCursorText
              limit = fromMaybe 100 (lookupParam "limit" params >>= parseIntText)
          in if not (filterNarrows (rcrFilter credential) requested)
               then send (forbidden "requested filter is not permitted by this credential")
               else do
                 events <- readHistoryFiltered registry after limit requested
                 traceHistoryRead registry (rcrName credential) (length events)
                 send $ okResponse $ object
                   [ "events"     .= map (Aeson.toJSON . snd) events
                   , "nextCursor" .= case events of
                       [] -> Nothing
                       _  -> Just (cursorToText (fst (last events)))
                   ]

--------------------------------------------------------------------------------
-- Server startup
--------------------------------------------------------------------------------

runAlarms :: TracerEnv -> IO ()
runAlarms tracerEnv =
  for_ ((,) <$> alarms (teConfig tracerEnv) <*> teAlarmRegistry tracerEnv) \(cfg, registry) ->
    runAlarmsServer tracerEnv cfg registry

-- | Unlike the existing Prometheus\/EKG\/Timeseries servers -- which fall
--   back to plaintext with just a warning trace if TLS was requested but no
--   certificate is configured -- the alarm server refuses to start in that
--   situation unless 'alAllowInsecure' is explicitly set. Alarm requests
--   carry bearer tokens in the clear otherwise, and the design doc's
--   Security section calls for refusing an externally reachable clear-text
--   endpoint by default.
runAlarmsServer :: TracerEnv -> AlarmsConfig -> AlarmRegistry -> IO ()
runAlarmsServer tracerEnv cfg registry = do
  -- Pause to prevent collision between "Listening"-notifications from servers.
  sleep 0.3
  case (wantsSSL, tlsCertificate (teConfig tracerEnv)) of
    (True, Nothing) | not insecureOk ->
      traceWith (teTracer tracerEnv) TracerAlarmRejected
        { ttAlarmRejectedReason =
            "alarms endpoint requested TLS but no certificate is configured; " <>
            "refusing to start (set alAllowInsecure: true to override)"
        }
    (True, Nothing) -> do
      traceWith (teTracer tracerEnv) TracerMissingCertificate { ttMissingCertificateEndpoint = endpoint }
      startPlain
    (True, Just cert) -> do
      traceWith (teTracer tracerEnv) TracerStartedAlarms { ttAlarmsEndpoint = endpoint }
      runTLS (tlsSettingsFor cert) settings application
    (False, _) -> do
      traceWith (teTracer tracerEnv) TracerStartedAlarms { ttAlarmsEndpoint = endpoint }
      startPlain
 where
  endpoint   = alEndpoint cfg
  insecureOk = fromMaybe False (alAllowInsecure cfg)
  wantsSSL   = epForceSSL endpoint == Just True

  settings    = setEndpoint endpoint defaultSettings
  application = alarmsApp cfg registry

  startPlain = runSettings settings application

  tlsSettingsFor Certificate{..} =
    tlsSettingsChain certificateFile (fromMaybe [] certificateChain) certificateKeyFile
