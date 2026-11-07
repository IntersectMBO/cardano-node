{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- 'Consumer' is a sum type whose two constructors define disjoint record fields
-- (cl* for 'ConsumerLog', cw* for 'ConsumerWebhook'). Every use is via
-- 'RecordWildCards'\/'NamedFieldPuns' pattern binding, never as a selector.
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Statically configured alarm dispatch targets. Two variants:
--
--   * 'ConsumerLog' — synchronous meta-trace with the alarm payload.
--   * 'ConsumerWebhook' — enqueues onto a bounded 'TBQueue'; a worker
--     thread POSTs each event to the configured URL with exponential
--     backoff on transient failures (5xx and network errors) and
--     drop-on-permanent-failure (4xx other than 429). Never blocks the
--     caller: when the queue is full, the submission is dropped and
--     logged to stderr, matching the concept doc's requirement that a
--     slow destination must not block ingestion.
module Cardano.Tracer.Handlers.Alarms.Consumers
  ( AlarmConsumer (..)
  , consumerFromConfig
  , dispatch
  ) where

import           Cardano.Tracer.Configuration (AlarmsConsumerConfig (..))
import           Cardano.Tracer.Handlers.Alarms.Types
import           Cardano.Tracer.MetaTrace (TracerTrace (..), Trace, traceWith)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async, async, link)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, newTBQueueIO, readTBQueue,
                   writeTBQueue)
import           Control.Exception (SomeException, try)
import           Control.Monad (forever)
import           Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import           Network.HTTP.Client (HttpException, Manager, RequestBody (..),
                   defaultManagerSettings, httpLbs, method, newManager, parseRequest, requestBody,
                   requestHeaders, responseStatus)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types (Status (..))
import           System.IO (stderr)

data AlarmConsumer
  = ConsumerLog
      { clName    :: !Text
      , clEnabled :: !Bool
      , clFilter  :: !AlarmFilter
      }
  | ConsumerWebhook
      { cwName    :: !Text
      , cwEnabled :: !Bool
      , cwFilter  :: !AlarmFilter
      , cwQueue   :: !(TBQueue AlarmEvent)
      , cwWorker  :: !(Async ())
      }

-- | Translate a config entry into a runtime consumer. For @webhook@,
--   allocates the bounded queue, spawns the delivery worker, and
--   'link's it to the caller so a worker crash surfaces.
consumerFromConfig :: AlarmsConsumerConfig -> IO AlarmConsumer
consumerFromConfig = \case
  AlarmsConsumerLog{aclName, aclEnabled, aclFilter} -> pure ConsumerLog
    { clName    = aclName
    , clEnabled = aclEnabled
    , clFilter  = maybe emptyAlarmFilter filterFromConfig aclFilter
    }
  AlarmsConsumerWebhook{acwName, acwEnabled, acwFilter, acwUrl, acwBearerToken, acwQueueCap, acwMaxBackoff} -> do
    let cap        = fromIntegral (max 1 (fromMaybe 1000 acwQueueCap))
        maxBackoff = max 1 (fromMaybe 60 acwMaxBackoff)
    queue   <- newTBQueueIO cap
    manager <- if "https://" `Text.isPrefixOf` acwUrl
                 then newManager tlsManagerSettings
                 else newManager defaultManagerSettings
    worker  <- async (runWebhookWorker acwName acwUrl acwBearerToken maxBackoff manager queue)
    link worker
    pure ConsumerWebhook
      { cwName    = acwName
      , cwEnabled = acwEnabled
      , cwFilter  = maybe emptyAlarmFilter filterFromConfig acwFilter
      , cwQueue   = queue
      , cwWorker  = worker
      }

dispatch :: Trace IO TracerTrace -> AlarmConsumer -> AlarmEvent -> IO ()
dispatch tracer consumer ev = case consumer of
  ConsumerLog{clName, clEnabled, clFilter}
    | clEnabled && matchesFilter clFilter ev -> traced clName
    | otherwise                              -> pure ()
  ConsumerWebhook{cwName, cwEnabled, cwFilter, cwQueue}
    | cwEnabled && matchesFilter cwFilter ev -> do
        enqueued <- atomically $ do
          full <- isFullTBQueue cwQueue
          if full then pure False
                  else writeTBQueue cwQueue ev >> pure True
        if enqueued
          then traced cwName
          else logLocal
                 ("webhook queue full [" <> cwName <> "]; dropped eventId "
                                          <> eventId ev)
    | otherwise -> pure ()
 where
  traced :: Text -> IO ()
  traced consumerName =
    traceWith tracer TracerAlarmDispatched
      { ttAlarmDispatchedConsumer = consumerName
      , ttAlarmDispatchedSource   = unAlarmSource (source ev)
      , ttAlarmDispatchedRuleId   = unRuleId (ruleId ev)
      , ttAlarmDispatchedSeverity = severity ev
      , ttAlarmDispatchedSummary  = summary ev
      }

--------------------------------------------------------------------------------
-- Webhook worker
--------------------------------------------------------------------------------

runWebhookWorker :: Text -> Text -> Maybe Text -> Int -> Manager -> TBQueue AlarmEvent -> IO ()
runWebhookWorker name url mBearer maxBackoff manager queue = forever $ do
  ev <- atomically (readTBQueue queue)
  deliverWithRetry name url mBearer maxBackoff manager ev 1

deliverWithRetry :: Text -> Text -> Maybe Text -> Int -> Manager -> AlarmEvent -> Int -> IO ()
deliverWithRetry name url mBearer maxBackoff manager ev backoff = do
  outcome <- attempt url mBearer manager ev
  case outcome of
    Delivered -> pure ()
    PermanentFailure reason -> logLocal
      ("webhook [" <> name <> "] dropped " <> eventId ev
                    <> " (permanent: " <> reason <> ")")
    Transient reason -> do
      logLocal ("webhook [" <> name <> "] retrying " <> eventId ev
                            <> " in " <> tshow backoff <> "s (" <> reason <> ")")
      threadDelay (backoff * 1_000_000)
      deliverWithRetry name url mBearer maxBackoff manager ev (min maxBackoff (backoff * 2))

data Outcome = Delivered | PermanentFailure !Text | Transient !Text

attempt :: Text -> Maybe Text -> Manager -> AlarmEvent -> IO Outcome
attempt url mBearer manager ev = do
  reqR <- try @SomeException (parseRequest (Text.unpack url))
  case reqR of
    Left e -> pure (PermanentFailure ("cannot parse URL: " <> tshow e))
    Right base -> do
      let req = base
            { method         = "POST"
            , requestBody    = RequestBodyLBS (encode ev)
            , requestHeaders = ("Content-Type", "application/json") : bearerHdr mBearer
            }
      respR <- try @HttpException (httpLbs req manager)
      case respR of
        Left  e    -> pure (Transient ("network error: " <> tshow e))
        Right resp -> pure (classify (statusCode (responseStatus resp)))
 where
  bearerHdr Nothing  = []
  bearerHdr (Just t) = [("Authorization", "Bearer " <> BSC.pack (Text.unpack t))]

classify :: Int -> Outcome
classify code
  | code == 200 || code == 201 = Delivered
  | code == 429                = Transient "429 rate-limited"
  | code >= 500 && code < 600  = Transient ("server " <> tshow code)
  | code >= 400 && code < 500  = PermanentFailure ("client " <> tshow code)
  | otherwise                  = Transient ("unexpected " <> tshow code)

--------------------------------------------------------------------------------
-- Local logging (never re-enters the alarm pipeline)
--------------------------------------------------------------------------------

logLocal :: Text -> IO ()
logLocal = TIO.hPutStrLn stderr

tshow :: Show a => a -> Text
tshow = Text.pack . show
