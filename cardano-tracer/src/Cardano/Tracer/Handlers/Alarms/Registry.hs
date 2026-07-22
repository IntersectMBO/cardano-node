{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The central alarm registry: static consumers (fixed at startup) sitting
-- on top of the append-only store. Mirrors the brainstorm design doc's
-- @AlarmRegistry@ sketch. Like
-- "Cardano.Tracer.Handlers.Alarms.Types"\/"...Store", this module never
-- imports "Cardano.Tracer.Environment".
module Cardano.Tracer.Handlers.Alarms.Registry
  ( AlarmRegistry
  , newAlarmRegistry
  , acceptEvent
  , rejectEvent
  , checkTraceObjectsForAlarms
  , readHistoryFiltered
  , lookupProducerCredential
  , lookupReaderCredential
  ) where

import           Cardano.Logging.Types (TraceObject)
import           Cardano.Tracer.Configuration (AlarmsConfig (..), AlarmsRetentionConfig (..))
import           Cardano.Tracer.Handlers.Alarms.Auth
import           Cardano.Tracer.Handlers.Alarms.Consumers
import           Cardano.Tracer.Handlers.Alarms.Store
import           Cardano.Tracer.Handlers.Alarms.TraceRules
import           Cardano.Tracer.Handlers.Alarms.Types
import           Cardano.Tracer.MetaTrace (TracerTrace (..), Trace, traceWith)

import           Data.Foldable (for_)
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Text (Text)
import           Data.Time.Clock (getCurrentTime)

data AlarmRegistry = AlarmRegistry
  { arStore      :: !AlarmStoreHandle
  , arConsumers  :: ![AlarmConsumer] -- ^ static, fixed at startup
  , arTraceRules :: ![TraceAlarmRule]
  , arAuth       :: !AuthTables
  , arTracer     :: !(Trace IO TracerTrace)
  }

newAlarmRegistry :: Trace IO TracerTrace -> AlarmsConfig -> IO AlarmRegistry
newAlarmRegistry tracer AlarmsConfig{alRetention, alConsumers, alAuthentication, alTraceRules} = do
  store     <- newAlarmStore (fromMaybe emptyRetention alRetention)
  authTable <- loadCredentials alAuthentication
  pure AlarmRegistry
    { arStore      = store
    , arConsumers  = map consumerFromConfig alConsumers
    , arTraceRules = map traceRuleFromConfig (fromMaybe [] alTraceRules)
    , arAuth       = authTable
    , arTracer     = tracer
    }
 where
  emptyRetention = AlarmsRetentionConfig Nothing Nothing

-- | Accept a (possibly duplicate) producer submission. On a genuinely new
--   event: insert into the store, then synchronously dispatch to every
--   static consumer. On a replay of an already-accepted
--   @(source, sourceEventId)@: skip dispatch -- "does not dispatch it
--   twice", per the design doc.
acceptEvent :: AlarmRegistry -> AlarmSource -> IngressRequest -> IO (AlarmEvent, Bool)
acceptEvent registry src req = do
  receivedAt <- getCurrentTime
  (_cursor, ev, wasCreated) <- insertOrGetExisting (arStore registry) src receivedAt req
  if wasCreated
    then do
      traceWith (arTracer registry) TracerAlarmAccepted
        { ttAlarmAcceptedSource   = unAlarmSource src
        , ttAlarmAcceptedRuleId   = unRuleId (ruleId ev)
        , ttAlarmAcceptedSeverity = severity ev
        }
      for_ (arConsumers registry) \consumer -> dispatch (arTracer registry) consumer ev
    else
      traceWith (arTracer registry) TracerAlarmDuplicate
        { ttAlarmDuplicateSource        = unAlarmSource src
        , ttAlarmDuplicateSourceEventId = sourceEventId ev
        }
  pure (ev, wasCreated)

-- | Trace a rejected (invalid, unauthenticated, or oversized) ingress
--   request. Never goes through 'acceptEvent' -- a rejected request never
--   reaches the store.
rejectEvent :: AlarmRegistry -> Text -> IO ()
rejectEvent registry reason =
  traceWith (arTracer registry) TracerAlarmRejected { ttAlarmRejectedReason = reason }

-- | Check every received trace message against the configured trace
--   severity rules and submit each match through the normal 'acceptEvent'
--   path. Within a rule's suppression window, repeated matches share their
--   idempotency key, so 'acceptEvent' reports them as duplicates and
--   dispatches nothing.
checkTraceObjectsForAlarms :: AlarmRegistry -> Text -> [TraceObject] -> IO ()
checkTraceObjectsForAlarms registry nodeName traceObjects =
  for_ (arTraceRules registry) \rule ->
    for_ (mapMaybe (traceRuleRequest rule nodeName) traceObjects) \req ->
      acceptEvent registry traceAlarmSource req

readHistoryFiltered :: AlarmRegistry -> Maybe AlarmCursor -> Int -> AlarmFilter -> IO [(AlarmCursor, AlarmEvent)]
readHistoryFiltered registry = readHistory (arStore registry)

lookupProducerCredential :: AlarmRegistry -> Text -> Maybe ProducerCredential
lookupProducerCredential registry = lookupProducer (arAuth registry)

lookupReaderCredential :: AlarmRegistry -> Text -> Maybe ReaderCredential
lookupReaderCredential registry = lookupReader (arAuth registry)
