{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An in-memory, append-only alarm store. Deliberately not persisted to
-- disk: the design doc (@cardano-tracer/docs/alarm-system-concept.md@)
-- explicitly leaves the storage backend as an open decision, so history is
-- lost across a @cardano-tracer@ restart in this sketch. The concrete
-- backend is meant to sit behind this small handle so a persistent
-- implementation can be swapped in later without changing callers.
module Cardano.Tracer.Handlers.Alarms.Store
  ( AlarmStoreHandle
  , newAlarmStore
  , insertOrGetExisting
  , readHistory
  , pruneOnce
  ) where

import           Cardano.Tracer.Configuration (AlarmsRetentionConfig (..))
import           Cardano.Tracer.Handlers.Alarms.Types
import           Cardano.Tracer.Time (getTimeMs)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, link, race_)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO,
                   stateTVar, writeTVar)
import           Control.Monad (forever)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import           Data.Word (Word64)

data AlarmStoreHandle = AlarmStoreHandle
  { ashEvents    :: !(TVar (Map AlarmCursor AlarmEvent))
  , ashIndex     :: !(TVar (Map (AlarmSource, Text) AlarmCursor)) -- ^ (source, sourceEventId) -> cursor, for idempotency
  , ashNextSeq   :: !(TVar Word64)
  , ashStartTag  :: !Text -- ^ process-start timestamp (ms); prefixes generated eventIds
  , ashRetention :: !AlarmsRetentionConfig
  , ashWake      :: !(MVar ()) -- ^ unused for now (no dynamic reconfiguration in this sketch); kept so the pruner loop matches Cardano.Timeseries.Component.create's shape
  }

-- | Prune every 60 seconds when any retention limit is configured. With no
--   retention there is nothing to prune, ever, and no pruner is started: a
--   thread parked forever on 'ashWake' would be killed by the RTS's deadlock
--   detector once the handle becomes garbage, and 'link' would then rethrow
--   into whatever the creating thread is doing by that time (observed as
--   spurious failures in unrelated tests).
pruneIntervalMicros :: Int
pruneIntervalMicros = 60 * 1000 * 1000

newAlarmStore :: AlarmsRetentionConfig -> IO AlarmStoreHandle
newAlarmStore retention = do
  events   <- newTVarIO Map.empty
  index    <- newTVarIO Map.empty
  nextSeq  <- newTVarIO 0
  wake     <- newEmptyMVar
  startTag <- Text.pack . show <$> getTimeMs
  let handle = AlarmStoreHandle events index nextSeq startTag retention wake
  case (arcMaxAgeSeconds retention, arcMaxEvents retention) of
    (Nothing, Nothing) -> pure ()
    _ -> async (runPruner handle) >>= link
  pure handle
 where
  runPruner :: AlarmStoreHandle -> IO ()
  runPruner handle = forever $ do
    now <- getCurrentTime
    pruneOnce handle now
    race_ (threadDelay pruneIntervalMicros) (takeMVar (ashWake handle))

-- | Insert a new event, or return the already-accepted event for a replayed
--   @(source, sourceEventId)@ pair without dispatching it again. One STM
--   transaction, so two concurrent submissions of the same key can never
--   both "win".
insertOrGetExisting
  :: AlarmStoreHandle
  -> AlarmSource
  -> UTCTime -- ^ receivedAt
  -> IngressRequest
  -> IO (AlarmCursor, AlarmEvent, Bool) -- ^ (cursor, event, was newly created)
insertOrGetExisting handle src receivedAt req =
  atomically do
    idx <- readTVar (ashIndex handle)
    case Map.lookup (src, irSourceEventId req) idx of
      Just cursor -> do
        evs <- readTVar (ashEvents handle)
        case Map.lookup cursor evs of
          Just ev -> pure (cursor, ev, False)
          -- Indexed but pruned already: treat as a fresh submission. This
          -- can only happen if pruning ever removed an event without also
          -- removing its index entry -- 'pruneOnce' takes care to avoid
          -- that, but resolving to a fresh insert here is a safe fallback
          -- either way.
          Nothing -> insertNew
      Nothing -> insertNew
 where
  insertNew = do
    seqNum <- stateTVar (ashNextSeq handle) (\n -> (n, n + 1))
    let cursor = AlarmCursor seqNum
        eid    = ashStartTag handle <> "-" <> Text.pack (show seqNum)
        ev = AlarmEvent
          { schemaVersion = 1
          , eventId       = eid
          , sourceEventId = irSourceEventId req
          , raisedAt      = irRaisedAt req
          , receivedAt    = receivedAt
          , source        = src
          , ruleId        = irRuleId req
          , severity      = irSeverity req
          , summary       = irSummary req
          , scope         = irScope req
          , labels        = irLabels req
          , details       = irDetails req
          }
    modifyTVar' (ashEvents handle) (Map.insert cursor ev)
    modifyTVar' (ashIndex handle) (Map.insert (src, irSourceEventId req) cursor)
    pure (cursor, ev, True)

-- | Cursor-ordered, filtered history read, strictly after the given cursor
--   (exclusive), capped at @limit@ results.
readHistory :: AlarmStoreHandle -> Maybe AlarmCursor -> Int -> AlarmFilter -> IO [(AlarmCursor, AlarmEvent)]
readHistory handle after limit filt = do
  evs <- readTVarIO (ashEvents handle)
  let afterOk cursor = maybe True (cursor >) after
      candidates = [ (c, ev) | (c, ev) <- Map.toAscList evs, afterOk c, matchesFilter filt ev ]
  pure (take (max 0 limit) candidates)

-- | Apply the configured age\/count retention limits. Removing an event from
--   'ashEvents' always removes its @(source, sourceEventId)@ entry from
--   'ashIndex' in the /same/ transaction -- if these two maps were ever
--   allowed to drift, a resubmission of a since-pruned key would return a
--   cursor that no longer resolves in 'readHistory'.
pruneOnce :: AlarmStoreHandle -> UTCTime -> IO ()
pruneOnce handle now = atomically do
  evs <- readTVar (ashEvents handle)
  let kept    = applyCount (applyAge evs)
      dropped = Map.difference evs kept
  writeTVar (ashEvents handle) kept
  modifyTVar' (ashIndex handle) (removeDropped dropped)
 where
  retention = ashRetention handle

  applyAge :: Map AlarmCursor AlarmEvent -> Map AlarmCursor AlarmEvent
  applyAge evs = case arcMaxAgeSeconds retention of
    Nothing         -> evs
    Just maxAgeSecs -> Map.filter (\ev -> diffUTCTime now (receivedAt ev) <= fromIntegral maxAgeSecs) evs

  applyCount :: Map AlarmCursor AlarmEvent -> Map AlarmCursor AlarmEvent
  applyCount evs = case arcMaxEvents retention of
    Nothing    -> evs
    Just maxN  -> Map.fromDistinctDescList (take (fromIntegral maxN) (Map.toDescList evs))

  removeDropped
    :: Map AlarmCursor AlarmEvent
    -> Map (AlarmSource, Text) AlarmCursor
    -> Map (AlarmSource, Text) AlarmCursor
  removeDropped dropped idx = foldr Map.delete idx keysToRemove
   where
    keysToRemove = [ (source ev, sourceEventId ev) | ev <- Map.elems dropped ]
