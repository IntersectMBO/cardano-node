{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Timeseries.Component(
    TimeseriesConfig(..)
  , TimeseriesHandle
  , QueryId
  , create
  , modifyConfig
  , readConfig
  , writeConfig
  , insert
  , execute
  , prune) where
import           Cardano.Logging (Trace, threadLabelMe, traceWith)
import           Cardano.Timeseries.API hiding (execute, insert)
import qualified Cardano.Timeseries.API as API
import           Cardano.Timeseries.Component.Trace
import           Cardano.Timeseries.Component.Types
import           Cardano.Timeseries.Domain.Instant
import qualified Cardano.Timeseries.Interp.Config as Interp
import           Cardano.Timeseries.Util (diag, getTimeMs)

import           Prelude hiding (Foldable (..))

import           Control.Arrow (second)
import           Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import           Control.Concurrent.Async (async, link, race_)
import           Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO,
                   stateTVar)
import           Control.Monad (forever)
import           Control.Monad.STM (atomically)
import           Data.Foldable (Foldable (..))
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Text (Text)

-- | Not exported. The user gets the default if `create`-d with a `Nothing`
defaultTimeseriesInterpConfig :: Interp.Config
defaultTimeseriesInterpConfig = Interp.Config
  (15 * 1000) -- 15 s

-- | Not exported. The user gets the default if `create`-d with a `Nothing`
defaultTimeseriesConfig :: TimeseriesConfig
defaultTimeseriesConfig = TimeseriesConfig
  (1 * 24 * 60 * 60 * 1000)      -- 1 day in ms
  (Just (1 * 60 * 60))           -- 1 hour in s
  defaultTimeseriesInterpConfig

-- | The constructor and projections are not exported. Use the API methods below for working with timeseries.
data TimeseriesHandle where
  TimeseriesHandle :: Store s Double =>
      { config        :: TVar TimeseriesConfig -- ^ Dynamically adjustable configuration.
      , store         :: TVar s -- ^ A timeseries store.
      , reconfigured  :: MVar () -- ^ A synchronisation primitive used for (waiting for)/signaling a reconfiguration.
      , tracer        :: Trace IO TimeseriesTrace -- ^ A tracer supplied on creation of the handle.
      , nextQueryId   :: TVar QueryId -- ^ A mutable integer for assigning a unique serial identifier to every executed query.
     } -> TimeseriesHandle

-- Create an opaque "handle" for a timeseries store, given an optional configuration.
-- Also spawns a "pruner" thread (if specified in the config) that periodically prunes the store of expired entries.
create :: forall s. Store s Double => Trace IO TimeseriesTrace -> Maybe TimeseriesConfig -> IO TimeseriesHandle
create tr mbCfg = do
  cfg <- newTVarIO (fromMaybe defaultTimeseriesConfig mbCfg)
  st <- newTVarIO (new @s)
  qid <- newTVarIO 0
  reconf <- newEmptyMVar
  let handle = TimeseriesHandle cfg st reconf tr qid
  async (runPruner handle) >>= link
  traceWith tr (TimeseriesTraceCreate mbCfg)
  pure handle
  where
    runPruner :: TimeseriesHandle -> IO ()
    runPruner handle = threadLabelMe "timeseries-pruner-thread" >> do
      forever $ do
        cfg <- readTVarIO handle.config
        case cfg.pruningPeriodSec of
          Nothing ->
            -- If the current configuration doesn't specify a pruning period, we block
            -- the thread until a reconfiguration happens.
            takeMVar handle.reconfigured
          Just period -> do
            prune handle
            -- Wait for the given period or wake up on a reconfiguration.
            race_
              (threadDelay (fromIntegral period * 1_000_000))
              (takeMVar handle.reconfigured)

-- | Reconfigure the store. The new parameters are applied immediately.
--   Wakes up the pruner thread as well.
--   If the supplied config is empty, reconfigures back to the default config.
modifyConfig :: TimeseriesHandle -> (TimeseriesConfig -> Maybe TimeseriesConfig) -> IO ()
modifyConfig handle cfg = do
  newCfg <- atomically $ stateTVar handle.config (second (fromMaybe defaultTimeseriesConfig) . diag . cfg)
  putMVar handle.reconfigured ()
  traceWith handle.tracer (TimeseriesTraceReconfigure newCfg)

-- | Read a snapshot of the configuration.
readConfig :: TimeseriesHandle -> IO TimeseriesConfig
readConfig handle = readTVarIO handle.config

-- | A specialised version of `modifyConfig` where the value of the new config doesn't depend on the old one.
writeConfig :: TimeseriesHandle -> Maybe TimeseriesConfig -> IO ()
writeConfig handle k = modifyConfig handle (const k)

-- | Insert a batch on metric data into the store at the given timestamp.
insert :: TimeseriesHandle -> Text -> Text -> Timestamp -> [(MetricIdentifier, Double)] -> IO ()
insert TimeseriesHandle{..} originKey originValue t batch = do
  atomically $ modifyTVar' store $ \st -> foldl' f st batch
  traceWith tracer (TimeseriesTraceInsert originKey originValue t batch)
  where
    f st (k, v) = API.insert st k (Instant (Set.singleton (originKey, originValue)) t v)

-- | Execute a query on the store at the given timestamp (ms).
execute :: TimeseriesHandle -> Timestamp -> Text -> IO (Either ExecutionError Value)
execute TimeseriesHandle{..} at stringQuery = do
  (theCfg, theStore) <- atomically $ (,) <$> readTVar config <*> readTVar store
  queryId <- atomically $ stateTVar nextQueryId (second (+ 1) . diag)
  traceWith tracer (TimeseriesTraceIssueExecute queryId stringQuery)
  let result = API.execute theStore theCfg.interpCfg (fromIntegral at) stringQuery
  traceWith tracer (TimeseriesTraceYieldExecute queryId result)
  pure result

-- | Prune the store of the entries past the configured retention period (relative to "now").
--   The pruner thread uses that function internally, but one can still invoke the pruning manually at will.
prune :: TimeseriesHandle -> IO ()
prune TimeseriesHandle{..} = do
  t <- getTimeMs
  theCfg <- readTVarIO config
  atomically $ modifyTVar' store (\s -> API.truncate s (fromIntegral t - theCfg.retentionMillis))
  traceWith tracer (TimeseriesTracePrune theCfg.retentionMillis)
