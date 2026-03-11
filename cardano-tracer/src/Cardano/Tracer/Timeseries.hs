{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Cardano.Tracer.Timeseries(TimeseriesConfig(..), TimeseriesHandle, create, reconfigure, insert, execute) where
import           Cardano.Timeseries.Domain.Instant (Instant (..))
import           Cardano.Timeseries.Domain.Types (MetricIdentifier, Timestamp)
import qualified Cardano.Timeseries.Interface as Interface
import qualified Cardano.Timeseries.Interp.Config as Interp (Config (..))
import           Cardano.Timeseries.Interp.Value (Value)
import qualified Cardano.Timeseries.Store as Store
import           Cardano.Timeseries.Store.Tree (Tree)
import           Cardano.Tracer.Types (NodeId (..))

import           Prelude hiding (Foldable (..))

import           Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVar)
import           Control.Monad.STM (atomically)
import           Data.Foldable (Foldable (..))
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Word (Word64)
import Cardano.Timeseries.Interface (ExecutionError)

data TimeseriesConfig = TimeseriesConfig {
  -- | How long the store entries are retained for (ms).
  retentionMillis :: Word64,
  interpCfg :: Interp.Config
}

-- | Not exported. The user gets the default if `create`-d with a `Nothing`
defaultTimeseriesInterpConfig :: Interp.Config
defaultTimeseriesInterpConfig = Interp.Config
  (15 * 1000) -- 15 s

-- | Not exported. The user gets the default if `create`-d with a `Nothing`
defaultTimeseriesConfig :: TimeseriesConfig
defaultTimeseriesConfig = TimeseriesConfig
  (1 * 24 * 60 * 60 * 1000)      -- 1 day
  defaultTimeseriesInterpConfig

-- | The constructor is not exported. Use the API methods below for working with timeseries.
data TimeseriesHandle = TimeseriesHandle {
  cfg :: !(TVar TimeseriesConfig),
  store :: !(TVar (Tree Double))
}

create :: Maybe TimeseriesConfig -> IO TimeseriesHandle
create mbCfg = do
  cfg_ <- newTVarIO (fromMaybe defaultTimeseriesConfig mbCfg)
  st <- newTVarIO Store.new
  pure (TimeseriesHandle cfg_ st)

reconfigure :: TimeseriesHandle -> TimeseriesConfig -> IO ()
reconfigure handle cfg_ = atomically $
  modifyTVar' handle.cfg (const cfg_)

insert :: TimeseriesHandle -> NodeId -> Timestamp -> [(MetricIdentifier, Double)] -> IO ()
insert handle nodeId t batch = atomically $
  modifyTVar' handle.store $ \st -> foldl' f st batch
  where
    f st (k, v) = Store.insert st k (Instant (Set.singleton ("node_id", nodeId.text)) t v)

execute :: TimeseriesHandle -> Timestamp -> Text -> IO (Either ExecutionError Value)
execute handle now stringQuery = do
  (theCfg, theStore) <- atomically $ (,) <$> readTVar handle.cfg <*> readTVar handle.store
  pure $ Interface.execute theStore theCfg.interpCfg now stringQuery
