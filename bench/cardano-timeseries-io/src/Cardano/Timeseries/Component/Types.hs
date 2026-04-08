{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Timeseries.Component.Types(QueryId, TimeseriesConfig(..)) where
import qualified Cardano.Timeseries.Interp.Config as Interp

import           Data.Aeson (ToJSON)
import           Data.Word (Word64)
import           GHC.Generics

type QueryId = Word64

data TimeseriesConfig = TimeseriesConfig {
    -- | How long the store entries are retained for (ms).
    retentionMillis :: Word64
    -- | How often the pruner thread shall prune the store (ms), if enabled.
  , pruningPeriodMillis :: Maybe Word64
    -- | Parameters of timeseries query interpretation.
  , interpCfg :: Interp.Config
} deriving (Show, Generic, ToJSON)

