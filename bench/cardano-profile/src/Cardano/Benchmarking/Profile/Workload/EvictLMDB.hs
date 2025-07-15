{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Workload.EvictLMDB (
  evictLMDBWorkload
) where

--------------------------------------------------------------------------------

import           Prelude
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

evictLMDBWorkload :: Types.Workload
evictLMDBWorkload = Types.Workload {
    Types.workloadName = "evict_lmdb"
  , Types.parameters = mempty
  , Types.entrypoints = Types.Entrypoints {
      Types.pre_generator = Nothing
    , Types.producers = "evict_lmdb"
    }
  , Types.wait_pools = True
}
