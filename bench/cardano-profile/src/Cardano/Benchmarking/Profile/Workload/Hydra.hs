{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Workload.Hydra (
  hydraWorkload
) where

--------------------------------------------------------------------------------

import           Prelude
-- Package: aeson.
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

hydraWorkload :: Types.Workload
hydraWorkload = Types.Workload {
    Types.workloadName = "hydra"
  , Types.parameters = KeyMap.fromList [
      ("baseport", Aeson.Number 31000)
      -- Up to (maximun) 9 hydra heads per deployed Cardano node.
    , ("heads_per_cardano_node", Aeson.Number 2)
    ]
  , Types.entrypoints = Types.Entrypoints {
      Types.pre_generator = Nothing
    , Types.producers = "hydra"
    }
  , Types.before_nodes = False
  , Types.wait_pools = True
}

