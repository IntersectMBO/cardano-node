{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Workload.Voting (
  votingWorkloadUtxo, votingWorkloadx1, votingWorkloadx2
) where

--------------------------------------------------------------------------------

import           Prelude
-- Package: aeson.
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Types as Types

--------------------------------------------------------------------------------

votingWorkload :: KeyMap.KeyMap Aeson.Value -> Types.Workload
votingWorkload parameters = Types.Workload {
    Types.workloadName = "voting"
  , Types.parameters = parameters
  , Types.entrypoints = Types.Entrypoints {
      Types.pre_generator = Just "workflow_generator"
    , Types.producers = "workflow_producer"
    }
  , Types.before_nodes = False
  , Types.wait_pools = True
}

votingWorkloadUtxo :: Types.Workload
votingWorkloadUtxo = votingWorkload $
  KeyMap.fromList [
    ("outs_per_split_transaction", Aeson.Number 193)
  , ("submit_vote", Aeson.Bool False)
  ]

votingWorkloadx1 :: Types.Workload
votingWorkloadx1 = votingWorkload $
  KeyMap.fromList [
    ("outs_per_split_transaction", Aeson.Number 193)
  , ("submit_vote", Aeson.Bool True)
  , ("votes_per_tx", Aeson.Number 1)
  ]

votingWorkloadx2 :: Types.Workload
votingWorkloadx2 = votingWorkload $
  KeyMap.fromList [
    ("outs_per_split_transaction", Aeson.Number 193)
  , ("submit_vote", Aeson.Bool True)
  , ("votes_per_tx", Aeson.Number 2)
  ]
