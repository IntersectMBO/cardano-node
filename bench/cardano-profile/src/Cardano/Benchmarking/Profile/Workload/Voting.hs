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

-- Two workloads: proposal creation / fund splitting must complete before the
-- load window opens (phase "setup"), vote submission runs alongside the
-- tx-generator (phase "load").
votingWorkload :: KeyMap.KeyMap Aeson.Value -> [Types.Workload]
votingWorkload parameters = [
    Types.Workload {
      Types.workloadName = "voting-setup"
    , Types.parameters = parameters
    , Types.entrypoint = "workflow_setup"
    , Types.phase = Types.Setup
    , Types.placement = Types.Explorer
    , Types.wait_pools = True
    }
  , Types.Workload {
      Types.workloadName = "voting"
    , Types.parameters = parameters
    , Types.entrypoint = "workflow_producer"
    , Types.phase = Types.Load
    , Types.placement = Types.Producers
    , Types.wait_pools = True
    }
  ]

votingWorkloadUtxo :: [Types.Workload]
votingWorkloadUtxo = votingWorkload $
  KeyMap.fromList [
    ("outs_per_split_transaction", Aeson.Number 193)
  , ("submit_vote", Aeson.Bool False)
  ]

votingWorkloadx1 :: [Types.Workload]
votingWorkloadx1 = votingWorkload $
  KeyMap.fromList [
    ("outs_per_split_transaction", Aeson.Number 193)
  , ("submit_vote", Aeson.Bool True)
  , ("votes_per_tx", Aeson.Number 1)
  ]

votingWorkloadx2 :: [Types.Workload]
votingWorkloadx2 = votingWorkload $
  KeyMap.fromList [
    ("outs_per_split_transaction", Aeson.Number 193)
  , ("submit_vote", Aeson.Bool True)
  , ("votes_per_tx", Aeson.Number 2)
  ]
