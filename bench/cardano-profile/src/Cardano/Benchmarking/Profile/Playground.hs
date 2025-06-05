{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Playground (
    calibrateLoopBlockMemx15
  , calibrateLoopBlockMemx2
  , profilesNoEraPlayground
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: aeson.
import qualified Data.Aeson        as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Builtin.Miniature as M
import qualified Cardano.Benchmarking.Profile.Primitives        as P
import qualified Cardano.Benchmarking.Profile.Types             as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary        as V
import qualified Cardano.Benchmarking.Profile.Workload.Voting   as W

--------------------------------------------------------------------------------

-- Corrections to fill the block memory budget with 4 txs per block.
calibrateLoopBlockMemx15 :: Aeson.Object
calibrateLoopBlockMemx15 =
  KeyMap.fromList [
    ("genesis", Aeson.Object $ KeyMap.fromList [
      ("alonzo", Aeson.Object $ KeyMap.fromList [
        ("maxTxExUnits", Aeson.Object $ KeyMap.fromList [
          ("exUnitsMem", Aeson.Number 23250000)
        ])
      , ("maxBlockExUnits", Aeson.Object $ KeyMap.fromList [
          ("exUnitsSteps", Aeson.Number 20625739876)
        ])
      ])
    ])
  , ("generator", Aeson.Object $ KeyMap.fromList [
      -- "ns":"Mempool.RejectedTx","data":{"err":{"fee":1000000,"kind":"FeeTooSmallUTxO","minimum":1892175}
      ("tx_fee", Aeson.Number 1892175)
    ])
  ]

-- Corrections to fill the block memory budget with 4 txs per block.
calibrateLoopBlockMemx2 :: Aeson.Object
calibrateLoopBlockMemx2 =
  KeyMap.fromList [
    ("genesis", Aeson.Object $ KeyMap.fromList [
      ("alonzo", Aeson.Object $ KeyMap.fromList [
        ("maxTxExUnits", Aeson.Object $ KeyMap.fromList [
          ("exUnitsMem", Aeson.Number 31000000)
        ])
      , ("maxBlockExUnits", Aeson.Object $ KeyMap.fromList [
          ("exUnitsSteps", Aeson.Number 27500804996)
        ])
      ])
    ])
  , ("generator", Aeson.Object $ KeyMap.fromList [
      -- "ns":"Mempool.RejectedTx","data":{"err":{"fee":1000000,"kind":"FeeTooSmallUTxO","minimum":2463246}
      ("tx_fee", Aeson.Number 2463246)
    ])
  ]

compressedFor3Epochs :: Types.Profile -> Types.Profile
compressedFor3Epochs =
    V.timescaleCompressed
  . P.generatorEpochs 3 . P.initCooldown 5
  . P.shutdownOnOff

profilesNoEraPlayground :: [Types.Profile]
profilesNoEraPlayground =
  ------------------------------------------------------------------------------
  -- ci-bench like: 2 nodes, FixedLoaded and "--shutdown-on-block-synced 15"
  ------------------------------------------------------------------------------
  let ciBenchLike =
          P.empty & M.base . P.dreps 0
        . P.uniCircle . P.loopback . V.hosts 2
        . M.benchDuration
        . P.traceForwardingOn . P.newTracing
        . P.p2pOn
        . V.genesisVariantVoltaire
        -- Cloud Plutus workload
        . V.plutusTypeLoop . V.plutusBase . P.tps 0.85
        . P.analysisSizeSmall
      mem15x = P.budgetBlockMemoryOneAndAHalf
      mem2x  = P.budgetBlockMemoryDouble
      voting =
          P.empty &
          P.fixedLoaded
        . P.uniCircle . V.hosts 2 . P.loopback
        . V.genesisVariantVoltaire . P.voting . P.v10Preview
        . V.datasetMiniature
        . V.fundsVoting
        . compressedFor3Epochs
        . V.plutusDoublePlusSaturation . P.txFee 1000000
        . P.analysisStandard
  in [
  -- Budget profiles.
    ciBenchLike & P.name "calibrate-volt"
  , ciBenchLike & P.name "calibrate-blockmem-x1.5-volt"      . mem15x
  , ciBenchLike & P.name "calibrate-blockmem-x1.5-volt-fill" . mem15x . P.overlay calibrateLoopBlockMemx15
  , ciBenchLike & P.name "calibrate-blockmem-x2-volt"        . mem2x
  , ciBenchLike & P.name "calibrate-blockmem-x2-volt-fill"   . mem2x  . P.overlay calibrateLoopBlockMemx2
  -- Voting profiles.
  , voting & P.name "development-voting"
           . P.dreps 1000
           . P.workloadAppend W.votingWorkloadx2
           . P.traceForwardingOn . P.newTracing . P.p2pOff
  ]
