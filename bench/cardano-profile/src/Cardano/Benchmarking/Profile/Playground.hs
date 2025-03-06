{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Playground (
  profilesNoEraPlayground
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

--------------------------------------------------------------------------------

-- Corrections to fill the block memory budget with 4 txs per block.
calibrate15x :: Aeson.Object
calibrate15x =
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
calibrate2x :: Aeson.Object
calibrate2x =
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
        . V.clusterDefault -- TODO: "cluster" should be "null" here.
        . V.genesisVariantVoltaire
        -- Cloud Plutus workload
        . V.plutusTypeLoop . V.plutusBase . P.tps 0.85
        . P.analysisSizeSmall
      mem15x = P.budgetBlockMemoryOneAndAHalf
      mem2x  = P.budgetBlockMemoryDouble
  in [
  -- Voltaire (like cloud profiles)
  -- Baseline.
    ciBenchLike & P.name "calibrate-volt"
  , ciBenchLike & P.name "calibrate-blockmem-x1.5-volt"      . mem15x
  , ciBenchLike & P.name "calibrate-blockmem-x1.5-volt-fill" . mem15x . P.overlay calibrate15x
  , ciBenchLike & P.name "calibrate-blockmem-x2-volt"        . mem2x
  , ciBenchLike & P.name "calibrate-blockmem-x2-volt-fill"   . mem2x  . P.overlay calibrate2x
  ]
