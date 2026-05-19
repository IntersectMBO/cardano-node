{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

-- These are a special case of "forge-stress" profiles (with `P.delegators 0`).
--------------------------------------------------------------------------------

module Cardano.Benchmarking.Profile.Builtin.Legacy.Dish (
  profilesNoEraDish
) where

--------------------------------------------------------------------------------

import           Prelude
import           Data.Function ((&))
-- Package: self.
import qualified Cardano.Benchmarking.Profile.Builtin.ForgeStress as FS
import qualified Cardano.Benchmarking.Profile.Primitives as P
import qualified Cardano.Benchmarking.Profile.Types as Types
import qualified Cardano.Benchmarking.Profile.Vocabulary as V

--------------------------------------------------------------------------------

{- This from "prof1-variants.jq" was not honored ???
      , shelley:
        { protocolParams:
          { maxBlockBodySize:               (1 * $M)
          }
        }
-}
profilesNoEraDish :: [Types.Profile]
profilesNoEraDish =
  let dish =
        -- Using `P.newTracing` like all "forge-stress" profiles.
          P.empty & FS.base
        . V.hosts 3
        . V.genesisVariant300
        . P.delegators 0 . P.dreps 0
        . FS.durationM
        . P.traceForwardingOn
        -- (1 * $M / (360 * 20))
      value = V.valueBase . P.tps 138.88888888888889 . P.analysisUnitary
  in [
    dish & P.name "dish"            . value        . P.utxo 30000000
  , dish & P.name "dish-10M"        . value        . P.utxo 10000000
  , dish & P.name "dish-plutus"     . V.plutusLoop . P.utxo 30000000 . P.analysisSizeSmall
  , dish & P.name "dish-10M-plutus" . V.plutusLoop . P.utxo 10000000 . P.analysisSizeSmall
  ]
