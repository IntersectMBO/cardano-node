{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Protocol.Dijkstra
  ( readGenesis
  , readGenesisMaybe
  , emptyDijkstraGenesis
  ) where

import           Cardano.Api

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Binary as L
import           Cardano.Ledger.Core (MaxPledgeLeverage (..))
import           Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import qualified Cardano.Ledger.Dijkstra.Genesis as Dijkstra
import           Cardano.Ledger.Dijkstra.PParams
import           Cardano.Ledger.Plutus.ExUnits (ExUnits (..), OrdExUnits (..))
import qualified Cardano.Ledger.Plutus.CostModels as L
import qualified Cardano.Ledger.Plutus.Language as L
import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Shelley (GenesisReadError, readGenesisAny)
import           Cardano.Node.Types

import qualified Data.ByteString.Lazy as LB
import           Data.Int
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Word

readGenesisMaybe :: Maybe GenesisFile
                 -> Maybe GenesisHash
                 -> ExceptT GenesisReadError IO
                            (Dijkstra.DijkstraGenesis, GenesisHash)
readGenesisMaybe (Just genFp) mHash = readGenesis genFp mHash
readGenesisMaybe Nothing _ = do
  let dijkstraGenesis = emptyDijkstraGenesis
      genesisHash = GenesisHash (Crypto.hashWith id $ LB.toStrict $ L.serialize (L.natVersion @11) emptyDijkstraGenesis)
  return (dijkstraGenesis, genesisHash)

-- PlutusV4 uses the same parameter set as PlutusV3.
-- Language index 3 corresponds to PlutusV4.
plutusV4CostModel :: Map.Map Word8 [Int64]
plutusV4CostModel = Map.singleton 3 plutusV4ExampleValues

plutusV4ExampleValues :: [Int64]
plutusV4ExampleValues =
  [ 205665, 812, 1, 1, 1000, 571, 0, 1, 1000, 24177, 4, 1, 1000, 32, 117366, 10475
  , 4, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 23000, 100, 100
  , 100, 23000, 100, 19537, 32, 175354, 32, 46417, 4, 221973, 511, 0, 1, 89141, 32
  , 497525, 14068, 4, 2, 196500, 453240, 220, 0, 1, 1, 1000, 28662, 4, 2, 245000
  , 216773, 62, 1, 1060367, 12586, 1, 208512, 421, 1, 187000, 1000, 52998, 1, 80436
  , 32, 43249, 32, 1000, 32, 80556, 1, 57667, 4, 1000, 10, 197145, 156, 1, 197145
  , 156, 1, 204924, 473, 1, 208896, 511, 1, 52467, 32, 64832, 32, 65493, 32, 22558
  , 32, 16563, 32, 76511, 32, 196500, 453240, 220, 0, 1, 1, 69522, 11687, 0, 1, 60091
  , 32, 196500, 453240, 220, 0, 1, 1, 196500, 453240, 220, 0, 1, 1, 1159724, 392670
  , 0, 2, 806990, 30482, 4, 1927926, 82523, 4, 265318, 0, 4, 0, 85931, 32, 205665, 812
  , 1, 1, 41182, 32, 212342, 32, 31220, 32, 32696, 32, 43357, 32, 32247, 32, 38314, 32
  , 35190005, 10, 57996947, 18975, 10, 39121781, 32260, 10, 23000, 100, 23000, 100, 832808
  , 18, 3209094, 6, 331451, 1, 65990684, 23097, 18, 114242, 18, 94393407, 87060, 18, 16420089
  , 18, 2145798, 36, 3795345, 12, 889023, 1, 204237282, 23271, 36, 129165, 36, 189977790
  , 85902, 36, 33012864, 36, 388443360, 1, 401885761, 72, 2331379, 72, 1927926, 82523
  , 4, 117366, 10475, 4, 1292075, 24469, 74, 0, 1, 936157, 49601, 237, 0, 1
  ]

emptyDijkstraGenesis :: DijkstraGenesis
emptyDijkstraGenesis =
  case L.mkCostModelsLenient plutusV4CostModel >>= Map.lookup L.PlutusV4 . L.costModelsValid of
    Nothing -> error "emptyDijkstraGenesis: missing PlutusV4 cost model in default cost models."
    Just cm ->
      let upgradePParamsDef = UpgradeDijkstraPParams
                                { udppMaxRefScriptSizePerBlock = 1048576
                                , udppMaxRefScriptSizePerTx = 204800
                                , udppRefScriptCostStride = unsafeNonZero 25600
                                , udppRefScriptCostMultiplier = fromMaybe (error "impossible") $ boundRational 1.2
                                , udppMaxPledgeLeverage = MaxPledgeLeverage SNothing
                                , udppMinPoolMargin = minBound
                                , udppPlutusV4CostModel = cm
                                , udppLeiosAnnouncementPeriodLength = Milliseconds32 1000
                                , udppLeiosVotePeriodLength = Milliseconds32 1000
                                , udppLeiosDiffusionPeriodLength = Milliseconds32 1000
                                , udppLeiosCommitteeSize = 1
                                , udppLeiosQuorumStakeThreshold = minBound
                                , udppMaxEndorserBlockReferencesSize = 0
                                , udppMaxEndorserBlockTxsSize = 0
                                , udppMaxEndorserBlockExUnits = OrdExUnits (ExUnits 0 0)
                                , udppMaxRefScriptSizePerEndorserBlock = 0
                                }
      in DijkstraGenesis { dgUpgradePParams = upgradePParamsDef }


readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (Dijkstra.DijkstraGenesis, GenesisHash)
readGenesis = readGenesisAny
