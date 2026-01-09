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
import           Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis (..))
import qualified Cardano.Ledger.Dijkstra.Genesis as Dijkstra
import           Cardano.Ledger.Dijkstra.PParams
import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Shelley (GenesisReadError, readGenesisAny)
import           Cardano.Node.Types
import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import qualified Data.ByteString.Lazy as LB
import           Data.Maybe (fromMaybe)

readGenesisMaybe :: Maybe GenesisFile
                 -> Maybe GenesisHash
                 -> ExceptT GenesisReadError IO
                            (Dijkstra.DijkstraGenesis, GenesisHash)
readGenesisMaybe (Just genFp) mHash = readGenesis genFp mHash
readGenesisMaybe Nothing _ = do
  let dijkstraGenesis = emptyDijkstraGenesis
      genesisHash = GenesisHash (Crypto.hashWith id $ LB.toStrict $ L.serialize (L.natVersion @11) emptyDijkstraGenesis)
  return (dijkstraGenesis, genesisHash)

emptyDijkstraGenesis :: DijkstraGenesis
emptyDijkstraGenesis =
  let upgradePParamsDef =  UpgradeDijkstraPParams
                            { udppMaxRefScriptSizePerBlock = 1048576
                            , udppMaxRefScriptSizePerTx = 204800
                            , udppRefScriptCostStride = unsafeNonZero 25600
                            , udppRefScriptCostMultiplier = fromMaybe (error "impossible") $ boundRational 1.2
                            }
  in DijkstraGenesis { dgUpgradePParams = upgradePParamsDef }


readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (Dijkstra.DijkstraGenesis, GenesisHash)
readGenesis = readGenesisAny
