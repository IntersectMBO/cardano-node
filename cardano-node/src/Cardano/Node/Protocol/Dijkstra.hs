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
import qualified Cardano.Ledger.Binary as L
import           Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis)
import qualified Cardano.Ledger.Dijkstra.Genesis as Dijkstra
import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Shelley (GenesisReadError, readGenesisAny)
import           Cardano.Node.Types

import qualified Data.ByteString.Lazy as LB


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
-- Share the complete API defaults, including the V4 cost model and every
-- Dijkstra integration field.
emptyDijkstraGenesis = dijkstraGenesisDefaults


readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (Dijkstra.DijkstraGenesis, GenesisHash)
readGenesis = readGenesisAny
