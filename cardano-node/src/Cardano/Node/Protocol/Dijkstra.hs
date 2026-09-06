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

import           Cardano.Api.Genesis (dijkstraGenesisDefaults)

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

-- | Deferring to the API's defaults keeps a node started without a Dijkstra
-- genesis file on exactly the parameters the CLI would have written for it.
emptyDijkstraGenesis :: DijkstraGenesis
emptyDijkstraGenesis = dijkstraGenesisDefaults


readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (Dijkstra.DijkstraGenesis, GenesisHash)
readGenesis = readGenesisAny
