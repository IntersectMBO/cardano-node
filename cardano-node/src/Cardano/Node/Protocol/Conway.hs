{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Protocol.Conway
  ( ConwayProtocolInstantiationError(..)
    -- * Reusable parts
  , readGenesis
  , validateGenesis
  ) where

import           Cardano.Api
import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Shelley (GenesisReadError, readGenesisAny)
import           Cardano.Node.Types
import           Cardano.Prelude
import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()
import           Prelude (String)

import qualified Cardano.Ledger.Conway.Genesis as Conway
import qualified Ouroboros.Consensus.Protocol.Praos as Praos

--
-- Conway genesis
--

readGenesis ::
     Praos.PraosCrypto c
  => GenesisFile
  -> Maybe GenesisHash
  -> ExceptT GenesisReadError IO (Conway.ConwayGenesis c, GenesisHash)
readGenesis = readGenesisAny

validateGenesis ::
     Conway.ConwayGenesis c
  -> ExceptT ConwayProtocolInstantiationError IO ()
validateGenesis _ = return () --TODO Conway: do the validation

data ConwayProtocolInstantiationError
  = InvalidCostModelError !FilePath
  | CostModelExtractionError !FilePath
  | ConwayCostModelFileError !(FileError ())
  | ConwayCostModelDecodeError !FilePath !String
  deriving Show

instance Error ConwayProtocolInstantiationError where
  displayError (InvalidCostModelError fp) =
    "Invalid cost model: " <> show fp
  displayError (CostModelExtractionError fp) =
    "Error extracting the cost model at: " <> show fp
  displayError (ConwayCostModelFileError err) =
    displayError err
  displayError (ConwayCostModelDecodeError fp err) =
    "Error decoding cost model at: " <> show fp <> " Error: " <> err

