{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Protocol.Conway
  ( ConwayProtocolInstantiationError(..)
    -- * Reusable parts
  , readGenesis
  , validateGenesis
  ) where

import           Cardano.Api

import qualified Cardano.Ledger.Conway.Genesis as Conway
import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Shelley (GenesisReadError, readGenesisAny)
import           Cardano.Node.Types
import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)

--
-- Conway genesis
--

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (Conway.ConwayGenesis StandardCrypto, GenesisHash)
readGenesis = readGenesisAny

validateGenesis :: Conway.ConwayGenesis StandardCrypto
                -> ExceptT ConwayProtocolInstantiationError IO ()
validateGenesis _ = return () --TODO conway: do the validation

data ConwayProtocolInstantiationError
  = InvalidCostModelError !FilePath
  | CostModelExtractionError !FilePath
  | ConwayCostModelFileError !(FileError ())
  | ConwayCostModelDecodeError !FilePath !String
  deriving Show

instance Error ConwayProtocolInstantiationError where
  prettyError (InvalidCostModelError fp) =
    "Invalid cost model: " <> pshow fp
  prettyError (CostModelExtractionError fp) =
    "Error extracting the cost model at: " <> pshow fp
  prettyError (ConwayCostModelFileError err) =
    prettyError err
  prettyError (ConwayCostModelDecodeError fp err) =
    "Error decoding cost model at: " <> pshow fp <> " Error: " <> pshow err
