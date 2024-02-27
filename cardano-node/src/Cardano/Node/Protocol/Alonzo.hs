{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Protocol.Alonzo
  ( AlonzoProtocolInstantiationError(..)
    -- * Reusable parts
  , readGenesis
  , validateGenesis
  ) where

import           Cardano.Api

import qualified Cardano.Ledger.Alonzo.Genesis as Alonzo
import           Cardano.Node.Orphans ()
import           Cardano.Node.Protocol.Shelley (GenesisReadError, readGenesisAny)
import           Cardano.Node.Types
import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()


--
-- Alonzo genesis
--

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (Alonzo.AlonzoGenesis, GenesisHash)
readGenesis = readGenesisAny

validateGenesis :: Alonzo.AlonzoGenesis
                -> ExceptT AlonzoProtocolInstantiationError IO ()
validateGenesis _ = return () --TODO alonzo: do the validation

data AlonzoProtocolInstantiationError
  = InvalidCostModelError !FilePath
  | CostModelExtractionError !FilePath
  | AlonzoCostModelFileError !(FileError ())
  | AlonzoCostModelDecodeError !FilePath !String
  deriving Show

instance Error AlonzoProtocolInstantiationError where
  prettyError (InvalidCostModelError fp) =
    "Invalid cost model: " <> pshow fp
  prettyError (CostModelExtractionError fp) =
    "Error extracting the cost model at: " <> pshow fp
  prettyError (AlonzoCostModelFileError err) =
    prettyError err
  prettyError (AlonzoCostModelDecodeError fp err) =
    "Error decoding cost model at: " <> pshow fp <> " Error: " <> pshow err
