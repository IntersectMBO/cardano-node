{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Protocol.Alonzo
  ( AlonzoProtocolInstantiationError(..)
    -- * Reusable parts
  , readGenesis
  , validateGenesis
  ) where

import           Cardano.Api
import           Cardano.Api.Pretty

import qualified Cardano.Ledger.Alonzo.Genesis as Alonzo

import           Cardano.Node.Orphans ()
import           Cardano.Node.Types

import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import           Cardano.Node.Protocol.Shelley (GenesisReadError, readGenesisAny)

import           Control.Monad.Except (ExceptT)

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
  displayError (InvalidCostModelError fp) =
    "Invalid cost model: " <> pretty fp
  displayError (CostModelExtractionError fp) =
    "Error extracting the cost model at: " <> pretty fp
  displayError (AlonzoCostModelFileError err) =
    displayError err
  displayError (AlonzoCostModelDecodeError fp err) =
    "Error decoding cost model at: " <> pretty fp <> " Error: " <> pretty err

