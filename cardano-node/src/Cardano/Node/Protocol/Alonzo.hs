{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Protocol.Alonzo
  ( AlonzoProtocolInstantiationError(..)
    -- * Reusable parts
  , readAlonzoGenesis
  ) where

import           Prelude (String)
import           Cardano.Prelude

import           Cardano.Api

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           System.IO.Error (isDoesNotExistError)

import qualified Cardano.Ledger.Alonzo.Genesis as Alonzo


import           Cardano.Node.Orphans ()

import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()


--
-- Alonzo genesis
--

-- | In order to avoid introducing a separate Alonzo genesis file, we
-- have added additional fields to the Shelley genesis that are required
-- when hardforking to Alonzo. Unfortunately the 'ShelleyGenesis' 'FromJSON'
-- instance exists in cardano-ledger-specs so we must duplicate code for now.

readAlonzoGenesis
  :: FilePath
  -> ExceptT AlonzoProtocolInstantiationError IO Alonzo.AlonzoGenesis
readAlonzoGenesis fpath = do
  readAndDecode
    `catchError` \err ->
      case err of
        AlonzoGenesisFileError (FileIOError _ ioe)
          | isDoesNotExistError ioe -> left $ GenesisFileNotFound fpath
        _                           -> left err
 where
  readAndDecode :: ExceptT AlonzoProtocolInstantiationError IO Alonzo.AlonzoGenesis
  readAndDecode = do
      lbs <- handleIOExceptT (AlonzoGenesisFileError . FileIOError fpath) $ LBS.readFile fpath
      firstExceptT (AlonzoGenesisDecodeError fpath)
        . hoistEither $ Aeson.eitherDecode' lbs


data AlonzoProtocolInstantiationError
  = InvalidCostModelError !FilePath
  | CostModelExtractionError !FilePath
  | AlonzoCostModelFileError !(FileError ())
  | AlonzoCostModelDecodeError !FilePath !String
  | AlonzoGenesisFileError !(FileError ())
  | AlonzoGenesisDecodeError !FilePath !String
  | GenesisFileNotFound !FilePath
  deriving Show

instance Error AlonzoProtocolInstantiationError where
  displayError (InvalidCostModelError fp) =
    "Invalid cost model: " <> show fp
  displayError (CostModelExtractionError fp) =
    "Error extracting the cost model at: " <> show fp
  displayError (AlonzoCostModelFileError err) =
    displayError err
  displayError (AlonzoCostModelDecodeError fp err) =
    "Error decoding cost model at: " <> show fp <> " Error: " <> err
  displayError (AlonzoGenesisFileError err) =
    displayError err
  displayError (AlonzoGenesisDecodeError fp err) =
    "Error decoding genesis at: " <> fp <> " Error: " <> err
  displayError (GenesisFileNotFound fp) =
    "Genesis file not found at: " <> fp

