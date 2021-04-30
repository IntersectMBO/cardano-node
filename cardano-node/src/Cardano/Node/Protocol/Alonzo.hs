{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Protocol.Alonzo
  ( AlonzoProtocolInstantiationError(..)
  , renderAlonzoProtocolInstantiationError
    -- * Reusable parts
  , readAlonzoGenesis
  ) where

import           Cardano.Prelude

import           Cardano.Api

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import           Data.Aeson (FromJSON (..), withObject, (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as Text
import           System.IO.Error (isDoesNotExistError)

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Translation as Alonzo

-- TODO: Remove me, cli should not depend directly on plutus repo.
import qualified PlutusCore.Evaluation.Machine.ExBudgeting as Plutus
import qualified PlutusCore.Evaluation.Machine.ExBudgetingDefaults as Plutus


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
  alonzoGenWrapper <- readAndDecode
                       `catchError` \err ->
                         case err of
                           AlonzoGenesisFileError (FileIOError _ ioe)
                             | isDoesNotExistError ioe -> left $ GenesisFileNotFound fpath
                           _                           -> left err
  createAlonzoGenesis alonzoGenWrapper

 where
  readAndDecode :: ExceptT AlonzoProtocolInstantiationError IO AlonzoGenWrapper
  readAndDecode = do
      lbs <- handleIOExceptT (AlonzoGenesisFileError . FileIOError fpath) $ LBS.readFile fpath
      firstExceptT (AlonzoGenesisDecodeError fpath . Text.pack)
        . hoistEither $ Aeson.eitherDecode' lbs


createAlonzoGenesis
  :: AlonzoGenWrapper
  -> ExceptT AlonzoProtocolInstantiationError IO Alonzo.AlonzoGenesis
createAlonzoGenesis (AlonzoGenWrapper costModelFp' alonzoGenesis) =
  case costModelFp' of
    Just fp -> do
       costModel <- readAndDecode fp
       case Plutus.extractModelParams costModel of
         -- TODO: We should not be using functions directly from the plutus repo
         -- These should be exposed via the ledger
         Just m -> -- if Plutus.validateCostModelParams m
                   -- then left $ ShelleyGenesisCmdCostModelsError costModelFp'
                   -- else
                   --TODO: Plutus repo needs to expose a cost model validation function
                   return $ alonzoGenesis { Alonzo.costmdls = Map.singleton Alonzo.PlutusV1 $ Alonzo.CostModel m }

         Nothing -> panic "createAlonzoGenesis: not implemented yet"
    Nothing ->
      case Plutus.extractModelParams Plutus.defaultCostModel of
        Just m ->
          if Alonzo.validateCostModelParams m
          then return $ alonzoGenesis { Alonzo.costmdls = Map.singleton Alonzo.PlutusV1 $ Alonzo.CostModel m  }
          else panic "createAlonzoGenesis: Plutus.defaultCostModel is invalid"
        Nothing -> panic "createAlonzoGenesis: Could not extract cost model params from Plutus.defaultCostModel"
 where
  readAndDecode :: FilePath -> ExceptT AlonzoProtocolInstantiationError IO Plutus.CostModel
  readAndDecode fp = do
      lbs <- handleIOExceptT (AlonzoCostModelFileError . FileIOError fp) $ LBS.readFile fp
      firstExceptT (AlonzoCostModelDecodeError fp . Text.pack)
        . hoistEither $ Aeson.eitherDecode' lbs


data AlonzoGenWrapper =
  AlonzoGenWrapper { costModelFp :: Maybe FilePath
                   , genesis :: Alonzo.AlonzoGenesis
                   }

instance FromJSON AlonzoGenWrapper where
  parseJSON = withObject "Alonzo Genesis Wrapper" $ \o -> do
                -- NB: This has an empty map for the cost model
                alonzoGenensis <- parseJSON (Aeson.Object o) :: Aeson.Parser Alonzo.AlonzoGenesis
                cModelFp <- o .:? "costModel"
                return $ AlonzoGenWrapper
                           { costModelFp = cModelFp
                           , genesis = alonzoGenensis
                           }

data AlonzoProtocolInstantiationError
  = InvalidCostModelError !FilePath
  | CostModelExtractionError !FilePath
  | AlonzoCostModelFileError !(FileError ())
  | AlonzoCostModelDecodeError !FilePath !Text
  | AlonzoGenesisFileError !(FileError ())
  | AlonzoGenesisDecodeError !FilePath !Text
  | GenesisFileNotFound !FilePath
  deriving Show

renderAlonzoProtocolInstantiationError :: AlonzoProtocolInstantiationError -> Text
renderAlonzoProtocolInstantiationError (InvalidCostModelError fp) =
  "Invalid cost model: " <> Text.pack (show fp)
renderAlonzoProtocolInstantiationError (CostModelExtractionError fp) =
  "Error extracting the cost model at: " <> Text.pack (show fp)
renderAlonzoProtocolInstantiationError (AlonzoCostModelFileError err) =
  Text.pack $ displayError err
renderAlonzoProtocolInstantiationError (AlonzoCostModelDecodeError fp err) =
  "Error decoding cost model at: " <> Text.pack (show fp) <> " Error: " <> err
renderAlonzoProtocolInstantiationError (AlonzoGenesisFileError err) =
  Text.pack $ displayError err
renderAlonzoProtocolInstantiationError (AlonzoGenesisDecodeError fp err) =
  "Error decoding genesis at: " <> Text.pack fp <> " Error: " <> err
renderAlonzoProtocolInstantiationError (GenesisFileNotFound fp) =
  "Genesis file not found at: " <> Text.pack fp

