{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Protocol.Alonzo
  ( AlonzoProtocolInstantiationError(..)
    -- * Reusable parts
  , readAlonzoGenesis
  ) where

import           Cardano.Prelude

import           Cardano.Api

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import           Data.Aeson (FromJSON (..), withObject, (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as Text
import           System.IO.Error (isDoesNotExistError)

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.Translation as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusCore.Evaluation.Machine.ExBudgeting as Plutus


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
createAlonzoGenesis (AlonzoGenWrapper costModelFp' alonzoGenesis) = do
  costModel <- readAndDecode
  case Plutus.extractModelParams costModel of
    Just m -> if Plutus.validateCostModelParams m
              then left $ InvalidCostModelError costModelFp'
              else return $ alonzoGenesis { Alonzo.costmdls = Map.singleton Alonzo.PlutusV1 $ Alonzo.CostModel m }

    Nothing -> left CostModelExtractionError -- TODO: costModel
 where
  readAndDecode :: ExceptT AlonzoProtocolInstantiationError IO Plutus.CostModel
  readAndDecode = do
      lbs <- handleIOExceptT (AlonzoCostModelFileError . FileIOError costModelFp') $ LBS.readFile costModelFp'
      firstExceptT (AlonzoCostModelDecodeError costModelFp' . Text.pack)
        . hoistEither $ Aeson.eitherDecode' lbs


data AlonzoGenWrapper =
  AlonzoGenWrapper { costModelFp :: FilePath
                   , genesis :: Alonzo.AlonzoGenesis
                   }

instance FromJSON AlonzoGenWrapper where
  parseJSON = withObject "Alonzo Genesis Wrapper" $ \o -> do
                -- NB: This has an empty map for the cost model
                alonzoGenensis <- parseJSON (Aeson.Object o) :: Aeson.Parser Alonzo.AlonzoGenesis
                cModelFp <- o .: "alonzoCostModel"
                return $ AlonzoGenWrapper
                           { costModelFp = cModelFp
                           , genesis = alonzoGenensis
                           }

data AlonzoProtocolInstantiationError
  = InvalidCostModelError !FilePath
  | CostModelExtractionError
  | AlonzoCostModelFileError !(FileError ())
  | AlonzoCostModelDecodeError !FilePath !Text
  | AlonzoGenesisFileError !(FileError ())
  | AlonzoGenesisDecodeError !FilePath !Text
  | GenesisFileNotFound !FilePath
  deriving Show


