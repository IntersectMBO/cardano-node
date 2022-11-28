{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions when dealing with Plutus scripts.
--   It currently only supports PlutusV1 script & cost model.
module Cardano.TxGenerator.Setup.Plutus
       ( readPlutusScript
       , preExecutePlutusScript
       )
       where

import qualified Data.Map.Strict as Map

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra

import           Cardano.CLI.Shelley.Run.Read (readFileScriptInAnyLang)

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..), ProtocolParameters (..), fromAlonzoExUnits,
                   protocolParamCostModels, toPlutusData)
import           Cardano.Ledger.Alonzo.TxInfo (exBudgetToExUnits)

import qualified Plutus.V1.Ledger.Api as Plutus
import           Plutus.V1.Ledger.Contexts (ScriptContext (..), ScriptPurpose (..), TxInfo (..),
                   TxOutRef (..))

import           Cardano.TxGenerator.Types


readPlutusScript :: FilePath -> IO (Either TxGenError (Script PlutusScriptV1))
readPlutusScript fp
  = do
  res <- runExceptT $ readFileScriptInAnyLang fp
  return $ case res of
    Left err -> Left $ ApiError err
    Right (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) script) -> Right script
    Right (ScriptInAnyLang lang _) -> Left $ TxGenError $ "readPlutusScript: only PlutusScriptV1 currently supported, found: " ++ show lang

preExecutePlutusScript ::
     ProtocolParameters
  -> Script PlutusScriptV1
  -> ScriptData
  -> ScriptData
  -> Either TxGenError ExecutionUnits
preExecutePlutusScript protocolParameters (PlutusScript _ (PlutusScriptSerialised script)) datum redeemer
  = runExcept $ do
    CostModel costModel <- hoistMaybe (TxGenError "preExecutePlutusScript: costModel unavailable") $
      AnyPlutusScriptVersion PlutusScriptV1 `Map.lookup` protocolParamCostModels protocolParameters
    evaluationContext <- firstExceptT PlutusError $
      Plutus.mkEvaluationContext costModel

    let
      (majVer, minVer) = protocolParamProtocolVersion protocolParameters
      protocolVersion = Plutus.ProtocolVersion (fromIntegral majVer) (fromIntegral minVer)

    exBudget <- firstExceptT PlutusError $
      hoistEither $
        snd $ Plutus.evaluateScriptCounting protocolVersion Plutus.Verbose evaluationContext script
          [ toPlutusData datum
          , toPlutusData redeemer
          , Plutus.toData dummyContext
          ]

    x <- hoistMaybe (TxGenError "preExecutePlutusScript: could not convert to execution units") $
      exBudgetToExUnits exBudget
    return $ fromAlonzoExUnits x
  where
    dummyContext :: ScriptContext
    dummyContext = ScriptContext dummyTxInfo (Spending dummyOutRef)

    dummyOutRef :: TxOutRef
    dummyOutRef = TxOutRef (Plutus.TxId "") 0
    dummyTxInfo :: TxInfo
    dummyTxInfo = TxInfo
      { txInfoInputs = []
      , txInfoOutputs = []
      , txInfoFee = mempty
      , txInfoMint = mempty
      , txInfoDCert = []
      , txInfoWdrl = []
      , txInfoValidRange = Plutus.always
      , txInfoSignatories = []
      , txInfoData = []
      , txInfoId = Plutus.TxId ""
      }
