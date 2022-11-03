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

import           Data.Map as Map (Map, lookup, toAscList)
import           Data.Text (Text)

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Control.Monad.Writer (runWriter)

import           Cardano.CLI.Shelley.Run.Read (readFileScriptInAnyLang)

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..), ProtocolParameters (..), fromAlonzoExUnits,
                   protocolParamCostModels, toPlutusData)
import           Cardano.Ledger.Alonzo.TxInfo (exBudgetToExUnits)

import qualified PlutusLedgerApi.V1 as Plutus
import           PlutusLedgerApi.V1.Contexts (ScriptContext (..), ScriptPurpose (..), TxInfo (..),
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
  = fst $                       -- for now, we discard warnings (:: PlutusCore.Evaluation.Machine.CostModelInterface.CostModelApplyWarn)
    runWriter $ runExceptT go
  where
    go
      = do
      CostModel costModel <- hoistMaybe (TxGenError "preExecutePlutusScript: costModel unavailable") $
        AnyPlutusScriptVersion PlutusScriptV1 `Map.lookup` protocolParamCostModels protocolParameters
      evaluationContext <- firstExceptT PlutusError $
        Plutus.mkEvaluationContext (flattenCostModel costModel)

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

    -- TODO: drop flattenCostModel when newtype CostModel in Cardano.Api.ProtocolParameters
    -- might be changed to the flattened representation rather than the key-value map.
    -- Context: The flattened list is sorted in the order given by `ParamName` enum, which is the lexicographic ordering.
    flattenCostModel :: Map Text Integer -> [Integer]
    flattenCostModel = map snd . Map.toAscList

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
