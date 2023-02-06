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
import           Control.Monad.Writer (runWriter)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra

import           Cardano.CLI.Shelley.Run.Read (readFileScriptInAnyLang)

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..), ProtocolParameters (..), fromAlonzoExUnits,
                   protocolParamCostModels, toPlutusData)
import           Cardano.Ledger.Alonzo.TxInfo (exBudgetToExUnits)

import qualified PlutusLedgerApi.V1 as PlutusV1
import qualified PlutusLedgerApi.V2 as PlutusV2

import           Cardano.TxGenerator.Types


readPlutusScript :: FilePath -> IO (Either TxGenError ScriptInAnyLang)
readPlutusScript fp
  = runExceptT $ do
    script <- firstExceptT ApiError $
      readFileScriptInAnyLang fp
    case script of
      ScriptInAnyLang (PlutusScriptLanguage _) _ -> pure script
      ScriptInAnyLang lang _ -> throwE $ TxGenError $ "readPlutusScript: only PlutusScript supported, found: " ++ show lang

preExecutePlutusScript ::
     ProtocolParameters
  -> ScriptInAnyLang
  -> ScriptData
  -> ScriptRedeemer
  -> Either TxGenError ExecutionUnits
preExecutePlutusScript protocolParameters script@(ScriptInAnyLang scriptLang _) datum redeemer
  = runExcept $ do
    case script of
      ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) script' ->
        hoistEither $ preExecutePlutusV1 protocolParameters script' datum redeemer
      ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2) script' ->
        hoistEither $ preExecutePlutusV2 protocolParameters script' datum redeemer
      _ ->
        throwE $ TxGenError $ "preExecutePlutusScript: script not supported: " ++ show scriptLang

preExecutePlutusV1 ::
     ProtocolParameters
  -> Script PlutusScriptV1
  -> ScriptData
  -> ScriptRedeemer
  -> Either TxGenError ExecutionUnits
preExecutePlutusV1 protocolParameters (PlutusScript _ (PlutusScriptSerialised script)) datum redeemer
    = fst $                       -- for now, we discard warnings (:: PlutusCore.Evaluation.Machine.CostModelInterface.CostModelApplyWarn)
        runWriter $ runExceptT go
  where
    go
      = do
      CostModel costModel <- hoistMaybe (TxGenError "preExecutePlutusScript: costModel unavailable") $
        AnyPlutusScriptVersion PlutusScriptV1 `Map.lookup` protocolParamCostModels protocolParameters
      evaluationContext <- firstExceptT PlutusError $
        PlutusV1.mkEvaluationContext (flattenCostModel costModel)

      let
        (majVer, minVer) = protocolParamProtocolVersion protocolParameters
        protocolVersion = PlutusV1.ProtocolVersion (fromIntegral majVer) (fromIntegral minVer)

      exBudget <- firstExceptT PlutusError $
        hoistEither $
          snd $ PlutusV1.evaluateScriptCounting protocolVersion PlutusV1.Verbose evaluationContext script
            [ toPlutusData datum
            , toPlutusData redeemer
            , PlutusV1.toData dummyContext
            ]

      x <- hoistMaybe (TxGenError "preExecutePlutusScript: could not convert to execution units") $
        exBudgetToExUnits exBudget
      return $ fromAlonzoExUnits x

    dummyContext :: PlutusV1.ScriptContext
    dummyContext = PlutusV1.ScriptContext dummyTxInfo (PlutusV1.Spending dummyOutRef)

    dummyOutRef :: PlutusV1.TxOutRef
    dummyOutRef = PlutusV1.TxOutRef (PlutusV1.TxId "") 0

    dummyTxInfo :: PlutusV1.TxInfo
    dummyTxInfo = PlutusV1.TxInfo
      { PlutusV1.txInfoInputs = []
      , PlutusV1.txInfoOutputs = []
      , PlutusV1.txInfoFee = mempty
      , PlutusV1.txInfoMint = mempty
      , PlutusV1.txInfoDCert = []
      , PlutusV1.txInfoWdrl = []
      , PlutusV1.txInfoValidRange = PlutusV1.always
      , PlutusV1.txInfoSignatories = []
      , PlutusV1.txInfoData = []
      , PlutusV1.txInfoId = PlutusV1.TxId ""
      }

preExecutePlutusV2 ::
     ProtocolParameters
  -> Script PlutusScriptV2
  -> ScriptData
  -> ScriptRedeemer
  -> Either TxGenError ExecutionUnits
preExecutePlutusV2 protocolParameters (PlutusScript _ (PlutusScriptSerialised script)) datum redeemer
    = fst $                       -- for now, we discard warnings (:: PlutusCore.Evaluation.Machine.CostModelInterface.CostModelApplyWarn)
        runWriter $ runExceptT go
  where
    go
      = do
      CostModel costModel <- hoistMaybe (TxGenError "preExecutePlutusScript: costModel unavailable") $
        AnyPlutusScriptVersion PlutusScriptV1 `Map.lookup` protocolParamCostModels protocolParameters
      evaluationContext <- firstExceptT PlutusError $
        PlutusV2.mkEvaluationContext (flattenCostModel costModel)

      let
        (majVer, minVer) = protocolParamProtocolVersion protocolParameters
        protocolVersion = PlutusV2.ProtocolVersion (fromIntegral majVer) (fromIntegral minVer)

      exBudget <- firstExceptT PlutusError $
        hoistEither $
          snd $ PlutusV2.evaluateScriptCounting protocolVersion PlutusV2.Verbose evaluationContext script
            [ toPlutusData datum
            , toPlutusData redeemer
            , PlutusV2.toData dummyContext
            ]

      x <- hoistMaybe (TxGenError "preExecutePlutusScript: could not convert to execution units") $
        exBudgetToExUnits exBudget
      return $ fromAlonzoExUnits x

    dummyContext :: PlutusV2.ScriptContext
    dummyContext = PlutusV2.ScriptContext dummyTxInfo (PlutusV2.Spending dummyOutRef)

    dummyOutRef :: PlutusV2.TxOutRef
    dummyOutRef = PlutusV2.TxOutRef (PlutusV2.TxId "") 0

    dummyTxInfo :: PlutusV2.TxInfo
    dummyTxInfo = PlutusV2.TxInfo
      { PlutusV2.txInfoInputs = []
      , PlutusV2.txInfoOutputs = []
      , PlutusV2.txInfoFee = mempty
      , PlutusV2.txInfoMint = mempty
      , PlutusV2.txInfoDCert = []
      , PlutusV2.txInfoWdrl = PlutusV2.fromList []
      , PlutusV2.txInfoValidRange = PlutusV2.always
      , PlutusV2.txInfoSignatories = []
      , PlutusV2.txInfoData = PlutusV2.fromList []
      , PlutusV2.txInfoId = PlutusV2.TxId ""
      , PlutusV2.txInfoReferenceInputs = []
      , PlutusV2.txInfoRedeemers = PlutusV2.fromList []
      }

-- This is an incredibly bad idea. The order of the output list is inportant, but:
--  * This way of flattening it is not guaranteed to always be correct.
--  * There is no way to ensure that the list remains in the correct order.
-- IMO, the `[Integer]` should *NEVER* have been exposed from `ledger`.
flattenCostModel :: Map Text Integer -> [Integer]
flattenCostModel = map snd . Map.toAscList
