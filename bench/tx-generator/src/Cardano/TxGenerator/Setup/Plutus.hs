{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions when dealing with Plutus scripts.
module Cardano.TxGenerator.Setup.Plutus
       ( readPlutusScript
       , preExecutePlutusScript
       )
       where

import           Data.Bifunctor (bimap)
import           Data.Map.Strict as Map (lookup)

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Control.Monad.Writer (runWriter)

import           Cardano.CLI.Shelley.Run.Read (readFileScriptInAnyLang)

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..), ProtocolParameters (..), fromAlonzoExUnits,
                   protocolParamCostModels, toPlutusData)
import           Cardano.Ledger.Alonzo.TxInfo (exBudgetToExUnits)

import qualified PlutusLedgerApi.V1 as PlutusV1
import qualified PlutusLedgerApi.V2 as PlutusV2

import           Cardano.TxGenerator.Types


type ProtocolVersion = (Int, Int)


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
preExecutePlutusScript
  ProtocolParameters{protocolParamCostModels, protocolParamProtocolVersion}
  script@(ScriptInAnyLang scriptLang _)
  datum
  redeemer
  = runExcept $ do
    costModel <- hoistMaybe (TxGenError $ "preExecutePlutusScript: cost model unavailable for: " ++ show scriptLang) $
      case script of
        ScriptInAnyLang _ (PlutusScript lang _) ->
          AnyPlutusScriptVersion lang `Map.lookup` protocolParamCostModels
        _ ->
          Nothing

    case script of
      ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) script' ->
        hoistEither $ preExecutePlutusV1 protocolVersion script' datum redeemer costModel
      ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2) script' ->
        hoistEither $ preExecutePlutusV2 protocolVersion script' datum redeemer costModel
      _ ->
        throwE $ TxGenError $ "preExecutePlutusScript: script not supported: " ++ show scriptLang
  where
    protocolVersion :: ProtocolVersion
    protocolVersion = bimap fromIntegral fromIntegral protocolParamProtocolVersion

preExecutePlutusV1 ::
     ProtocolVersion
  -> Script PlutusScriptV1
  -> ScriptData
  -> ScriptRedeemer
  -> CostModel
  -> Either TxGenError ExecutionUnits
preExecutePlutusV1 protocolVersion_ (PlutusScript _ (PlutusScriptSerialised script)) datum redeemer costModel
  = fst $ runWriter $ runExceptT go       -- for now, we discard warnings (:: PlutusCore.Evaluation.Machine.CostModelInterface.CostModelApplyWarn)
  where
    protocolVersion = uncurry PlutusV1.ProtocolVersion protocolVersion_
    go
      = do
      evaluationContext <- firstExceptT PlutusError $
        PlutusV1.mkEvaluationContext (flattenCostModel costModel)

      exBudget <- firstExceptT PlutusError $
        hoistEither $
          snd $ PlutusV1.evaluateScriptCounting protocolVersion PlutusV1.Verbose evaluationContext script
            [ toPlutusData datum
            , toPlutusData (getScriptData redeemer)
            , PlutusV1.toData dummyContext
            ]

      x <- hoistMaybe (TxGenError "preExecutePlutusV1: could not convert to execution units") $
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
     ProtocolVersion
  -> Script PlutusScriptV2
  -> ScriptData
  -> ScriptRedeemer
  -> CostModel
  -> Either TxGenError ExecutionUnits
preExecutePlutusV2 protocolVersion_ (PlutusScript _ (PlutusScriptSerialised script)) datum redeemer costModel
  = fst $ runWriter $ runExceptT go       -- for now, we discard warnings (:: PlutusCore.Evaluation.Machine.CostModelInterface.CostModelApplyWarn)
  where
    protocolVersion = uncurry PlutusV2.ProtocolVersion protocolVersion_
    go
      = do
      evaluationContext <- firstExceptT PlutusError $
        PlutusV2.mkEvaluationContext (flattenCostModel costModel)

      exBudget <- firstExceptT PlutusError $
        hoistEither $
          snd $ PlutusV2.evaluateScriptCounting protocolVersion PlutusV2.Verbose evaluationContext script
            [ toPlutusData datum
            , toPlutusData (getScriptData redeemer)
            , PlutusV2.toData dummyContext
            ]

      x <- hoistMaybe (TxGenError "preExecutePlutusV2: could not convert to execution units") $
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

flattenCostModel :: CostModel -> [Integer]
flattenCostModel (CostModel cm) = cm
