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

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Control.Monad.Writer (runWriter)

import           Cardano.CLI.Shelley.Run.Read (readFileScriptInAnyLang)

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..), ProtocolParameters (..), fromAlonzoExUnits,
                   protocolParamCostModels, toPlutusData)
import           Cardano.Ledger.Alonzo.TxInfo (exBudgetToExUnits)

import qualified Plutus.V1.Ledger.Api as PlutusV1
import qualified Plutus.V2.Ledger.Api as PlutusV2
import qualified PlutusTx.AssocMap as PlutusMap (empty)

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
    costModel <- hoistMaybe (TxGenError $ "preExecutePlutusScript: cost model unavailable for: " ++ show scriptLang) $
      case script of
        ScriptInAnyLang _ (PlutusScript lang _) ->
          AnyPlutusScriptVersion lang `Map.lookup` protocolParamCostModels protocolParameters
        _ -> Nothing

    case script of
      ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) script' ->
        preExecutePlutusV1 protocolVersion script' datum redeemer costModel
      ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2) script' ->
        preExecutePlutusV2 protocolVersion script' datum redeemer costModel
      _ ->
        throwE $ TxGenError $ "preExecutePlutusScript: script not supported: " ++ show scriptLang
  where
    protocolVersion = protocolParamProtocolVersion protocolParameters

preExecutePlutusV1 ::
     (Natural, Natural)
  -> Script PlutusScriptV1
  -> ScriptData
<<<<<<< HEAD
  -> ScriptRedeemer
  -> CostModel
  -> Except TxGenError ExecutionUnits
preExecutePlutusV1 (majVer, minVer) (PlutusScript _ (PlutusScriptSerialised script)) datum redeemer (CostModel costModel)
  = do
    evaluationContext <- firstExceptT PlutusError $
      PlutusV1.mkEvaluationContext costModel

    let
      protocolVersion = PlutusV1.ProtocolVersion (fromIntegral majVer) (fromIntegral minVer)

    exBudget <- firstExceptT PlutusError $
      hoistEither $
        snd $ PlutusV1.evaluateScriptCounting protocolVersion PlutusV1.Verbose evaluationContext script
          [ toPlutusData datum
          , toPlutusData redeemer
          , PlutusV1.toData dummyContext
          ]

    x <- hoistMaybe (TxGenError "preExecutePlutusV1: could not convert to execution units") $
      exBudgetToExUnits exBudget
    return $ fromAlonzoExUnits x
  where
    dummyContext :: PlutusV1.ScriptContext
    dummyContext = PlutusV1.ScriptContext dummyTxInfo (PlutusV1.Spending dummyOutRef)
=======
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
>>>>>>> 99b603779 (Updated ledger and ouroboros-network dependencies)

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
     (Natural, Natural)
  -> Script PlutusScriptV2
  -> ScriptData
  -> ScriptRedeemer
  -> CostModel
  -> Except TxGenError ExecutionUnits
preExecutePlutusV2 (majVer, minVer) (PlutusScript _ (PlutusScriptSerialised script)) datum redeemer (CostModel costModel)
  = do
    evaluationContext <- firstExceptT PlutusError $
      PlutusV2.mkEvaluationContext costModel

    let
      protocolVersion = PlutusV2.ProtocolVersion (fromIntegral majVer) (fromIntegral minVer)

    exBudget <- firstExceptT PlutusError $
      hoistEither $
        snd $ PlutusV2.evaluateScriptCounting protocolVersion PlutusV2.Verbose evaluationContext script
          [ toPlutusData datum
          , toPlutusData redeemer
          , PlutusV2.toData dummyContext
          ]

    x <- hoistMaybe (TxGenError "preExecutePlutusV2: could not convert to execution units") $
      exBudgetToExUnits exBudget
    return $ fromAlonzoExUnits x
  where
    dummyContext :: PlutusV2.ScriptContext
    dummyContext = PlutusV2.ScriptContext dummyTxInfo (PlutusV2.Spending dummyOutRef)

    dummyOutRef :: PlutusV2.TxOutRef
    dummyOutRef = PlutusV2.TxOutRef (PlutusV2.TxId "") 0

    dummyTxInfo :: PlutusV2.TxInfo
    dummyTxInfo = PlutusV2.TxInfo
      { PlutusV2.txInfoInputs = []
      , PlutusV2.txInfoReferenceInputs = []
      , PlutusV2.txInfoOutputs = []
      , PlutusV2.txInfoFee = mempty
      , PlutusV2.txInfoMint = mempty
      , PlutusV2.txInfoDCert = []
      , PlutusV2.txInfoWdrl = PlutusMap.empty
      , PlutusV2.txInfoValidRange = PlutusV2.always
      , PlutusV2.txInfoSignatories = []
      , PlutusV2.txInfoRedeemers = PlutusMap.empty
      , PlutusV2.txInfoData = PlutusMap.empty
      , PlutusV2.txInfoId = PlutusV2.TxId ""
      }
