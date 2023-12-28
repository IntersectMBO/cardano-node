{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Cardano.TxGenerator.Setup.Plutus
Description : Convenience functions for dealing with Plutus scripts
 -}
module Cardano.TxGenerator.Setup.Plutus
       ( readPlutusScript
       , preExecutePlutusScript
       )
       where

import           Data.Bifunctor
import           Data.ByteString.Short (ShortByteString)
import           Data.Map.Strict as Map (lookup)

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Control.Monad.Writer (runWriter)

import           Cardano.CLI.Read (readFileScriptInAnyLang)

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..), ProtocolParameters (..), fromAlonzoExUnits,
                   protocolParamCostModels, toPlutusData)
import           Cardano.Ledger.Plutus.TxInfo (exBudgetToExUnits)

import qualified PlutusLedgerApi.V1 as PlutusV1
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusTx.AssocMap as AssocMap (empty)

import           Cardano.TxGenerator.Types (TxGenError (..))

#ifdef WITH_LIBRARY
import           Cardano.Benchmarking.PlutusScripts (findPlutusScript)
#else
import           Control.Exception (SomeException (..), try)
import           Paths_tx_generator
#endif

type ProtocolVersion = (Int, Int)

-- | 'readPlutusScript' accepts a string for the name of a script that
-- may be known in the 'Left' case and a filepath to read as a script
-- in the 'Right' case. API errors are signalled via an 'Either'.
-- What the @WITH_LIBRARY@ flag signifies is to use a set of statically-
-- defined (via TH) scripts for the script name lookups instead of a
-- set of library files.
readPlutusScript :: Either String FilePath -> IO (Either TxGenError ScriptInAnyLang)
#ifdef WITH_LIBRARY
readPlutusScript (Left s)
  = pure
  $ maybe (Left . TxGenError $ "readPlutusScript: " ++ s ++ " not found.")
          Right
          (findPlutusScript s)
#else
readPlutusScript (Left s)
  = try (getDataFileName $ "scripts-fallback/" ++ s ++ ".plutus") >>= either
      (\(SomeException e) -> pure $ Left $ TxGenError $ show e)
      (readPlutusScript . Right)
#endif

readPlutusScript (Right fp)
  = runExceptT $ do
    script <- firstExceptT ApiError $
      readFileScriptInAnyLang fp
    case script of
      ScriptInAnyLang (PlutusScriptLanguage _) _ -> pure script
      ScriptInAnyLang lang _ -> throwE $ TxGenError $ "readPlutusScript: only PlutusScript supported, found: " ++ show lang

-- | 'preExecutePlutusScript' is a front end for the internal
-- @preExecutePlutusVn@ functions used to calculate 'ExecutionUnits'
-- that switches on Plutus versions. The
-- 'PlutusV1.evaluateScriptCounting', 'PlutusV2.evaluateScriptCounting'
-- and 'PlutusV3.evaluateScriptCounting' functions do the actual work on
-- the script's binary representation to count the number of execution
-- units needed.
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
      ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3) script' ->
        hoistEither $ preExecutePlutusV3 protocolVersion script' datum redeemer costModel
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
    protocolVersion = PlutusV1.MajorProtocolVersion (fst protocolVersion_)
    go
      = do
      evaluationContext <- firstExceptT PlutusError $
        PlutusV1.mkEvaluationContext (flattenCostModel costModel)

      deserialisedScript <- firstExceptT PlutusError $ PlutusV1.deserialiseScript protocolVersion script
      exBudget <- firstExceptT PlutusError $
        hoistEither $
          snd $ PlutusV1.evaluateScriptCounting protocolVersion PlutusV1.Verbose evaluationContext deserialisedScript
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
preExecutePlutusV2 (major, _minor) (PlutusScript _ (PlutusScriptSerialised script)) datum redeemer costModel
  = fst $ runWriter $ runExceptT go       -- for now, we discard warnings (:: PlutusCore.Evaluation.Machine.CostModelInterface.CostModelApplyWarn)
  where
    protocolVersion = PlutusV2.MajorProtocolVersion major
    go
      = do
      evaluationContext <- firstExceptT PlutusError $
        PlutusV2.mkEvaluationContext (flattenCostModel costModel)

      deserialisedScript <- firstExceptT PlutusError $ PlutusV2.deserialiseScript protocolVersion script

      exBudget <- firstExceptT PlutusError $
        hoistEither $
          snd $ PlutusV2.evaluateScriptCounting protocolVersion PlutusV2.Verbose evaluationContext deserialisedScript
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

preExecutePlutusV3 ::
     ProtocolVersion
  -> Script PlutusScriptV3
  -> ScriptData
  -> ScriptRedeemer
  -> CostModel
  -> Either TxGenError ExecutionUnits
preExecutePlutusV3 (major, _minor) (PlutusScript _ (PlutusScriptSerialised (script :: ShortByteString {- a.k.a. SerialisedScript -}))) datum redeemer costModel
  = fst $ runWriter $ runExceptT go       -- for now, we discard warnings (:: PlutusCore.Evaluation.Machine.CostModelInterface.CostModelApplyWarn)
  where
    protocolVersion = PlutusV3.MajorProtocolVersion major
    go
      = do
      evaluationContext <- firstExceptT PlutusError $
        PlutusV3.mkEvaluationContext (flattenCostModel costModel)

      scriptForEval <- withExceptT PlutusError $ PlutusV3.deserialiseScript protocolVersion script
      exBudget <- firstExceptT PlutusError $
        hoistEither .
          snd $ PlutusV3.evaluateScriptCounting protocolVersion PlutusV3.Verbose evaluationContext scriptForEval
            [ toPlutusData datum
            , toPlutusData (getScriptData redeemer)
            , PlutusV3.toData dummyContext
            ]

      x <- hoistMaybe (TxGenError "preExecutePlutusV3: could not convert to execution units") $
        exBudgetToExUnits exBudget
      return $ fromAlonzoExUnits x

    dummyContext :: PlutusV3.ScriptContext
    dummyContext = PlutusV3.ScriptContext dummyTxInfo (PlutusV3.Spending dummyOutRef)

    dummyOutRef :: PlutusV3.TxOutRef
    dummyOutRef = PlutusV3.TxOutRef (PlutusV3.TxId "") 0

    dummyTxInfo :: PlutusV3.TxInfo
    dummyTxInfo = PlutusV3.TxInfo
      { PlutusV3.txInfoInputs = []
      , PlutusV3.txInfoOutputs = []
      , PlutusV3.txInfoFee = 0
      , PlutusV3.txInfoMint = mempty
      , PlutusV3.txInfoTxCerts = []
      , PlutusV3.txInfoWdrl = PlutusV3.fromList []
      , PlutusV3.txInfoValidRange = PlutusV3.always
      , PlutusV3.txInfoSignatories = []
      , PlutusV3.txInfoData = PlutusV3.fromList []
      , PlutusV3.txInfoId = PlutusV3.TxId ""
      , PlutusV3.txInfoReferenceInputs = []
      , PlutusV3.txInfoRedeemers = PlutusV3.fromList []
      , PlutusV3.txInfoVotes = AssocMap.empty
      , PlutusV3.txInfoProposalProcedures = []
      , PlutusV3.txInfoCurrentTreasuryAmount = Nothing
      , PlutusV3.txInfoTreasuryDonation = Nothing
      }

flattenCostModel :: CostModel -> [Integer]
flattenCostModel (CostModel cm) = cm
