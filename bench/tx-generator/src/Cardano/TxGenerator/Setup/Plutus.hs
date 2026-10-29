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
import           Data.Int (Int64)
import           Data.Map.Strict as Map (lookup)

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Control.Monad.Writer (runWriter)

import           Cardano.CLI.Read (readFileScriptInAnyLang)

import           Cardano.Api
import           Cardano.Ledger.Plutus.TxInfo (exBudgetToExUnits)

import qualified PlutusLedgerApi.V1 as PlutusV1
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusLedgerApi.V4 as PlutusV4
import qualified PlutusTx.AssocMap as AssocMap (empty)

import           Cardano.TxGenerator.ProtocolParameters (ProtocolParameters(..))
import           Cardano.TxGenerator.Types (TxGenError (..), TxGenPlutusResolvedTo (..))
import           Control.Exception (SomeException (..), try, displayException)
import           System.FilePath ((<.>), (</>))
#ifdef WITH_LIBRARY
import           Cardano.Benchmarking.PlutusScripts (findPlutusScript)
#endif

import           Paths_tx_generator

type ProtocolVersion = (Int, Int)


resolveFromLibrary :: String -> Maybe ScriptInAnyLang
#ifdef WITH_LIBRARY
resolveFromLibrary = findPlutusScript
#else
resolveFromLibrary = const Nothing
#endif

-- | 'readPlutusScript' accepts a string for the name of a script that
-- may be known in the 'Left' case and a filepath to read as a script
-- in the 'Right' case. API errors are signalled via an 'Either'.
-- What the @WITH_LIBRARY@ flag signifies is to use a set of statically-
-- defined (via TH) scripts for the script name lookups instead of a
-- set of library files.
readPlutusScript :: Either String FilePath -> IO (Either TxGenError (ScriptInAnyLang, TxGenPlutusResolvedTo))
readPlutusScript (Left s)
  = case resolveFromLibrary s of
      Just s' -> pure $ Right (s', ResolvedToLibrary s)
      Nothing -> try (getDataFileName $ "scripts-fallback" </> asFileName) >>= either
        (\(SomeException e) -> pure $ Left $ TxGenError $ show e)
        doLoad
  where
    asFileName = s <.> "plutus"
    doLoad fp  = second (second (const $ ResolvedToFallback asFileName)) <$> readPlutusScript (Right fp)
readPlutusScript (Right fp)
  = runExceptT $ do
    script <-
       handleExceptT (\(e :: SomeException) -> ApiError $ displayException e) (readFileScriptInAnyLang fp)
    case script of
      ScriptInAnyLang (PlutusScriptLanguage _) _ -> pure (script, ResolvedToFileName fp)
      ScriptInAnyLang lang _ -> throwE $ TxGenError $ "readPlutusScript: only PlutusScript supported, found: " ++ show lang

-- | 'preExecutePlutusScript' is a front end for the internal
-- @preExecutePlutusVn@ functions used to calculate 'ExecutionUnits'
-- that switches on Plutus versions. The @evaluateScriptCounting@
-- functions of 'PlutusV1' to 'PlutusV4' do the actual work on the script's
-- binary representation to count the number of execution units needed.
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
      ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV4) script' ->
        hoistEither $ preExecutePlutusV4 protocolVersion script' datum redeemer costModel
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
      , PlutusV2.txInfoWdrl = PlutusV2.unsafeFromList []
      , PlutusV2.txInfoValidRange = PlutusV2.always
      , PlutusV2.txInfoSignatories = []
      , PlutusV2.txInfoData = PlutusV2.unsafeFromList []
      , PlutusV2.txInfoId = PlutusV2.TxId ""
      , PlutusV2.txInfoReferenceInputs = []
      , PlutusV2.txInfoRedeemers = PlutusV2.unsafeFromList []
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
                (PlutusV3.toData scriptContext)

      x <- hoistMaybe (TxGenError "preExecutePlutusV3: could not convert to execution units") $
        exBudgetToExUnits exBudget
      return $ fromAlonzoExUnits x

    r :: PlutusV3.Redeemer
    r = PlutusV3.Redeemer $ PlutusV3.dataToBuiltinData $ toPlutusData $ getScriptData redeemer

    d :: PlutusV3.Datum
    d = PlutusV3.Datum $ PlutusV3.dataToBuiltinData $ toPlutusData datum

    scriptContext :: PlutusV3.ScriptContext
    scriptContext = PlutusV3.ScriptContext dummyTxInfo r scriptInfo

    scriptInfo :: PlutusV3.ScriptInfo
    scriptInfo = PlutusV3.SpendingScript dummyOutRef (Just d)

    dummyOutRef :: PlutusV3.TxOutRef
    dummyOutRef = PlutusV3.TxOutRef (PlutusV3.TxId "") 0

    dummyTxInfo :: PlutusV3.TxInfo
    dummyTxInfo = PlutusV3.TxInfo
      { PlutusV3.txInfoInputs = []
      , PlutusV3.txInfoOutputs = []
      , PlutusV3.txInfoFee = 0
      , PlutusV3.txInfoMint = PlutusV3.emptyMintValue
      , PlutusV3.txInfoTxCerts = []
      , PlutusV3.txInfoWdrl = PlutusV3.unsafeFromList []
      , PlutusV3.txInfoValidRange = PlutusV3.always
      , PlutusV3.txInfoSignatories = []
      , PlutusV3.txInfoData = PlutusV3.unsafeFromList []
      , PlutusV3.txInfoId = PlutusV3.TxId ""
      , PlutusV3.txInfoReferenceInputs = []
      , PlutusV3.txInfoRedeemers = PlutusV3.unsafeFromList []
      , PlutusV3.txInfoVotes = AssocMap.empty
      , PlutusV3.txInfoProposalProcedures = []
      , PlutusV3.txInfoCurrentTreasuryAmount = Nothing
      , PlutusV3.txInfoTreasuryDonation = Nothing
      }

-- | Like 'preExecutePlutusV3', with the V4 script context: it adds the hash
-- of the executing script, and its transaction info has new fields, here
-- all empty. A V4 context is encoded as a list rather than a constructor.
preExecutePlutusV4 ::
     ProtocolVersion
  -> Script PlutusScriptV4
  -> ScriptData
  -> ScriptRedeemer
  -> CostModel
  -> Either TxGenError ExecutionUnits
preExecutePlutusV4 (major, _minor) (PlutusScript _ (PlutusScriptSerialised (script :: ShortByteString {- a.k.a. SerialisedScript -}))) datum redeemer costModel
  = fst $ runWriter $ runExceptT go       -- for now, we discard warnings (:: PlutusCore.Evaluation.Machine.CostModelInterface.CostModelApplyWarn)
  where
    protocolVersion = PlutusV4.MajorProtocolVersion major
    go
      = do
      evaluationContext <- firstExceptT PlutusError $
        PlutusV4.mkEvaluationContext (flattenCostModel costModel)

      scriptForEval <- withExceptT PlutusError $ PlutusV4.deserialiseScript protocolVersion script
      exBudget <- firstExceptT PlutusError $
        hoistEither .
          snd $ PlutusV4.evaluateScriptCounting protocolVersion PlutusV4.Verbose evaluationContext scriptForEval
                (PlutusV4.toData scriptContext)

      x <- hoistMaybe (TxGenError "preExecutePlutusV4: could not convert to execution units") $
        exBudgetToExUnits exBudget
      return $ fromAlonzoExUnits x

    r :: PlutusV4.Redeemer
    r = PlutusV4.Redeemer $ PlutusV4.dataToBuiltinData $ toPlutusData $ getScriptData redeemer

    d :: PlutusV4.Datum
    d = PlutusV4.Datum $ PlutusV4.dataToBuiltinData $ toPlutusData datum

    scriptContext :: PlutusV4.ScriptContext
    scriptContext = PlutusV4.ScriptContext dummyTxInfo r scriptInfo (PlutusV4.ScriptHash "")

    scriptInfo :: PlutusV4.ScriptInfo
    scriptInfo = PlutusV4.SpendingScript dummyOutRef (Just d)

    dummyOutRef :: PlutusV4.TxOutRef
    dummyOutRef = PlutusV4.TxOutRef (PlutusV4.TxId "") 0

    dummyTxInfo :: PlutusV4.TxInfo
    dummyTxInfo = PlutusV4.TxInfo
      { PlutusV4.txInfoId = PlutusV4.TxId ""
      , PlutusV4.txInfoSubTxIx = Nothing
      , PlutusV4.txInfoInputs = []
      , PlutusV4.txInfoReferenceInputs = []
      , PlutusV4.txInfoOutputs = []
      , PlutusV4.txInfoMint = PlutusV4.emptyMintValue
      , PlutusV4.txInfoTxCerts = []
      , PlutusV4.txInfoWithdrawals = AssocMap.empty
      , PlutusV4.txInfoDirectDeposits = AssocMap.empty
      , PlutusV4.txInfoAccountBalanceIntervals = PlutusV4.AccountBalanceIntervals AssocMap.empty
      , PlutusV4.txInfoValidRange = PlutusV4.POSIXTimeRange Nothing Nothing
      , PlutusV4.txInfoGuards = []
      , PlutusV4.txInfoRequiredTopLevelGuards = AssocMap.empty
      , PlutusV4.txInfoRedeemers = AssocMap.empty
      , PlutusV4.txInfoData = AssocMap.empty
      , PlutusV4.txInfoVotes = AssocMap.empty
      , PlutusV4.txInfoProposalProcedures = []
      , PlutusV4.txInfoCurrentTreasuryAmount = Nothing
      , PlutusV4.txInfoTreasuryDonation = 0
      }

flattenCostModel :: CostModel -> [Int64]
flattenCostModel (CostModel cm) = cm
