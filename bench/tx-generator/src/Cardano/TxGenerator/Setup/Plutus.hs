{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Cardano.TxGenerator.Setup.Plutus
Description : Convenience functions for dealing with Plutus scripts
 -}
module Cardano.TxGenerator.Setup.Plutus
       ( readPlutusScript
       , preExecutePlutusScript
       , toAnyPlutusScript
       , mkPlutusSpendingWitness
       )
       where

import           Data.Bifunctor
import           Data.ByteString.Short (ShortByteString)
import           Data.Functor (void)
import           Data.Int (Int64)
import           Data.Map.Strict as Map (lookup)

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import           Control.Monad.Writer (runWriter)

import           Cardano.CLI.Read (readFileScriptInAnyLang)

import           Cardano.Api hiding (PScript, PlutusScriptInEra, PlutusScriptWitness)
import           Cardano.Api.Experimental (AnyWitness (..), PlutusScriptOrReferenceInput (..),
                   PlutusScriptWitness (..), mkSpendingScriptDatum, obtainLangConstraints,
                   toPlutusSLanguage)
import           Cardano.Api.Experimental.AnyScriptWitness (AnyPlutusScriptWitness (..),
                   createPlutusSpendingScriptWitness)
import           Cardano.Api.Experimental.Plutus (AnyPlutusScript (..), PlutusScriptInEra (..),
                   plutusScriptInEraSLanguage)
import qualified Cardano.Ledger.Plutus.Language as L (PlutusBinary (..), PlutusLanguage, Plutus (..),
                   SLanguage, decodePlutusRunnable, plutusRunnableResult)
import           Cardano.Ledger.Plutus.TxInfo (exBudgetToExUnits)

import qualified PlutusLedgerApi.V1 as PlutusV1
import qualified PlutusLedgerApi.V2 as PlutusV2
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusTx.AssocMap as AssocMap (empty)

import           Cardano.TxGenerator.ProtocolParameters (ProtocolParameters(..))
import           Cardano.TxGenerator.Types (TxGenError (..), TxGenPlutusResolvedTo (..))
import           Control.Exception (SomeException (..), try, displayException)
import           System.FilePath ((<.>), (</>))
#ifdef WITH_LIBRARY
import           Cardano.Benchmarking.PlutusScripts (findPlutusScript)
#endif

import           Paths_tx_generator hiding (version)

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

-- | 'toAnyPlutusScript' decodes a 'ScriptInAnyLang' (as returned by 'readPlutusScript')
-- into the ledger-side runnable representation needed to build a script witness.
-- Fails for: a non-Plutus script; a Plutus language unsupported in this era
-- (V1 needs Alonzo onwards, V2 needs Babbage onwards, checked via
-- 'scriptLanguageSupportedInEra' - the same check
-- 'Cardano.TxGenerator.UTxO.mkUTxOScript' uses); or a binary payload that
-- fails to decode against the era's protocol version.
toAnyPlutusScript :: forall era. ShelleyBasedEra era -> ScriptInAnyLang -> Either TxGenError (AnyPlutusScript (ShelleyLedgerEra era))
toAnyPlutusScript sbe script
  = case script of
      ScriptInAnyLang lang (PlutusScript version (PlutusScriptSerialised sbs)) -> do
        alonzoOnwards <-
          forEraMaybeEon (toCardanoEra sbe)
            ?! TxGenError "toAnyPlutusScript: Plutus scripts are not supported before Alonzo"
        void $
          scriptLanguageSupportedInEra sbe lang
            ?! TxGenError ("toAnyPlutusScript: " ++ show lang ++ " is not supported in " ++ show sbe)
        alonzoEraOnwardsConstraints alonzoOnwards $ do
          let
            slang = toPlutusSLanguage version
            decode :: forall l. L.PlutusLanguage l => L.SLanguage l -> Either TxGenError (AnyPlutusScript (ShelleyLedgerEra era))
            decode _ = do
              let runnable = L.decodePlutusRunnable @l (eraProtVerHigh sbe) (L.Plutus (L.PlutusBinary sbs))
              AnyPlutusScript (PlutusScriptInEra runnable)
                <$ L.plutusRunnableResult runnable
                ?!& \err -> TxGenError $ "toAnyPlutusScript: script decode failed: " ++ show err
          obtainLangConstraints slang $ decode slang
      ScriptInAnyLang lang _ ->
        Left $ TxGenError $ "toAnyPlutusScript: only PlutusScript supported, found: " ++ show lang

-- | 'mkPlutusSpendingWitness' builds the transaction witness for spending a UTxO locked
-- by a Plutus script, dispatching on the script's Plutus language version.
mkPlutusSpendingWitness ::
     AnyPlutusScript ledgerEra
  -> HashableScriptData
  -> ScriptRedeemer
  -> ExecutionUnits
  -> AnyWitness ledgerEra
mkPlutusSpendingWitness (AnyPlutusScript plutusScript) datum redeemer executionUnits =
  AnyPlutusScriptWitness $ AnyPlutusSpendingScriptWitness $
    createPlutusSpendingScriptWitness slang $
      PlutusScriptWitness slang (PScript plutusScript) (mkSpendingScriptDatum slang datum) redeemer executionUnits
  where
    slang = plutusScriptInEraSLanguage plutusScript

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

flattenCostModel :: CostModel -> [Int64]
flattenCostModel (CostModel cm) = cm
