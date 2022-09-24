{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( ShelleyTxCmdError(..)
  , renderShelleyTxCmdError
  , runTransactionCmd
  , readFileTx
  , readProtocolParametersSourceSpec
  , toTxOutInAnyEra
  ) where

import           Cardano.Prelude hiding (All, Any)
import           Prelude (String, error, id)

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, handleLeftT,
                   hoistEither, hoistMaybe, left, newExceptT)
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Type.Equality (TestEquality (..))
import qualified System.IO as IO


import           Cardano.Api
import           Cardano.Api.Byron hiding (SomeByronSigningKey (..))
import           Cardano.Api.Shelley

--TODO: do this nicely via the API too:
import qualified Cardano.Binary as CBOR
--TODO: following import needed for orphan Eq Script instance
import           Cardano.Ledger.Shelley.Scripts ()

import           Cardano.CLI.Run.Friendly (friendlyTxBS, friendlyTxBodyBS)
import           Cardano.CLI.Shelley.Key (InputDecodeError, readSigningKeyFileAnyOf)
import           Cardano.CLI.Shelley.Output
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Shelley.Run.Genesis (ShelleyGenesisCmdError (..),
                   readShelleyGenesisWithDefault)
import           Cardano.CLI.Shelley.Script
import           Cardano.CLI.Types

import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

{- HLINT ignore "Use let" -}

data ShelleyTxCmdError
  = ShelleyTxCmdAesonDecodeProtocolParamsError !FilePath !Text
  | ShelleyTxCmdReadFileError !(FileError ())
  | ShelleyTxCmdScriptFileError (FileError ScriptDecodeError)
  | ShelleyTxCmdReadTextViewFileError !(FileError TextEnvelopeError)
  | ShelleyTxCmdReadWitnessSigningDataError !ReadWitnessSigningDataError
  | ShelleyTxCmdReadRequiredSignerError !(FileError InputDecodeError)
  | ShelleyTxCmdRequiredSignerByronKeyError !SigningKeyFile
  | ShelleyTxCmdWriteFileError !(FileError ())
  | ShelleyTxCmdEraConsensusModeMismatch
      !(Maybe FilePath)
      !AnyConsensusMode
      !AnyCardanoEra
      -- ^ Era
  | ShelleyTxCmdMetadataJsonParseError !FilePath !String
  | ShelleyTxCmdMetadataConversionError !FilePath !TxMetadataJsonError
  | ShelleyTxCmdMetaValidationError !FilePath ![(Word64, TxMetadataRangeError)]
  | ShelleyTxCmdScriptDataJsonParseError  !FilePath !String
  | ShelleyTxCmdScriptDataConversionError !FilePath !ScriptDataJsonError
  | ShelleyTxCmdScriptDataValidationError !FilePath !ScriptDataRangeError
  | ShelleyTxCmdMetaDecodeError !FilePath !CBOR.DecoderError
  | ShelleyTxCmdBootstrapWitnessError !ShelleyBootstrapWitnessError
  | ShelleyTxCmdSocketEnvError !EnvSocketError
  | ShelleyTxCmdTxSubmitError !Text
  | ShelleyTxCmdTxSubmitErrorEraMismatch !EraMismatch
  | ShelleyTxCmdTxFeatureMismatch !AnyCardanoEra !TxFeature
  | ShelleyTxCmdTxBodyError !TxBodyError
  | ShelleyTxCmdNotImplemented !Text
  | ShelleyTxCmdWitnessEraMismatch !AnyCardanoEra !AnyCardanoEra !WitnessFile
  | ShelleyTxCmdScriptLanguageNotSupportedInEra !AnyScriptLanguage !AnyCardanoEra
  | ShelleyTxCmdReferenceScriptsNotSupportedInEra !AnyCardanoEra
  | ShelleyTxCmdScriptExpectedSimple !FilePath !AnyScriptLanguage
  | ShelleyTxCmdScriptExpectedPlutus !FilePath !AnyScriptLanguage
  | ShelleyTxCmdGenesisCmdError !ShelleyGenesisCmdError
  | ShelleyTxCmdPolicyIdsMissing ![PolicyId]
  | ShelleyTxCmdPolicyIdsExcess  ![PolicyId]
  | ShelleyTxCmdUnsupportedMode !AnyConsensusMode
  | ShelleyTxCmdByronEra
  | ShelleyTxCmdEraConsensusModeMismatchTxBalance
      !TxBuildOutputOptions
      !AnyConsensusMode
      !AnyCardanoEra
  | ShelleyTxCmdBalanceTxBody !TxBodyErrorAutoBalance
  | ShelleyTxCmdTxInsDoNotExist !TxInsExistError
  | ShelleyTxCmdMinimumUTxOErr !MinimumUTxOError
  | ShelleyTxCmdPParamsErr !ProtocolParametersError
  | ShelleyTxCmdTextEnvCddlError
      !(FileError TextEnvelopeError)
      !(FileError TextEnvelopeCddlError)
  | ShelleyTxCmdTxExecUnitsErr !TransactionValidityError
  | ShelleyTxCmdPlutusScriptCostErr !PlutusScriptCostError
  | ShelleyTxCmdPParamExecutionUnitsNotAvailable
  | ShelleyTxCmdPlutusScriptsRequireCardanoMode
  | ShelleyTxCmdProtocolParametersNotPresentInTxBody
  | ShelleyTxCmdTxEraCastErr EraCastError
  | ShelleyTxCmdQueryConvenienceError !QueryConvenienceError
  | ShelleyTxCmdQueryNotScriptLocked !NotScriptLockedTxInsError

renderShelleyTxCmdError :: ShelleyTxCmdError -> Text
renderShelleyTxCmdError err =
  case err of
    ShelleyTxCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdReadTextViewFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdScriptFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdReadWitnessSigningDataError witSignDataErr ->
      renderReadWitnessSigningDataError witSignDataErr
    ShelleyTxCmdReadRequiredSignerError fileErr ->
      "Error reading required signer: " <> Text.pack (displayError fileErr)
    ShelleyTxCmdRequiredSignerByronKeyError (SigningKeyFile fp) ->
      "Byron key witness was used as a required signer: " <> show fp
    ShelleyTxCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdMetadataJsonParseError fp jsonErr ->
       "Invalid JSON format in file: " <> show fp
                <> "\nJSON parse error: " <> Text.pack jsonErr
    ShelleyTxCmdMetadataConversionError fp metadataErr ->
       "Error reading metadata at: " <> show fp
                             <> "\n" <> Text.pack (displayError metadataErr)
    ShelleyTxCmdMetaDecodeError fp metadataErr ->
       "Error decoding CBOR metadata at: " <> show fp
                             <> " Error: " <> show metadataErr
    ShelleyTxCmdMetaValidationError fp errs ->
      "Error validating transaction metadata at: " <> show fp <> "\n" <>
      Text.intercalate "\n"
        [ "key " <> show k <> ":" <> Text.pack (displayError valErr)
        | (k, valErr) <- errs ]

    ShelleyTxCmdScriptDataJsonParseError  fp jsonErr ->
       "Invalid JSON format in file: " <> show fp <>
       "\nJSON parse error: " <> Text.pack jsonErr
    ShelleyTxCmdScriptDataConversionError fp cerr ->
       "Error reading metadata at: " <> show fp
                             <> "\n" <> Text.pack (displayError cerr)
    ShelleyTxCmdScriptDataValidationError fp verr ->
      "Error validating script data at: " <> show fp <> ":\n" <>
      Text.pack (displayError verr)

    ShelleyTxCmdSocketEnvError envSockErr -> renderEnvSocketError envSockErr
    ShelleyTxCmdAesonDecodeProtocolParamsError fp decErr ->
      "Error while decoding the protocol parameters at: " <> show fp
                                            <> " Error: " <> show decErr
    ShelleyTxCmdTxSubmitError res -> "Error while submitting tx: " <> res
    ShelleyTxCmdTxSubmitErrorEraMismatch EraMismatch{ledgerEraName, otherEraName} ->
      "The era of the node and the tx do not match. " <>
      "The node is running in the " <> ledgerEraName <>
      " era, but the transaction is for the " <> otherEraName <> " era."
    ShelleyTxCmdBootstrapWitnessError sbwErr ->
      renderShelleyBootstrapWitnessError sbwErr

    ShelleyTxCmdTxFeatureMismatch era TxFeatureImplicitFees ->
      "An explicit transaction fee must be specified for " <>
      renderEra era <> " era transactions."

    ShelleyTxCmdTxFeatureMismatch (AnyCardanoEra ShelleyEra)
                                  TxFeatureValidityNoUpperBound ->
      "A TTL must be specified for Shelley era transactions."

    ShelleyTxCmdTxFeatureMismatch era feature ->
      renderFeature feature <> " cannot be used for " <> renderEra era <>
      " era transactions."

    ShelleyTxCmdTxBodyError err' ->
      "Transaction validaton error: " <> Text.pack (displayError err')

    ShelleyTxCmdNotImplemented msg ->
      "Feature not yet implemented: " <> msg

    ShelleyTxCmdWitnessEraMismatch era era' (WitnessFile file) ->
      "The era of a witness does not match the era of the transaction. " <>
      "The transaction is for the " <> renderEra era <> " era, but the " <>
      "witness in " <> show file <> " is for the " <> renderEra era' <> " era."

    ShelleyTxCmdScriptLanguageNotSupportedInEra (AnyScriptLanguage lang) era ->
      "The script language " <> show lang <> " is not supported in the " <>
      renderEra era <> " era."

    ShelleyTxCmdScriptExpectedSimple file (AnyScriptLanguage lang) ->
      Text.pack file <> ": expected a script in the simple script language, " <>
      "but it is actually using " <> show lang <> ". Alternatively, to use " <>
      "a Plutus script, you must also specify the redeemer " <>
      "(datum if appropriate) and script execution units."

    ShelleyTxCmdScriptExpectedPlutus file (AnyScriptLanguage lang) ->
      Text.pack file <> ": expected a script in the Plutus script language, " <>
      "but it is actually using " <> show lang <> "."

    ShelleyTxCmdEraConsensusModeMismatch fp mode era ->
       "Submitting " <> renderEra era <> " era transaction (" <> show fp <>
       ") is not supported in the " <> renderMode mode <> " consensus mode."
    ShelleyTxCmdGenesisCmdError e -> Text.pack $ displayError e
    ShelleyTxCmdPolicyIdsMissing policyids ->
      "The \"--mint\" flag specifies an asset with a policy Id, but no \
      \corresponding monetary policy script has been provided as a witness \
      \(via the \"--mint-script-file\" flag). The policy Id in question is: "
      <> Text.intercalate ", " (map serialiseToRawBytesHexText policyids)

    ShelleyTxCmdPolicyIdsExcess policyids ->
      "A script provided to witness minting does not correspond to the policy \
      \id of any asset specified in the \"--mint\" field. The script hash is: "
      <> Text.intercalate ", " (map serialiseToRawBytesHexText policyids)
    ShelleyTxCmdUnsupportedMode mode -> "Unsupported mode: " <> renderMode mode
    ShelleyTxCmdByronEra -> "This query cannot be used for the Byron era"
    ShelleyTxCmdEraConsensusModeMismatchTxBalance fp mode era ->
       "Cannot balance " <> renderEra era <> " era transaction body (" <> show fp <>
       ") because is not supported in the " <> renderMode mode <> " consensus mode."
    ShelleyTxCmdBalanceTxBody err' -> Text.pack $ displayError err'
    ShelleyTxCmdTxInsDoNotExist e ->
      renderTxInsExistError e
    ShelleyTxCmdMinimumUTxOErr err' -> Text.pack $ displayError err'
    ShelleyTxCmdPParamsErr err' -> Text.pack $ displayError err'
    ShelleyTxCmdTextEnvCddlError textEnvErr cddlErr ->
      "Failed to decode neither the cli's serialisation format nor the ledger's \
      \CDDL serialisation format. TextEnvelope error: " <> Text.pack (displayError textEnvErr) <> "\n" <>
      "TextEnvelopeCddl error: " <> Text.pack (displayError cddlErr)
    ShelleyTxCmdTxExecUnitsErr err' ->  Text.pack $ displayError err'
    ShelleyTxCmdPlutusScriptCostErr err'-> Text.pack $ displayError err'
    ShelleyTxCmdPParamExecutionUnitsNotAvailable ->
      "Execution units not available in the protocol parameters. This is \
      \likely due to not being in the Alonzo era"
    ShelleyTxCmdReferenceScriptsNotSupportedInEra (AnyCardanoEra era) ->
      "TxCmd: Reference scripts not supported in era: " <> show era
    ShelleyTxCmdTxEraCastErr (EraCastError value fromEra toEra) ->
      "Unable to cast era from " <> show fromEra <> " to " <> show toEra <> " the value " <> show value
    ShelleyTxCmdQueryConvenienceError e ->
      renderQueryConvenienceError e
    ShelleyTxCmdQueryNotScriptLocked e ->
      renderNotScriptLockedTxInsError e
    ShelleyTxCmdPlutusScriptsRequireCardanoMode ->
      "Plutus scripts are only available in CardanoMode"
    ShelleyTxCmdProtocolParametersNotPresentInTxBody ->
      "Protocol parameters were not found in transaction body"

renderEra :: AnyCardanoEra -> Text
renderEra (AnyCardanoEra ByronEra)   = "Byron"
renderEra (AnyCardanoEra ShelleyEra) = "Shelley"
renderEra (AnyCardanoEra AllegraEra) = "Allegra"
renderEra (AnyCardanoEra MaryEra)    = "Mary"
renderEra (AnyCardanoEra AlonzoEra)  = "Alonzo"
renderEra (AnyCardanoEra BabbageEra) = "Babbage"

renderFeature :: TxFeature -> Text
renderFeature TxFeatureShelleyAddresses     = "Shelley addresses"
renderFeature TxFeatureExplicitFees         = "Explicit fees"
renderFeature TxFeatureImplicitFees         = "Implicit fees"
renderFeature TxFeatureValidityLowerBound   = "A validity lower bound"
renderFeature TxFeatureValidityUpperBound   = "A validity upper bound"
renderFeature TxFeatureValidityNoUpperBound = "An absent validity upper bound"
renderFeature TxFeatureTxMetadata           = "Transaction metadata"
renderFeature TxFeatureAuxScripts           = "Auxiliary scripts"
renderFeature TxFeatureWithdrawals          = "Reward account withdrawals"
renderFeature TxFeatureCertificates         = "Certificates"
renderFeature TxFeatureMintValue            = "Asset minting"
renderFeature TxFeatureMultiAssetOutputs    = "Multi-Asset outputs"
renderFeature TxFeatureScriptWitnesses      = "Script witnesses"
renderFeature TxFeatureShelleyKeys          = "Shelley keys"
renderFeature TxFeatureCollateral           = "Collateral inputs"
renderFeature TxFeatureProtocolParameters   = "Protocol parameters"
renderFeature TxFeatureTxOutDatum           = "Transaction output datums"
renderFeature TxFeatureScriptValidity       = "Script validity"
renderFeature TxFeatureExtraKeyWits         = "Required signers"
renderFeature TxFeatureInlineDatums         = "Inline datums"
renderFeature TxFeatureTotalCollateral      = "Total collateral"
renderFeature TxFeatureReferenceInputs      = "Reference inputs"
renderFeature TxFeatureReturnCollateral     = "Return collateral"

runTransactionCmd :: TransactionCmd -> ExceptT ShelleyTxCmdError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuild (AnyCardanoEra cEra) consensusModeParams@(AnyConsensusModeParams cModeParams) nid mScriptValidity mOverrideWits txins readOnlyRefIns
            reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
            mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mPparams
            mUpProp outputFormat outputOptions -> do

      inputsAndMaybeScriptWits <- readScriptWitnessFiles cEra txins
      certFilesAndMaybeScriptWits <- readScriptWitnessFiles cEra certs
      certsAndMaybeScriptWits <- sequence
                 [ fmap (,mSwit) (firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                     readFileTextEnvelope AsCertificate certFile)
                 | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
                 ]
      withdrawalsAndMaybeScriptWits <- readScriptWitnessFilesThruple cEra wdrls
      txMetadata <- validateTxMetadataInEra cEra metadataSchema metadataFiles
      valuesWithScriptWits <- readValueScriptWitnesses cEra $ maybe (mempty, []) id mValue
      txAuxScripts <- validateTxAuxScripts cEra scriptFiles
      mpparams <- case mPparams of
                  Just ppFp -> Just <$> readProtocolParametersSourceSpec ppFp
                  Nothing -> return Nothing

      mProp <- case mUpProp of
                 Just (UpdateProposalFile upFp) ->
                  Just <$> firstExceptT ShelleyTxCmdReadTextViewFileError
                             (newExceptT $ readFileTextEnvelope AsUpdateProposal upFp)
                 Nothing -> return Nothing
      requiredSigners  <- mapM readRequiredSigner reqSigners
      mReturnCollateral <- case mReturnColl of
                             Just retCol -> Just <$> toTxOutInAnyEra cEra retCol
                             Nothing -> return Nothing
      txOuts <- mapM (toTxOutInAnyEra cEra) txouts

      -- We need to construct the txBodycontent outside of runTxBuild
      BalancedTxBody txBodycontent txBody _ _
        <- runTxBuild cEra consensusModeParams nid mScriptValidity inputsAndMaybeScriptWits readOnlyRefIns txinsc
                      mReturnCollateral mTotCollateral txOuts changeAddr valuesWithScriptWits mLowBound
                      mUpperBound certsAndMaybeScriptWits withdrawalsAndMaybeScriptWits
                      requiredSigners txAuxScripts txMetadata mpparams mProp mOverrideWits outputOptions

      let allReferenceInputs = getAllReferenceInputs
                                 inputsAndMaybeScriptWits
                                 (snd valuesWithScriptWits)
                                 certsAndMaybeScriptWits
                                 withdrawalsAndMaybeScriptWits
                                 readOnlyRefIns

      let inputsThatRequireWitnessing = [input | (input,_) <- inputsAndMaybeScriptWits]
          allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ txinsc

      -- TODO: Calculating the script cost should live as a different command.
      -- Why? Because then we can simply read a txbody and figure out
      -- the script cost vs having to build the tx body each time
      case outputOptions of
        OutputScriptCostOnly fp -> do
          let BuildTxWith mTxProtocolParams = txProtocolParams txBodycontent
          case mTxProtocolParams of
            Just pparams ->
             case protocolParamPrices pparams of
               Just executionUnitPrices -> do
                 let consensusMode = consensusModeOnly cModeParams
                 case consensusMode of
                   CardanoMode -> do
                     (utxo, _, eraHistory, systemStart, _)
                       <- firstExceptT ShelleyTxCmdQueryConvenienceError
                             . newExceptT $ queryStateForBalancedTx cEra nid allTxInputs
                     case toEraInMode cEra CardanoMode of
                       Just eInMode -> do
                         scriptExecUnitsMap <- firstExceptT ShelleyTxCmdTxExecUnitsErr $ hoistEither
                                                 $ evaluateTransactionExecutionUnits
                                                     eInMode systemStart eraHistory
                                                     pparams utxo txBody
                         scriptCostOutput <- firstExceptT ShelleyTxCmdPlutusScriptCostErr $ hoistEither
                                               $ renderScriptCosts
                                                   utxo
                                                   executionUnitPrices
                                                   (collectTxBodyScriptWitnesses txBodycontent)
                                                   scriptExecUnitsMap
                         liftIO $ LBS.writeFile fp $ encodePretty scriptCostOutput
                       Nothing -> left $ ShelleyTxCmdUnsupportedMode (AnyConsensusMode consensusMode)
                   _ -> left ShelleyTxCmdPlutusScriptsRequireCardanoMode
               Nothing -> left ShelleyTxCmdPParamExecutionUnitsNotAvailable
            Nothing -> left ShelleyTxCmdProtocolParametersNotPresentInTxBody
        OutputTxBodyOnly (TxBodyFile fpath)  ->
          case outputFormat of
            OutputCliSerialisation ->
              firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
                writeFileTextEnvelope fpath Nothing txBody
            OutputLedgerCDDLSerialisation ->
              let noWitTx = makeSignedTransaction [] txBody
              in firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
                   writeTxFileTextEnvelopeCddl fpath noWitTx

    TxBuildRaw (AnyCardanoEra cEra) mScriptValidity txins readOnlyRefIns txinsc mReturnColl
               mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
               metadataSchema scriptFiles metadataFiles mpparams mUpProp outputFormat (TxBodyFile out) -> do

      inputsAndMaybeScriptWits <- readScriptWitnessFiles cEra txins
      certFilesAndMaybeScriptWits <- readScriptWitnessFiles cEra certs
      certsAndMaybeScriptWits <- sequence
                 [ fmap (,mSwit) (firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                     readFileTextEnvelope AsCertificate certFile)
                 | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
                 ]
      withdrawalsAndMaybeScriptWits <- readScriptWitnessFilesThruple cEra wdrls
      txMetadata <- validateTxMetadataInEra cEra metadataSchema metadataFiles
      valuesWithScriptWits <- readValueScriptWitnesses cEra $ maybe (mempty, []) id mValue
      txAuxScripts <- validateTxAuxScripts cEra scriptFiles
      pparams <- case mpparams of
                  Just ppFp -> Just <$> readProtocolParametersSourceSpec ppFp
                  Nothing -> return Nothing
      mProp <- case mUpProp of
                 Just (UpdateProposalFile upFp) ->
                  Just <$> firstExceptT ShelleyTxCmdReadTextViewFileError
                             (newExceptT $ readFileTextEnvelope AsUpdateProposal upFp)
                 Nothing -> return Nothing
      requiredSigners  <- mapM readRequiredSigner reqSigners
      mReturnCollateral <- case mReturnColl of
                             Just retCol -> Just <$> toTxOutInAnyEra cEra retCol
                             Nothing -> return Nothing
      txOuts <- mapM (toTxOutInAnyEra cEra) txouts

      txBody <- hoistEither $ runTxBuildRaw cEra mScriptValidity inputsAndMaybeScriptWits readOnlyRefIns txinsc
                              mReturnCollateral mTotColl txOuts mLowBound mUpperBound fee valuesWithScriptWits
                              certsAndMaybeScriptWits withdrawalsAndMaybeScriptWits requiredSigners txAuxScripts
                              txMetadata pparams mProp

      case outputFormat of
        OutputCliSerialisation ->
          firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
            getIsCardanoEraConstraint cEra $ writeFileTextEnvelope out Nothing txBody
        OutputLedgerCDDLSerialisation ->
          let noWitTx = makeSignedTransaction [] txBody
          in firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
               getIsCardanoEraConstraint cEra $ writeTxFileTextEnvelopeCddl out noWitTx

    TxSign txinfile skfiles network txoutfile ->
      runTxSign txinfile skfiles network txoutfile
    TxSubmit anyConensusModeParams network txFp ->
      runTxSubmit anyConensusModeParams network txFp
    TxCalculateMinFee txbody mnw pGenesisOrParamsFile nInputs nOutputs
                      nShelleyKeyWitnesses nByronKeyWitnesses ->
      runTxCalculateMinFee txbody mnw pGenesisOrParamsFile nInputs nOutputs
                           nShelleyKeyWitnesses nByronKeyWitnesses
    TxCalculateMinRequiredUTxO era pParamSpec txOuts -> runTxCalculateMinRequiredUTxO era pParamSpec txOuts
    TxHashScriptData scriptDataOrFile -> runTxHashScriptData scriptDataOrFile
    TxGetTxId txinfile -> runTxGetTxId txinfile
    TxView txinfile -> runTxView txinfile
    TxMintedPolicyId sFile -> runTxCreatePolicyId sFile
    TxCreateWitness txBodyfile witSignData mbNw outFile ->
      runTxCreateWitness txBodyfile witSignData mbNw outFile
    TxAssembleTxBodyWitness txBodyFile witnessFile outFile ->
      runTxSignWitness txBodyFile witnessFile outFile

-- ----------------------------------------------------------------------------
-- Building transactions
--

runTxBuildRaw
  :: CardanoEra era
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
  -- ^ TxIn with potential script witness
  -> [TxIn]
  -- ^ Read only reference inputs
  -> [TxIn]
  -- ^ TxIn for collateral
  -> Maybe (TxOut CtxTx era)
  -- ^ Return collateral
  -> Maybe Lovelace
  -- ^ Total collateral
  -> [TxOut CtxTx era]
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> Maybe SlotNo
  -- ^ Tx upper bound
  -> Maybe Lovelace
  -- ^ Tx fee
  -> (Value, [ScriptWitness WitCtxMint era])
  -- ^ Multi-Asset value(s)
  -> [(Certificate, Maybe (ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))]
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> Maybe ProtocolParameters
  -> Maybe UpdateProposal
  -> Either ShelleyTxCmdError (TxBody era)
runTxBuildRaw era
              mScriptValidity inputsAndMaybeScriptWits
              readOnlyRefIns txinsc
              mReturnCollateral mTotCollateral txouts
              mLowerBound mUpperBound
              mFee valuesWithScriptWits
              certsAndMaybeSriptWits withdrawals reqSigners
              txAuxScripts txMetadata mpparams mUpdateProp = do

    let allReferenceInputs = getAllReferenceInputs
                               inputsAndMaybeScriptWits
                               (snd valuesWithScriptWits)
                               certsAndMaybeSriptWits
                               withdrawals
                               readOnlyRefIns

    validatedCollateralTxIns <- validateTxInsCollateral era txinsc
    validatedRefInputs <- validateTxInsReference era allReferenceInputs
    validatedTotCollateral <- validateTxTotalCollateral era mTotCollateral
    validatedRetCol <- validateTxReturnCollateral era mReturnCollateral
    validatedFee <- validateTxFee era mFee
    validatedBounds <- (,) <$> validateTxValidityLowerBound era mLowerBound
                           <*> validateTxValidityUpperBound era mUpperBound
    validatedReqSigners <- validateRequiredSigners era reqSigners
    validatedPParams <- validateProtocolParameters era mpparams
    validatedTxWtdrwls <- validateTxWithdrawals era withdrawals
    validatedTxCerts <- validateTxCertificates era certsAndMaybeSriptWits
    validatedTxUpProp <- validateTxUpdateProposal era mUpdateProp
    validatedMintValue <- createTxMintValue era valuesWithScriptWits
    validatedTxScriptValidity <- validateTxScriptValidity era mScriptValidity

    let txBodyContent = TxBodyContent
                          (validateTxIns inputsAndMaybeScriptWits)
                          validatedCollateralTxIns
                          validatedRefInputs
                          txouts
                          validatedTotCollateral
                          validatedRetCol
                          validatedFee
                          validatedBounds
                          txMetadata
                          txAuxScripts
                          validatedReqSigners
                          validatedPParams
                          validatedTxWtdrwls
                          validatedTxCerts
                          validatedTxUpProp
                          validatedMintValue
                          validatedTxScriptValidity

    first ShelleyTxCmdTxBodyError $
      getIsCardanoEraConstraint era $ makeTransactionBody txBodyContent

-- txMetadata <- validateTxMetadataInEra era metadataSchema metadataFiles
runTxBuild
  :: CardanoEra era
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe ScriptValidity
  -- ^ Mark script as expected to pass or fail validation
  -> [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
  -- ^ Read only reference inputs
  -> [TxIn]
  -- ^ TxIn with potential script witness
  -> [TxIn]
  -- ^ TxIn for collateral
  -> Maybe (TxOut CtxTx era)
  -- ^ Return collateral
  -> Maybe Lovelace
  -- ^ Total collateral
  -> [TxOut CtxTx era]
  -- ^ Normal outputs
  -> TxOutChangeAddress
  -- ^ A change output
  -> (Value, [ScriptWitness WitCtxMint era])
  -- ^ Multi-Asset value(s)
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> Maybe SlotNo
  -- ^ Tx upper bound
  -> [(Certificate, Maybe (ScriptWitness WitCtxStake era))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))]
  -> [Hash PaymentKey]
  -- ^ Required signers
  -> TxAuxScripts era
  -> TxMetadataInEra era
  -> Maybe ProtocolParameters
  -> Maybe UpdateProposal
  -> Maybe Word
  -> TxBuildOutputOptions
  -> ExceptT ShelleyTxCmdError IO (BalancedTxBody era)
runTxBuild era (AnyConsensusModeParams cModeParams) networkId mScriptValidity
           inputsAndMaybeScriptWits readOnlyRefIns txinsc mReturnCollateral mTotCollateral txouts
           (TxOutChangeAddress changeAddr) valuesWithScriptWits mLowerBound mUpperBound
           certsAndMaybeScriptWits withdrawals reqSigners txAuxScripts txMetadata mpparams
           mUpdatePropF mOverrideWits outputOptions = do
  let consensusMode = consensusModeOnly cModeParams
      dummyFee = Just $ Lovelace 0
      inputsThatRequireWitnessing = [input | (input,_) <- inputsAndMaybeScriptWits]

  -- Pure
  let allReferenceInputs = getAllReferenceInputs
                             inputsAndMaybeScriptWits
                             (snd valuesWithScriptWits)
                             certsAndMaybeScriptWits
                             withdrawals readOnlyRefIns

  validatedCollateralTxIns <- hoistEither $ validateTxInsCollateral era txinsc
  validatedRefInputs <- hoistEither $ validateTxInsReference era allReferenceInputs
  validatedTotCollateral <- hoistEither $ validateTxTotalCollateral era mTotCollateral
  validatedRetCol <- hoistEither $ validateTxReturnCollateral era mReturnCollateral
  dFee <- hoistEither $ validateTxFee era dummyFee
  validatedBounds <- (,) <$> hoistEither (validateTxValidityLowerBound era mLowerBound)
                         <*> hoistEither (validateTxValidityUpperBound era mUpperBound)
  validatedReqSigners <- hoistEither $ validateRequiredSigners era reqSigners
  validatedPParams <- hoistEither $ validateProtocolParameters era mpparams
  validatedTxWtdrwls <- hoistEither $ validateTxWithdrawals era withdrawals
  validatedTxCerts <- hoistEither $ validateTxCertificates era certsAndMaybeScriptWits
  validatedTxUpProp <- hoistEither $ validateTxUpdateProposal era mUpdatePropF
  validatedMintValue <- hoistEither $ createTxMintValue era valuesWithScriptWits
  validatedTxScriptValidity <- hoistEither $ validateTxScriptValidity era mScriptValidity

  case (consensusMode, cardanoEraStyle era) of
    (CardanoMode, ShelleyBasedEra _sbe) -> do
      let txBodyContent = TxBodyContent
                            (validateTxIns inputsAndMaybeScriptWits)
                            validatedCollateralTxIns
                            validatedRefInputs
                            txouts
                            validatedTotCollateral
                            validatedRetCol
                            dFee
                            validatedBounds
                            txMetadata
                            txAuxScripts
                            validatedReqSigners
                            validatedPParams
                            validatedTxWtdrwls
                            validatedTxCerts
                            validatedTxUpProp
                            validatedMintValue
                            validatedTxScriptValidity
      eInMode <- case toEraInMode era CardanoMode of
                   Just result -> return result
                   Nothing ->
                     left (ShelleyTxCmdEraConsensusModeMismatchTxBalance outputOptions
                            (AnyConsensusMode CardanoMode) (AnyCardanoEra era))

      let allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ txinsc

      (utxo, pparams, eraHistory, systemStart, stakePools) <-
        firstExceptT ShelleyTxCmdQueryConvenienceError . newExceptT
          $ queryStateForBalancedTx era networkId allTxInputs

      firstExceptT ShelleyTxCmdTxInsDoNotExist
        . hoistEither $ txInsExistInUTxO allTxInputs utxo
      firstExceptT ShelleyTxCmdQueryNotScriptLocked
        . hoistEither $ notScriptLockedTxIns txinsc utxo

      let cAddr = case anyAddressInEra era changeAddr of
                    Just addr -> addr
                    Nothing -> error $ "runTxBuild: Byron address used: " <> show changeAddr

      balancedTxBody@(BalancedTxBody _ _ _ fee) <-
        firstExceptT ShelleyTxCmdBalanceTxBody
          . hoistEither
          $ makeTransactionBodyAutoBalance eInMode systemStart eraHistory
                                           pparams stakePools utxo txBodyContent
                                           cAddr mOverrideWits
      putStrLn $ "Estimated transaction fee: " <> (show fee :: String)

      return balancedTxBody

    (CardanoMode, LegacyByronEra) -> left ShelleyTxCmdByronEra

    (wrongMode, _) -> left (ShelleyTxCmdUnsupportedMode (AnyConsensusMode wrongMode))

-- ----------------------------------------------------------------------------
-- Transaction body validation and conversion
--

-- | An enumeration of era-dependent features where we have to check that it
-- is permissible to use this feature in this era.
--
data TxFeature = TxFeatureShelleyAddresses
               | TxFeatureExplicitFees
               | TxFeatureImplicitFees
               | TxFeatureValidityLowerBound
               | TxFeatureValidityUpperBound
               | TxFeatureValidityNoUpperBound
               | TxFeatureTxMetadata
               | TxFeatureAuxScripts
               | TxFeatureWithdrawals
               | TxFeatureCertificates
               | TxFeatureMintValue
               | TxFeatureMultiAssetOutputs
               | TxFeatureScriptWitnesses
               | TxFeatureShelleyKeys
               | TxFeatureCollateral
               | TxFeatureProtocolParameters
               | TxFeatureTxOutDatum
               | TxFeatureScriptValidity
               | TxFeatureExtraKeyWits
               | TxFeatureInlineDatums
               | TxFeatureTotalCollateral
               | TxFeatureReferenceInputs
               | TxFeatureReturnCollateral
  deriving Show

txFeatureMismatch :: CardanoEra era
                  -> TxFeature
                  -> ExceptT ShelleyTxCmdError IO a
txFeatureMismatch era feature =
    hoistEither . Left $ ShelleyTxCmdTxFeatureMismatch (anyCardanoEra era) feature

txFeatureMismatchPure :: CardanoEra era
                      -> TxFeature
                      -> Either ShelleyTxCmdError a
txFeatureMismatchPure era feature =
    Left (ShelleyTxCmdTxFeatureMismatch (anyCardanoEra era) feature)


validateTxIns
  :: [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
  -> [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
validateTxIns = map convert
 where
   convert
     :: (TxIn, Maybe (ScriptWitness WitCtxTxIn era))
     -> (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
   convert (txin, mScriptWitness) =
     case mScriptWitness of
       Just sWit ->
         (txin , BuildTxWith $ ScriptWitness ScriptWitnessForSpending sWit)
       Nothing ->
         (txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)

readScriptWitnessFiles
  :: CardanoEra era
  -> [(a, Maybe (ScriptWitnessFiles ctx))]
  -> ExceptT ShelleyTxCmdError IO [(a, Maybe (ScriptWitness ctx era))]
readScriptWitnessFiles era = mapM readSwitFile
 where
  readSwitFile (tIn, Just switFile) = do
      sWit <- readScriptWitness era switFile
      return (tIn, Just sWit)
  readSwitFile (tIn, Nothing) = return (tIn, Nothing)

readScriptWitnessFilesThruple
  :: CardanoEra era
  -> [(a, b, Maybe (ScriptWitnessFiles ctx))]
  -> ExceptT ShelleyTxCmdError IO [(a, b, Maybe (ScriptWitness ctx era))]
readScriptWitnessFilesThruple era = mapM readSwitFile
 where
  readSwitFile (tIn, b, Just switFile) = do
      sWit <- readScriptWitness era switFile
      return (tIn, b, Just sWit)
  readSwitFile (tIn, b, Nothing) = return (tIn, b, Nothing)



validateTxInsCollateral :: CardanoEra era
                        -> [TxIn]
                        -> Either ShelleyTxCmdError (TxInsCollateral era)
validateTxInsCollateral _   []    = return TxInsCollateralNone
validateTxInsCollateral era txins =
    case collateralSupportedInEra era of
      Nothing -> txFeatureMismatchPure era TxFeatureCollateral
      Just supported -> return (TxInsCollateral supported txins)

validateTxInsReference
  :: forall era. CardanoEra era
  -> [TxIn]
  -> Either ShelleyTxCmdError (TxInsReference BuildTx era)
validateTxInsReference _ []  = return TxInsReferenceNone
validateTxInsReference era allRefIns =
  case refInsScriptsAndInlineDatsSupportedInEra era of
    Nothing -> txFeatureMismatchPure era TxFeatureReferenceInputs
    Just supp -> return $ TxInsReference supp allRefIns


getAllReferenceInputs
 :: [(TxIn, Maybe (ScriptWitness WitCtxTxIn era))]
 -> [ScriptWitness WitCtxMint era]
 -> [(Certificate , Maybe (ScriptWitness WitCtxStake era))]
 -> [(StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))]
 -> [TxIn] -- ^ Read only reference inputs
 -> [TxIn]
getAllReferenceInputs txins mintWitnesses certFiles withdrawals readOnlyRefIns = do
  let txinsWitByRefInputs = [getReferenceInput sWit | (_, Just sWit) <- txins]
      mintingRefInputs = map getReferenceInput mintWitnesses
      certsWitByRefInputs = [getReferenceInput sWit | (_, Just sWit) <- certFiles]
      withdrawalsWitByRefInputs = [getReferenceInput sWit | (_, _, Just sWit) <- withdrawals]

  catMaybes $ concat [ txinsWitByRefInputs
                     , mintingRefInputs
                     , certsWitByRefInputs
                     , withdrawalsWitByRefInputs
                     , map Just readOnlyRefIns
                     ]
 where
  getReferenceInput
    :: ScriptWitness witctx era -> Maybe TxIn
  getReferenceInput sWit =
    case sWit of
      PlutusScriptWitness _ _ (PReferenceScript refIn _) _ _ _ -> Just refIn
      PlutusScriptWitness _ _ PScript{} _ _ _ -> Nothing
      SimpleScriptWitness _ _ (SReferenceScript refIn _)  -> Just refIn
      SimpleScriptWitness _ _ SScript{}  -> Nothing

toAddressInAnyEra
  :: CardanoEra era
  -> AddressAny
  -> Either ShelleyTxCmdError (AddressInEra era)
toAddressInAnyEra era addrAny =
  case addrAny of
    AddressByron   bAddr -> return (AddressInEra ByronAddressInAnyEra bAddr)
    AddressShelley sAddr ->
      case cardanoEraStyle era of
        LegacyByronEra -> txFeatureMismatchPure era TxFeatureShelleyAddresses
        ShelleyBasedEra era' ->
          return (AddressInEra (ShelleyAddressInEra era') sAddr)

toTxOutValueInAnyEra
  :: CardanoEra era
  -> Value
  -> ExceptT ShelleyTxCmdError IO (TxOutValue era)
toTxOutValueInAnyEra era val =
  case multiAssetSupportedInEra era of
    Left adaOnlyInEra ->
      case valueToLovelace val of
        Just l  -> return (TxOutAdaOnly adaOnlyInEra l)
        Nothing -> txFeatureMismatch era TxFeatureMultiAssetOutputs
    Right multiAssetInEra -> return (TxOutValue multiAssetInEra val)

toTxOutInAnyEra :: CardanoEra era
                -> TxOutAnyEra
                -> ExceptT ShelleyTxCmdError IO (TxOut CtxTx era)
toTxOutInAnyEra era (TxOutAnyEra addr' val' mDatumHash refScriptFp) = do
  addr <- hoistEither $ toAddressInAnyEra era addr'
  val <- toTxOutValueInAnyEra era val'
  (datum, refScript)
    <- case (scriptDataSupportedInEra era, refInsScriptsAndInlineDatsSupportedInEra era) of
         (Nothing, Nothing) -> pure (TxOutDatumNone, ReferenceScriptNone)
         (Just sup, Nothing)->
           (,) <$> toTxAlonzoDatum sup mDatumHash <*> pure ReferenceScriptNone
         (Just sup, Just inlineDatumRefScriptSupp) ->
           toTxDatumReferenceScriptBabbage sup inlineDatumRefScriptSupp mDatumHash refScriptFp
         (Nothing, Just _) ->
           -- TODO: Figure out how to make this state unrepresentable
           panic "toTxOutInAnyEra: Should not be possible that inline datums are allowed but datums are not"
  pure $ TxOut addr val datum refScript
 where
  getReferenceScript
    :: ReferenceScriptAnyEra
    -> ReferenceTxInsScriptsInlineDatumsSupportedInEra era
    -> ExceptT ShelleyTxCmdError IO (ReferenceScript era)
  getReferenceScript ReferenceScriptAnyEraNone _ = return ReferenceScriptNone
  getReferenceScript (ReferenceScriptAnyEra fp) supp = do
    ReferenceScript supp
      <$> firstExceptT ShelleyTxCmdScriptFileError (readFileScriptInAnyLang fp)

  toTxDatumReferenceScriptBabbage
    :: ScriptDataSupportedInEra era
    -> ReferenceTxInsScriptsInlineDatumsSupportedInEra era
    -> TxOutDatumAnyEra
    -> ReferenceScriptAnyEra
    -> ExceptT ShelleyTxCmdError IO (TxOutDatum CtxTx era, ReferenceScript era)
  toTxDatumReferenceScriptBabbage sDataSupp inlineRefSupp cliDatum refScriptFp' = do
    refScript <- getReferenceScript refScriptFp' inlineRefSupp
    case cliDatum of
       TxOutDatumByNone -> do
         pure (TxOutDatumNone, refScript)
       TxOutDatumByHashOnly dh -> do
         pure (TxOutDatumHash sDataSupp dh, refScript)
       TxOutDatumByHashOf fileOrSdata -> do
         sData <- readScriptDataOrFile fileOrSdata
         pure (TxOutDatumHash sDataSupp $ hashScriptData sData, refScript)
       TxOutDatumByValue fileOrSdata -> do
         sData <- readScriptDataOrFile fileOrSdata
         pure (TxOutDatumInTx sDataSupp sData, refScript)
       TxOutInlineDatumByValue fileOrSdata -> do
         sData <- readScriptDataOrFile fileOrSdata
         pure (TxOutDatumInline inlineRefSupp sData, refScript)

  toTxAlonzoDatum
    :: ScriptDataSupportedInEra era
    -> TxOutDatumAnyEra
    -> ExceptT ShelleyTxCmdError IO (TxOutDatum CtxTx era)
  toTxAlonzoDatum supp cliDatum =
    case cliDatum of
      TxOutDatumByHashOnly h -> pure (TxOutDatumHash supp h)
      TxOutDatumByHashOf sDataOrFile -> do
        sData <- readScriptDataOrFile sDataOrFile
        pure (TxOutDatumHash supp $ hashScriptData sData)
      TxOutDatumByValue sDataOrFile -> do
        sData <- readScriptDataOrFile sDataOrFile
        pure (TxOutDatumInTx supp sData)
      TxOutInlineDatumByValue _ ->
        txFeatureMismatch era TxFeatureInlineDatums
      TxOutDatumByNone -> pure TxOutDatumNone

validateTxFee :: CardanoEra era
              -> Maybe Lovelace
              -> Either ShelleyTxCmdError (TxFee era)
validateTxFee era mfee =
    case (txFeesExplicitInEra era, mfee) of
      (Left  implicit, Nothing)  -> return (TxFeeImplicit implicit)
      (Right explicit, Just fee) -> return (TxFeeExplicit explicit fee)

      (Right _, Nothing) -> txFeatureMismatchPure era TxFeatureImplicitFees
      (Left  _, Just _)  -> txFeatureMismatchPure era TxFeatureExplicitFees

validateTxTotalCollateral :: CardanoEra era
                          -> Maybe Lovelace
                          -> Either ShelleyTxCmdError (TxTotalCollateral era)
validateTxTotalCollateral _ Nothing = return TxTotalCollateralNone
validateTxTotalCollateral era (Just coll) =
  case totalAndReturnCollateralSupportedInEra era of
    Just supp -> return $ TxTotalCollateral supp coll
    Nothing -> txFeatureMismatchPure era TxFeatureTotalCollateral

validateTxReturnCollateral :: CardanoEra era
                           -> Maybe (TxOut CtxTx era)
                           -> Either ShelleyTxCmdError (TxReturnCollateral CtxTx era)
validateTxReturnCollateral _ Nothing = return TxReturnCollateralNone
validateTxReturnCollateral era (Just retColTxOut) = do
  case totalAndReturnCollateralSupportedInEra era of
    Just supp -> return $ TxReturnCollateral supp retColTxOut
    Nothing -> txFeatureMismatchPure era TxFeatureReturnCollateral



validateTxValidityLowerBound :: CardanoEra era
                             -> Maybe SlotNo
                             -> Either ShelleyTxCmdError  (TxValidityLowerBound era)
validateTxValidityLowerBound _ Nothing = return TxValidityNoLowerBound
validateTxValidityLowerBound era (Just slot) =
    case validityLowerBoundSupportedInEra era of
      Nothing -> txFeatureMismatchPure era TxFeatureValidityLowerBound
      Just supported -> return (TxValidityLowerBound supported slot)


validateTxValidityUpperBound :: CardanoEra era
                             -> Maybe SlotNo
                             -> Either ShelleyTxCmdError (TxValidityUpperBound era)
validateTxValidityUpperBound era Nothing =
    case validityNoUpperBoundSupportedInEra era of
      Nothing -> txFeatureMismatchPure era TxFeatureValidityNoUpperBound
      Just supported -> return (TxValidityNoUpperBound supported)
validateTxValidityUpperBound era (Just slot) =
    case validityUpperBoundSupportedInEra era of
      Nothing -> txFeatureMismatchPure era TxFeatureValidityUpperBound
      Just supported -> return (TxValidityUpperBound supported slot)


validateTxMetadataInEra :: CardanoEra era
                        -> TxMetadataJsonSchema
                        -> [MetadataFile]
                        -> ExceptT ShelleyTxCmdError IO (TxMetadataInEra era)
validateTxMetadataInEra _ _ [] = return TxMetadataNone
validateTxMetadataInEra era schema files =
    case txMetadataSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureTxMetadata
      Just supported -> do
        metadata <- mconcat <$> mapM (readFileTxMetadata schema) files
        return (TxMetadataInEra supported metadata)


validateTxAuxScripts :: CardanoEra era
                     -> [ScriptFile]
                     -> ExceptT ShelleyTxCmdError IO (TxAuxScripts era)
validateTxAuxScripts _ [] = return TxAuxScriptsNone
validateTxAuxScripts era files =
  case auxScriptsSupportedInEra era of
    Nothing -> txFeatureMismatch era TxFeatureAuxScripts
    Just supported -> do
      scripts <- sequence
        [ do script <- firstExceptT ShelleyTxCmdScriptFileError $
                         readFileScriptInAnyLang file
             validateScriptSupportedInEra era script
        | ScriptFile file <- files ]
      return $ TxAuxScripts supported scripts

validateRequiredSigners :: CardanoEra era
                        -> [Hash PaymentKey]
                        -> Either ShelleyTxCmdError (TxExtraKeyWitnesses era)
validateRequiredSigners _ [] = return TxExtraKeyWitnessesNone
validateRequiredSigners era reqSigs =
  case extraKeyWitnessesSupportedInEra era of
    Nothing -> txFeatureMismatchPure era TxFeatureExtraKeyWits
    Just supported -> return $ TxExtraKeyWitnesses supported reqSigs


validateTxWithdrawals
  :: forall era.
     CardanoEra era
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))]
  -> Either ShelleyTxCmdError (TxWithdrawals BuildTx era)
validateTxWithdrawals _ [] = return TxWithdrawalsNone
validateTxWithdrawals era withdrawals =
  case withdrawalsSupportedInEra era of
    Nothing -> txFeatureMismatchPure era TxFeatureWithdrawals
    Just supported -> do
      let convWithdrawals =map convert withdrawals
      return (TxWithdrawals supported convWithdrawals)
 where
  convert
    :: (StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))
    -> (StakeAddress, Lovelace, BuildTxWith BuildTx (Witness WitCtxStake era))
  convert (sAddr, ll, mScriptWitnessFiles) =
    case mScriptWitnessFiles of
      Just sWit -> do
        (sAddr, ll, BuildTxWith $ ScriptWitness ScriptWitnessForStakeAddr sWit)
      Nothing -> (sAddr,ll, BuildTxWith $ KeyWitness KeyWitnessForStakeAddr)

validateTxCertificates
  :: forall era.
     CardanoEra era
  -> [(Certificate, Maybe (ScriptWitness WitCtxStake era))]
  -> Either ShelleyTxCmdError (TxCertificates BuildTx era)
validateTxCertificates _ [] = return TxCertificatesNone
validateTxCertificates era certsAndScriptWitnesses =
  case certificatesSupportedInEra era of
    Nothing -> txFeatureMismatchPure era TxFeatureCertificates
    Just supported -> do
      let certs = map fst certsAndScriptWitnesses
          reqWits = Map.fromList . catMaybes $ map convert certsAndScriptWitnesses
      return $ TxCertificates supported certs $ BuildTxWith reqWits
  where
   -- We get the stake credential witness for a certificate that requires it.
   -- NB: Only stake address deregistration and delegation requires
   -- witnessing (witness can be script or key)
   deriveStakeCredentialWitness
     :: Certificate
     -> Maybe StakeCredential
   deriveStakeCredentialWitness cert = do
     case cert of
       StakeAddressDeregistrationCertificate sCred -> Just sCred
       StakeAddressDelegationCertificate sCred _ -> Just sCred
       _ -> Nothing

   convert
     :: (Certificate, Maybe (ScriptWitness WitCtxStake era))
     -> Maybe (StakeCredential, Witness WitCtxStake era)
   convert (cert, mScriptWitnessFiles) = do
     sCred <- deriveStakeCredentialWitness cert
     case mScriptWitnessFiles of
       Just sWit -> do
         Just ( sCred
              , ScriptWitness ScriptWitnessForStakeAddr sWit
              )
       Nothing -> Just (sCred, KeyWitness KeyWitnessForStakeAddr)

validateProtocolParameters
  :: CardanoEra era
  -> Maybe ProtocolParameters
  -> Either ShelleyTxCmdError (BuildTxWith BuildTx (Maybe ProtocolParameters))
validateProtocolParameters _ Nothing = return (BuildTxWith Nothing)
validateProtocolParameters era (Just pparams) =
    case scriptDataSupportedInEra era of
      Nothing -> txFeatureMismatchPure era TxFeatureProtocolParameters
      Just _  -> return . BuildTxWith $ Just pparams

validateTxUpdateProposal :: CardanoEra era
                         -> Maybe UpdateProposal
                         -> Either ShelleyTxCmdError (TxUpdateProposal era)
validateTxUpdateProposal _ Nothing = return TxUpdateProposalNone
validateTxUpdateProposal era (Just prop) =
    case updateProposalSupportedInEra era of
      Nothing -> txFeatureMismatchPure era TxFeatureCertificates
      Just supported -> return $ TxUpdateProposal supported prop

validateTxScriptValidity :: forall era.
     CardanoEra era
  -> Maybe ScriptValidity
  -> Either ShelleyTxCmdError (TxScriptValidity era)
validateTxScriptValidity _ Nothing = pure TxScriptValidityNone
validateTxScriptValidity era (Just scriptValidity) =
  case txScriptValiditySupportedInCardanoEra era of
    Nothing -> txFeatureMismatchPure era TxFeatureScriptValidity
    Just supported -> pure $ TxScriptValidity supported scriptValidity

-- TODO: Currently we specify the policyId with the '--mint' option on the cli
-- and we added a separate '--policy-id' parser that parses the policy id for the
-- given reference input (since we don't have the script in this case). To avoid asking
-- for the policy id twice (in the build command) we can potentially query the UTxO and
-- access the script (and therefore the policy id).
createTxMintValue :: forall era. CardanoEra era
                  -> (Value, [ScriptWitness WitCtxMint era])
                  -> Either ShelleyTxCmdError (TxMintValue BuildTx era)
createTxMintValue era (val, scriptWitnesses) =
  if List.null (valueToList val) && List.null scriptWitnesses
  then return TxMintNone
  else do
    case multiAssetSupportedInEra era of
      Left _ -> txFeatureMismatchPure era TxFeatureMintValue
      Right supported -> do
        -- The set of policy ids for which we need witnesses:
        let witnessesNeededSet :: Set PolicyId
            witnessesNeededSet =
              Set.fromList [ pid | (AssetId pid _, _) <- valueToList val ]

        let witnessesProvidedMap :: Map PolicyId (ScriptWitness WitCtxMint era)
            witnessesProvidedMap = Map.fromList $ gatherMintingWitnesses scriptWitnesses

            witnessesProvidedSet = Map.keysSet witnessesProvidedMap

        -- Check not too many, nor too few:
        validateAllWitnessesProvided   witnessesNeededSet witnessesProvidedSet
        validateNoUnnecessaryWitnesses witnessesNeededSet witnessesProvidedSet

        return (TxMintValue supported val (BuildTxWith witnessesProvidedMap))
 where
  gatherMintingWitnesses
    :: [ScriptWitness WitCtxMint era]
    -> [(PolicyId, ScriptWitness WitCtxMint era)]
  gatherMintingWitnesses [] = []
  gatherMintingWitnesses (sWit : rest) =
    case scriptWitnessPolicyId sWit of
      Nothing -> gatherMintingWitnesses rest
      Just pid -> (pid, sWit) : gatherMintingWitnesses rest

  validateAllWitnessesProvided witnessesNeeded witnessesProvided
    | null witnessesMissing = return ()
    | otherwise = Left (ShelleyTxCmdPolicyIdsMissing witnessesMissing)
    where
      witnessesMissing = Set.elems (witnessesNeeded Set.\\ witnessesProvided)

  validateNoUnnecessaryWitnesses witnessesNeeded witnessesProvided
    | null witnessesExtra = return ()
    | otherwise = Left (ShelleyTxCmdPolicyIdsExcess witnessesExtra)
    where
      witnessesExtra = Set.elems (witnessesProvided Set.\\ witnessesNeeded)

scriptWitnessPolicyId :: ScriptWitness witctx era -> Maybe PolicyId
scriptWitnessPolicyId (SimpleScriptWitness _ version (SScript script)) =
   Just . scriptPolicyId $ SimpleScript version script
scriptWitnessPolicyId (SimpleScriptWitness _ _ (SReferenceScript _ mPid)) =
   PolicyId <$> mPid
scriptWitnessPolicyId (PlutusScriptWitness _ version (PScript script) _ _ _) =
   Just . scriptPolicyId $ PlutusScript version script
scriptWitnessPolicyId (PlutusScriptWitness _ _ (PReferenceScript _ mPid) _ _ _) =
   PolicyId <$> mPid


readValueScriptWitnesses
  :: CardanoEra era
  -> (Value, [ScriptWitnessFiles WitCtxMint])
  -> ExceptT ShelleyTxCmdError IO (Value, [ScriptWitness WitCtxMint era])
readValueScriptWitnesses era (v, sWitFiles) = do
  sWits <- mapM (readScriptWitness era) sWitFiles
  return (v, sWits)

readScriptWitness
  :: CardanoEra era
  -> ScriptWitnessFiles witctx
  -> ExceptT ShelleyTxCmdError IO (ScriptWitness witctx era)
readScriptWitness era (SimpleScriptWitnessFile (ScriptFile scriptFile)) = do
    script@(ScriptInAnyLang lang _) <- firstExceptT ShelleyTxCmdScriptFileError $
                                         readFileScriptInAnyLang scriptFile
    ScriptInEra langInEra script'   <- validateScriptSupportedInEra era script
    case script' of
      SimpleScript version sscript ->
        return . SimpleScriptWitness
                   langInEra version $ SScript sscript

      -- If the supplied cli flags were for a simple script (i.e. the user did
      -- not supply the datum, redeemer or ex units), but the script file turns
      -- out to be a valid plutus script, then we must fail.
      PlutusScript{} ->
        left $ ShelleyTxCmdScriptExpectedSimple
                 scriptFile
                 (AnyScriptLanguage lang)

readScriptWitness era (PlutusScriptWitnessFiles
                          (ScriptFile scriptFile)
                          datumOrFile
                          redeemerOrFile
                          execUnits) = do
    script@(ScriptInAnyLang lang _) <- firstExceptT ShelleyTxCmdScriptFileError $
                                         readFileScriptInAnyLang scriptFile
    ScriptInEra langInEra script'   <- validateScriptSupportedInEra era script
    case script' of
      PlutusScript version pscript -> do
        datum    <- readScriptDatumOrFile    datumOrFile
        redeemer <- readScriptRedeemerOrFile redeemerOrFile
        return $ PlutusScriptWitness
                   langInEra version (PScript pscript)
                   datum
                   redeemer
                   execUnits

      -- If the supplied cli flags were for a plutus script (i.e. the user did
      -- supply the datum, redeemer and ex units), but the script file turns
      -- out to be a valid simple script, then we must fail.
      SimpleScript{} ->
        left $ ShelleyTxCmdScriptExpectedPlutus
                 scriptFile
                 (AnyScriptLanguage lang)

readScriptWitness era (PlutusReferenceScriptWitnessFiles refTxIn
                          anyScrLang@(AnyScriptLanguage anyScriptLanguage)
                          datumOrFile redeemerOrFile execUnits mPid) = do
  case refInsScriptsAndInlineDatsSupportedInEra era of
    Nothing -> left $ ShelleyTxCmdReferenceScriptsNotSupportedInEra
                    $ getIsCardanoEraConstraint era (AnyCardanoEra era)
    Just _ -> do

      case scriptLanguageSupportedInEra era anyScriptLanguage of
        Just sLangInEra ->
          case languageOfScriptLanguageInEra sLangInEra of
            SimpleScriptLanguage _v ->
              -- TODO: We likely need another datatype eg data ReferenceScriptWitness lang
              -- in order to make this branch unrepresentable.
              panic "readScriptWitness: Should not be possible to specify a simple script"
            PlutusScriptLanguage version -> do
              datum    <- readScriptDatumOrFile    datumOrFile
              redeemer <- readScriptRedeemerOrFile redeemerOrFile
              return $ PlutusScriptWitness
                         sLangInEra
                         version
                         (PReferenceScript refTxIn (unPolicyId <$> mPid))
                         datum redeemer execUnits
        Nothing ->
          left $ ShelleyTxCmdScriptLanguageNotSupportedInEra anyScrLang (anyCardanoEra era)
readScriptWitness era (SimpleReferenceScriptWitnessFiles refTxIn
                         anyScrLang@(AnyScriptLanguage anyScriptLanguage) mPid) = do
  case refInsScriptsAndInlineDatsSupportedInEra era of
    Nothing -> left $ ShelleyTxCmdReferenceScriptsNotSupportedInEra
                    $ getIsCardanoEraConstraint era (AnyCardanoEra era)
    Just _ -> do
      case scriptLanguageSupportedInEra era anyScriptLanguage of
        Just sLangInEra ->
          case languageOfScriptLanguageInEra sLangInEra of
            SimpleScriptLanguage v ->
              return . SimpleScriptWitness sLangInEra v
                     $ SReferenceScript refTxIn (unPolicyId <$> mPid)
            PlutusScriptLanguage{} ->
              panic "readScriptWitness: Should not be possible to specify a plutus script"
        Nothing ->
          left $ ShelleyTxCmdScriptLanguageNotSupportedInEra anyScrLang (anyCardanoEra era)

readScriptDatumOrFile :: ScriptDatumOrFile witctx
                      -> ExceptT ShelleyTxCmdError IO (ScriptDatum witctx)
readScriptDatumOrFile (ScriptDatumOrFileForTxIn df) = ScriptDatumForTxIn <$>
                                                        readScriptDataOrFile df
readScriptDatumOrFile InlineDatumPresentAtTxIn      = pure InlineScriptDatum
readScriptDatumOrFile NoScriptDatumOrFileForMint    = pure NoScriptDatumForMint
readScriptDatumOrFile NoScriptDatumOrFileForStake   = pure NoScriptDatumForStake

readScriptRedeemerOrFile :: ScriptRedeemerOrFile
                         -> ExceptT ShelleyTxCmdError IO ScriptRedeemer
readScriptRedeemerOrFile = readScriptDataOrFile

readScriptDataOrFile :: ScriptDataOrFile
                     -> ExceptT ShelleyTxCmdError IO ScriptData
readScriptDataOrFile (ScriptDataValue d) = return d
readScriptDataOrFile (ScriptDataJsonFile fp) = do
  bs <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fp) $ LBS.readFile fp
  v  <- firstExceptT (ShelleyTxCmdScriptDataJsonParseError fp)
          $ hoistEither $ Aeson.eitherDecode' bs
  sd <- firstExceptT (ShelleyTxCmdScriptDataConversionError fp)
          $ hoistEither $ scriptDataFromJson ScriptDataJsonDetailedSchema v
  firstExceptT (ShelleyTxCmdScriptDataValidationError fp)
          $ hoistEither $ validateScriptData sd
  return sd
readScriptDataOrFile (ScriptDataCborFile fp) = do
  bs <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fp)
          $ BS.readFile fp
  sd <- firstExceptT (ShelleyTxCmdMetaDecodeError fp)
          $ hoistEither $ deserialiseFromCBOR AsScriptData bs
  firstExceptT (ShelleyTxCmdScriptDataValidationError fp)
          $ hoistEither $ validateScriptData sd
  return sd

-- ----------------------------------------------------------------------------
-- Transaction signing
--

runTxSign :: InputTxBodyOrTxFile
          -> [WitnessSigningData]
          -> Maybe NetworkId
          -> TxFile
          -> ExceptT ShelleyTxCmdError IO ()
runTxSign txOrTxBody witSigningData mnw (TxFile outTxFile) = do
  sks <- firstExceptT ShelleyTxCmdReadWitnessSigningDataError $
            mapM readWitnessSigningData witSigningData

  let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeWitness sks

  case txOrTxBody of
    (InputTxFile (TxFile inputTxFile)) -> do
      anyTx <- readFileTx inputTxFile

      InAnyShelleyBasedEra _era tx <-
          onlyInShelleyBasedEras "sign for Byron era transactions" anyTx

      let (txbody, existingTxKeyWits) = getTxBodyAndWitnesses tx

      byronWitnesses <- firstExceptT ShelleyTxCmdBootstrapWitnessError
                          . hoistEither
                          $ mkShelleyBootstrapWitnesses mnw txbody sksByron

      let newShelleyKeyWits = map (makeShelleyKeyWitness txbody) sksShelley
          allKeyWits = existingTxKeyWits ++ newShelleyKeyWits ++ byronWitnesses
          signedTx = makeSignedTransaction allKeyWits txbody

      firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
        writeFileTextEnvelope outTxFile Nothing signedTx

    (InputTxBodyFile (TxBodyFile txbodyFile)) -> do
      unwitnessed <- readFileTxBody txbodyFile

      case unwitnessed of
        IncompleteCddlFormattedTx anyTx -> do
         InAnyShelleyBasedEra _era unwitTx <-
           onlyInShelleyBasedEras "sign for Byron era transactions" anyTx

         let txbody = getTxBody unwitTx
         -- Byron witnesses require the network ID. This can either be provided
         -- directly or derived from a provided Byron address.
         byronWitnesses <- firstExceptT ShelleyTxCmdBootstrapWitnessError
           . hoistEither
           $ mkShelleyBootstrapWitnesses mnw txbody sksByron

         let shelleyKeyWitnesses = map (makeShelleyKeyWitness txbody) sksShelley
             tx = makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txbody

         firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
                      writeTxFileTextEnvelopeCddl outTxFile tx

        UnwitnessedCliFormattedTxBody anyTxbody -> do
          InAnyShelleyBasedEra _era txbody <-
            --TODO: in principle we should be able to support Byron era txs too
            onlyInShelleyBasedEras "sign for Byron era transactions" anyTxbody
          -- Byron witnesses require the network ID. This can either be provided
          -- directly or derived from a provided Byron address.
          byronWitnesses <- firstExceptT ShelleyTxCmdBootstrapWitnessError
            . hoistEither
            $ mkShelleyBootstrapWitnesses mnw txbody sksByron

          let shelleyKeyWitnesses = map (makeShelleyKeyWitness txbody) sksShelley
              tx = makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txbody

          firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
            writeFileTextEnvelope outTxFile Nothing tx

-- ----------------------------------------------------------------------------
-- Transaction submission
--


runTxSubmit
  :: AnyConsensusModeParams
  -> NetworkId
  -> FilePath
  -> ExceptT ShelleyTxCmdError IO ()
runTxSubmit (AnyConsensusModeParams cModeParams) network txFile = do
    SocketPath sockPath <- firstExceptT ShelleyTxCmdSocketEnvError
                             $ newExceptT readEnvSocketPath

    InAnyCardanoEra era tx <- readFileTx txFile
    let cMode = AnyConsensusMode $ consensusModeOnly cModeParams
    eraInMode <- hoistMaybe
                   (ShelleyTxCmdEraConsensusModeMismatch (Just txFile) cMode (AnyCardanoEra era))
                   (toEraInMode era $ consensusModeOnly cModeParams)
    let txInMode = TxInMode tx eraInMode
        localNodeConnInfo = LocalNodeConnectInfo
                              { localConsensusModeParams = cModeParams
                              , localNodeNetworkId = network
                              , localNodeSocketPath = sockPath
                              }

    res <- liftIO $ submitTxToNodeLocal localNodeConnInfo txInMode
    case res of
      Net.Tx.SubmitSuccess -> liftIO $ putTextLn "Transaction successfully submitted."
      Net.Tx.SubmitFail reason ->
        case reason of
          TxValidationErrorInMode err _eraInMode -> left . ShelleyTxCmdTxSubmitError . Text.pack $ show err
          TxValidationEraMismatch mismatchErr -> left $ ShelleyTxCmdTxSubmitErrorEraMismatch mismatchErr

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTxCalculateMinFee
  :: TxBodyFile
  -> Maybe NetworkId
  -> ProtocolParamsSourceSpec
  -> TxInCount
  -> TxOutCount
  -> TxShelleyWitnessCount
  -> TxByronWitnessCount
  -> ExceptT ShelleyTxCmdError IO ()
runTxCalculateMinFee (TxBodyFile txbodyFile) nw protocolParamsSourceSpec
                     (TxInCount nInputs) (TxOutCount nOutputs)
                     (TxShelleyWitnessCount nShelleyKeyWitnesses)
                     (TxByronWitnessCount nByronKeyWitnesses) = do

    unwitnessed <- readFileTxBody txbodyFile
    pparams <- readProtocolParametersSourceSpec protocolParamsSourceSpec
    case unwitnessed of
      IncompleteCddlFormattedTx anyTx -> do
        InAnyShelleyBasedEra _era unwitTx <-
          onlyInShelleyBasedEras "sign for Byron era transactions" anyTx
        let txbody =  getTxBody unwitTx
        let tx = makeSignedTransaction [] txbody
            Lovelace fee = estimateTransactionFee
                             (fromMaybe Mainnet nw)
                             (protocolParamTxFeeFixed pparams)
                             (protocolParamTxFeePerByte pparams)
                             tx
                             nInputs nOutputs
                             nByronKeyWitnesses nShelleyKeyWitnesses

        liftIO $ putStrLn $ (show fee :: String) <> " Lovelace"

      UnwitnessedCliFormattedTxBody anyTxBody -> do
        InAnyShelleyBasedEra _era txbody <-
              --TODO: in principle we should be able to support Byron era txs too
              onlyInShelleyBasedEras "calculate-min-fee for Byron era transactions" anyTxBody

        let tx = makeSignedTransaction [] txbody
            Lovelace fee = estimateTransactionFee
                             (fromMaybe Mainnet nw)
                             (protocolParamTxFeeFixed pparams)
                             (protocolParamTxFeePerByte pparams)
                             tx
                             nInputs nOutputs
                             nByronKeyWitnesses nShelleyKeyWitnesses

        liftIO $ putStrLn $ (show fee :: String) <> " Lovelace"

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTxCalculateMinRequiredUTxO
  :: AnyCardanoEra
  -> ProtocolParamsSourceSpec
  -> TxOutAnyEra
  -> ExceptT ShelleyTxCmdError IO ()
runTxCalculateMinRequiredUTxO (AnyCardanoEra era) protocolParamsSourceSpec txOut = do
  pp <- readProtocolParametersSourceSpec protocolParamsSourceSpec
  out <- toTxOutInAnyEra era txOut
  case cardanoEraStyle era of
    LegacyByronEra -> error "runTxCalculateMinRequiredUTxO: Byron era not implemented yet"
    ShelleyBasedEra sbe -> do
      firstExceptT ShelleyTxCmdPParamsErr . hoistEither
        $ checkProtocolParameters sbe pp
      minValue <- firstExceptT ShelleyTxCmdMinimumUTxOErr
                    . hoistEither $ calculateMinimumUTxO sbe out pp
      liftIO . IO.print $ selectLovelace minValue

runTxCreatePolicyId :: ScriptFile -> ExceptT ShelleyTxCmdError IO ()
runTxCreatePolicyId (ScriptFile sFile) = do
  ScriptInAnyLang _ script <- firstExceptT ShelleyTxCmdScriptFileError $
                                readFileScriptInAnyLang sFile
  liftIO . putTextLn . serialiseToRawBytesHexText $ hashScript script

readProtocolParametersSourceSpec :: ProtocolParamsSourceSpec
                                 -> ExceptT ShelleyTxCmdError IO
                                            ProtocolParameters
readProtocolParametersSourceSpec (ParamsFromGenesis (GenesisFile f)) =
    fromShelleyPParams . sgProtocolParams <$>
      firstExceptT ShelleyTxCmdGenesisCmdError
        (readShelleyGenesisWithDefault f identity)
readProtocolParametersSourceSpec (ParamsFromFile f) =
    readProtocolParameters f

--TODO: eliminate this and get only the necessary params, and get them in a more
-- helpful way rather than requiring them as a local file.
readProtocolParameters :: ProtocolParamsFile
                       -> ExceptT ShelleyTxCmdError IO ProtocolParameters
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (ShelleyTxCmdAesonDecodeProtocolParamsError fpath . Text.pack) . hoistEither $
    Aeson.eitherDecode' pparams


-- ----------------------------------------------------------------------------
-- Witness handling
--

data SomeWitness
  = AByronSigningKey           (SigningKey ByronKey) (Maybe (Address ByronAddr))
  | APaymentSigningKey         (SigningKey PaymentKey)
  | APaymentExtendedSigningKey (SigningKey PaymentExtendedKey)
  | AStakeSigningKey           (SigningKey StakeKey)
  | AStakeExtendedSigningKey   (SigningKey StakeExtendedKey)
  | AStakePoolSigningKey       (SigningKey StakePoolKey)
  | AGenesisSigningKey         (SigningKey GenesisKey)
  | AGenesisExtendedSigningKey (SigningKey GenesisExtendedKey)
  | AGenesisDelegateSigningKey (SigningKey GenesisDelegateKey)
  | AGenesisDelegateExtendedSigningKey
                               (SigningKey GenesisDelegateExtendedKey)
  | AGenesisUTxOSigningKey     (SigningKey GenesisUTxOKey)


-- | Error reading the data required to construct a key witness.
data ReadWitnessSigningDataError
  = ReadWitnessSigningDataSigningKeyDecodeError !(FileError InputDecodeError)
  | ReadWitnessSigningDataScriptError !(FileError JsonDecodeError)
  | ReadWitnessSigningDataSigningKeyAndAddressMismatch
  -- ^ A Byron address was specified alongside a non-Byron signing key.
  deriving Show

-- | Render an error message for a 'ReadWitnessSigningDataError'.
renderReadWitnessSigningDataError :: ReadWitnessSigningDataError -> Text
renderReadWitnessSigningDataError err =
  case err of
    ReadWitnessSigningDataSigningKeyDecodeError fileErr ->
      "Error reading signing key: " <> Text.pack (displayError fileErr)
    ReadWitnessSigningDataScriptError fileErr ->
      "Error reading script: " <> Text.pack (displayError fileErr)
    ReadWitnessSigningDataSigningKeyAndAddressMismatch ->
      "Only a Byron signing key may be accompanied by a Byron address."

readWitnessSigningData
  :: WitnessSigningData
  -> ExceptT ReadWitnessSigningDataError IO SomeWitness
readWitnessSigningData (KeyWitnessSigningData skFile mbByronAddr) = do
    res <- firstExceptT ReadWitnessSigningDataSigningKeyDecodeError
      . newExceptT
      $ readSigningKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
    case (res, mbByronAddr) of
      (AByronSigningKey _ _, Just _) -> pure res
      (AByronSigningKey _ _, Nothing) -> pure res
      (_, Nothing) -> pure res
      (_, Just _) ->
        -- A Byron address should only be specified along with a Byron signing key.
        left ReadWitnessSigningDataSigningKeyAndAddressMismatch
  where
    textEnvFileTypes =
      [ FromSomeType (AsSigningKey AsByronKey)
                          (`AByronSigningKey` mbByronAddr)
      , FromSomeType (AsSigningKey AsPaymentKey)
                          APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                          AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                          AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                          AStakePoolSigningKey
      , FromSomeType (AsSigningKey AsGenesisKey)
                          AGenesisSigningKey
      , FromSomeType (AsSigningKey AsGenesisExtendedKey)
                          AGenesisExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateKey)
                          AGenesisDelegateSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey)
                          AGenesisDelegateExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisUTxOKey)
                          AGenesisUTxOSigningKey
      ]

    bech32FileTypes =
      [ FromSomeType (AsSigningKey AsPaymentKey)
                          APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                          AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                          AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                          AStakePoolSigningKey
      ]

partitionSomeWitnesses
  :: [ByronOrShelleyWitness]
  -> ( [ShelleyBootstrapWitnessSigningKeyData]
     , [ShelleyWitnessSigningKey]
     )
partitionSomeWitnesses = reversePartitionedWits . foldl' go mempty
  where
    reversePartitionedWits (bw, skw) =
      (reverse bw, reverse skw)

    go (byronAcc, shelleyKeyAcc) byronOrShelleyWit =
      case byronOrShelleyWit of
        AByronWitness byronWit ->
          (byronWit:byronAcc, shelleyKeyAcc)
        AShelleyKeyWitness shelleyKeyWit ->
          (byronAcc, shelleyKeyWit:shelleyKeyAcc)


-- | Some kind of Byron or Shelley witness.
data ByronOrShelleyWitness
  = AByronWitness !ShelleyBootstrapWitnessSigningKeyData
  | AShelleyKeyWitness !ShelleyWitnessSigningKey

categoriseSomeWitness :: SomeWitness -> ByronOrShelleyWitness
categoriseSomeWitness swsk =
  case swsk of
    AByronSigningKey           sk addr -> AByronWitness (ShelleyBootstrapWitnessSigningKeyData sk addr)
    APaymentSigningKey         sk      -> AShelleyKeyWitness (WitnessPaymentKey         sk)
    APaymentExtendedSigningKey sk      -> AShelleyKeyWitness (WitnessPaymentExtendedKey sk)
    AStakeSigningKey           sk      -> AShelleyKeyWitness (WitnessStakeKey           sk)
    AStakeExtendedSigningKey   sk      -> AShelleyKeyWitness (WitnessStakeExtendedKey   sk)
    AStakePoolSigningKey       sk      -> AShelleyKeyWitness (WitnessStakePoolKey       sk)
    AGenesisSigningKey         sk      -> AShelleyKeyWitness (WitnessGenesisKey sk)
    AGenesisExtendedSigningKey sk      -> AShelleyKeyWitness (WitnessGenesisExtendedKey sk)
    AGenesisDelegateSigningKey sk      -> AShelleyKeyWitness (WitnessGenesisDelegateKey sk)
    AGenesisDelegateExtendedSigningKey sk
                                       -> AShelleyKeyWitness (WitnessGenesisDelegateExtendedKey sk)
    AGenesisUTxOSigningKey     sk      -> AShelleyKeyWitness (WitnessGenesisUTxOKey     sk)

-- | Data required for constructing a Shelley bootstrap witness.
data ShelleyBootstrapWitnessSigningKeyData
  = ShelleyBootstrapWitnessSigningKeyData
      !(SigningKey ByronKey)
      -- ^ Byron signing key.
      !(Maybe (Address ByronAddr))
      -- ^ An optionally specified Byron address.
      --
      -- If specified, both the network ID and derivation path are extracted
      -- from the address and used in the construction of the Byron witness.

-- | Error constructing a Shelley bootstrap witness (i.e. a Byron key witness
-- in the Shelley era).
data ShelleyBootstrapWitnessError
  = MissingNetworkIdOrByronAddressError
  -- ^ Neither a network ID nor a Byron address were provided to construct the
  -- Shelley bootstrap witness. One or the other is required.
  deriving Show

-- | Render an error message for a 'ShelleyBootstrapWitnessError'.
renderShelleyBootstrapWitnessError :: ShelleyBootstrapWitnessError -> Text
renderShelleyBootstrapWitnessError MissingNetworkIdOrByronAddressError =
  "Transactions witnessed by a Byron signing key must be accompanied by a "
    <> "network ID. Either provide a network ID or provide a Byron "
    <> "address with each Byron signing key (network IDs can be derived "
    <> "from Byron addresses)."

-- | Construct a Shelley bootstrap witness (i.e. a Byron key witness in the
-- Shelley era).
mkShelleyBootstrapWitness
  :: IsShelleyBasedEra era
  => Maybe NetworkId
  -> TxBody era
  -> ShelleyBootstrapWitnessSigningKeyData
  -> Either ShelleyBootstrapWitnessError (KeyWitness era)
mkShelleyBootstrapWitness Nothing _ (ShelleyBootstrapWitnessSigningKeyData _ Nothing) =
  Left MissingNetworkIdOrByronAddressError
mkShelleyBootstrapWitness (Just nw) txBody (ShelleyBootstrapWitnessSigningKeyData skey Nothing) =
  Right $ makeShelleyBootstrapWitness (WitnessNetworkId nw) txBody skey
mkShelleyBootstrapWitness _ txBody (ShelleyBootstrapWitnessSigningKeyData skey (Just addr)) =
  Right $ makeShelleyBootstrapWitness (WitnessByronAddress addr) txBody skey

-- | Attempt to construct Shelley bootstrap witnesses until an error is
-- encountered.
mkShelleyBootstrapWitnesses
  :: IsShelleyBasedEra era
  => Maybe NetworkId
  -> TxBody era
  -> [ShelleyBootstrapWitnessSigningKeyData]
  -> Either ShelleyBootstrapWitnessError [KeyWitness era]
mkShelleyBootstrapWitnesses mnw txBody =
  mapM (mkShelleyBootstrapWitness mnw txBody)


-- ----------------------------------------------------------------------------
-- Other misc small commands
--

runTxHashScriptData :: ScriptDataOrFile -> ExceptT ShelleyTxCmdError IO ()
runTxHashScriptData scriptDataOrFile = do
    d <- readScriptDataOrFile scriptDataOrFile
    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (hashScriptData d)

runTxGetTxId :: InputTxBodyOrTxFile -> ExceptT ShelleyTxCmdError IO ()
runTxGetTxId txfile = do
    InAnyCardanoEra _era txbody <-
      case txfile of
        InputTxBodyFile (TxBodyFile txbodyFile) -> do
          unwitnessed <- readFileTxBody txbodyFile
          case unwitnessed of
            UnwitnessedCliFormattedTxBody anyTxBody -> return anyTxBody
            IncompleteCddlFormattedTx (InAnyCardanoEra era tx) ->
              return (InAnyCardanoEra era (getTxBody tx))

        InputTxFile (TxFile txFile) -> do
          InAnyCardanoEra era tx <- readFileTx txFile
          return . InAnyCardanoEra era $ getTxBody tx

    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (getTxId txbody)

runTxView :: InputTxBodyOrTxFile -> ExceptT ShelleyTxCmdError IO ()
runTxView = \case
  InputTxBodyFile (TxBodyFile txbodyFile) -> do
    unwitnessed <- readFileTxBody txbodyFile
    InAnyCardanoEra era txbody <-
      case unwitnessed of
        UnwitnessedCliFormattedTxBody anyTxBody -> pure anyTxBody
        IncompleteCddlFormattedTx (InAnyCardanoEra era tx) ->
          pure $ InAnyCardanoEra era (getTxBody tx)
    --TODO: Why are we maintaining friendlyTxBodyBS and friendlyTxBS?
    -- In the case of a transaction body, we can simply call makeSignedTransaction []
    -- to get a transaction which allows us to reuse friendlyTxBS!
    liftIO $ BS.putStr $ friendlyTxBodyBS era txbody
  InputTxFile (TxFile txFile) -> do
    InAnyCardanoEra era tx <- readFileTx txFile
    liftIO $ BS.putStr $ friendlyTxBS era tx


-- ----------------------------------------------------------------------------
-- Witness commands
--

runTxCreateWitness
  :: TxBodyFile
  -> WitnessSigningData
  -> Maybe NetworkId
  -> OutputFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxCreateWitness (TxBodyFile txbodyFile) witSignData mbNw (OutputFile oFile) = do
  unwitnessed <- readFileTxBody txbodyFile
  case unwitnessed of
    IncompleteCddlFormattedTx anyTx -> do
     InAnyShelleyBasedEra sbe cddlTx <-
       onlyInShelleyBasedEras "sign for Byron era transactions" anyTx

     let txbody = getTxBody cddlTx
     someWit <- firstExceptT ShelleyTxCmdReadWitnessSigningDataError
        $ readWitnessSigningData witSignData

     witness <-
       case categoriseSomeWitness someWit of
         -- Byron witnesses require the network ID. This can either be provided
         -- directly or derived from a provided Byron address.
         AByronWitness bootstrapWitData ->
           firstExceptT ShelleyTxCmdBootstrapWitnessError
             . hoistEither
             $ mkShelleyBootstrapWitness mbNw txbody bootstrapWitData
         AShelleyKeyWitness skShelley ->
           pure $ makeShelleyKeyWitness txbody skShelley

     firstExceptT ShelleyTxCmdWriteFileError . newExceptT
       $ writeTxWitnessFileTextEnvelopeCddl sbe oFile witness

    UnwitnessedCliFormattedTxBody anyTxbody -> do
      InAnyShelleyBasedEra _era txbody <-
        onlyInShelleyBasedEras "sign for Byron era transactions" anyTxbody

      someWit <- firstExceptT ShelleyTxCmdReadWitnessSigningDataError
                   $ readWitnessSigningData witSignData

      witness <-
        case categoriseSomeWitness someWit of
          -- Byron witnesses require the network ID. This can either be provided
          -- directly or derived from a provided Byron address.
          AByronWitness bootstrapWitData ->
            firstExceptT ShelleyTxCmdBootstrapWitnessError
              . hoistEither
              $ mkShelleyBootstrapWitness mbNw txbody bootstrapWitData
          AShelleyKeyWitness skShelley ->
            pure $ makeShelleyKeyWitness txbody skShelley

      firstExceptT ShelleyTxCmdWriteFileError . newExceptT
        $ writeFileTextEnvelope oFile Nothing witness

runTxSignWitness
  :: TxBodyFile
  -> [WitnessFile]
  -> OutputFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxSignWitness (TxBodyFile txbodyFile) witnessFiles (OutputFile oFp) = do
    unwitnessed <- readFileTxBody txbodyFile
    case unwitnessed of
      UnwitnessedCliFormattedTxBody (InAnyCardanoEra era txbody) -> do
        witnesses <-
          sequence
            [ do InAnyCardanoEra era' witness <- readFileWitness file
                 case testEquality era era' of
                   Nothing   -> left $ ShelleyTxCmdWitnessEraMismatch
                                         (AnyCardanoEra era)
                                         (AnyCardanoEra era')
                                         witnessFile
                   Just Refl -> return witness
            | witnessFile@(WitnessFile file) <- witnessFiles
            ]

        let tx = makeSignedTransaction witnesses txbody
        firstExceptT ShelleyTxCmdWriteFileError
          . newExceptT
          $ writeFileTextEnvelope oFp Nothing tx

      IncompleteCddlFormattedTx (InAnyCardanoEra era anyTx) -> do
        let txbody = getTxBody anyTx

        witnesses <-
          sequence
            [ do InAnyCardanoEra era' witness <- readFileWitness file
                 case testEquality era era' of
                   Nothing   -> left $ ShelleyTxCmdWitnessEraMismatch
                                         (AnyCardanoEra era)
                                         (AnyCardanoEra era')
                                         witnessFile
                   Just Refl -> return witness
            | witnessFile@(WitnessFile file) <- witnessFiles ]

        let tx = makeSignedTransaction witnesses txbody

        firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
          writeTxFileTextEnvelopeCddl oFp tx


-- ----------------------------------------------------------------------------
-- Reading files in any era
--

-- TODO: This is a stop gap to avoid modifying the TextEnvelope
-- related functions. We intend to remove this after fully deprecating
-- the cli's serialisation format
acceptKeyWitnessCDDLSerialisation
  :: ShelleyTxCmdError
  -> ExceptT ShelleyTxCmdError IO CddlWitness
acceptKeyWitnessCDDLSerialisation err =
  case err of
    ShelleyTxCmdReadTextViewFileError e@(FileError fp (TextEnvelopeDecodeError _)) ->
      readCddlWitness e fp

    ShelleyTxCmdReadTextViewFileError e@(FileError fp (TextEnvelopeAesonDecodeError _)) ->
      readCddlWitness e fp

    ShelleyTxCmdReadTextViewFileError e@(FileError fp (TextEnvelopeTypeError _ _)) ->
      readCddlWitness e fp

    _ -> left err
 where
  readCddlWitness
    :: FileError TextEnvelopeError
    -> FilePath
    -> ExceptT ShelleyTxCmdError IO CddlWitness
  readCddlWitness tEnvErr fp = do
    firstExceptT (ShelleyTxCmdTextEnvCddlError tEnvErr)
          $ newExceptT $ readFileTextEnvelopeCddlAnyOf teTypes fp

  teTypes = [ FromCDDLWitness "TxWitness Shelley" CddlWitness
            , FromCDDLWitness "TxWitness AllegraEra" CddlWitness
            , FromCDDLWitness "TxWitness MaryEra" CddlWitness
            , FromCDDLWitness "TxWitness AlonzoEra" CddlWitness
            , FromCDDLWitness "TxWitness BabbageEra" CddlWitness
            ]

newtype CddlWitness = CddlWitness { unCddlWitness :: InAnyCardanoEra KeyWitness}

readFileWitness :: FilePath
                -> ExceptT ShelleyTxCmdError IO (InAnyCardanoEra KeyWitness)
readFileWitness fp =
  handleLeftT
    (\e -> unCddlWitness <$> acceptKeyWitnessCDDLSerialisation e)
    (readFileInAnyCardanoEra AsKeyWitness fp)

-- IncompleteCddlFormattedTx is an CDDL formatted tx or partial tx
-- (respectively needs additional witnesses or totally unwitnessed)
-- while UnwitnessedCliFormattedTxBody is CLI formatted TxBody and
-- needs to be key witnessed.
data IncompleteTx
  = UnwitnessedCliFormattedTxBody (InAnyCardanoEra TxBody)
  | IncompleteCddlFormattedTx (InAnyCardanoEra Tx)

readFileTxBody :: FilePath -> ExceptT ShelleyTxCmdError IO IncompleteTx
readFileTxBody fp =
  handleLeftT
    (\e -> IncompleteCddlFormattedTx . unCddlTx <$> acceptTxCDDLSerialisation e)
    (UnwitnessedCliFormattedTxBody <$> readFileInAnyCardanoEra AsTxBody fp)

acceptTxCDDLSerialisation
  :: ShelleyTxCmdError
  -> ExceptT ShelleyTxCmdError IO CddlTx
acceptTxCDDLSerialisation err =
  case err of
    ShelleyTxCmdReadTextViewFileError e@(FileError fp (TextEnvelopeDecodeError _)) ->
      firstExceptT (ShelleyTxCmdTextEnvCddlError e)
                          $ newExceptT $ readFileTextEnvelopeCddlAnyOf teTypes fp


    ShelleyTxCmdReadTextViewFileError e@(FileError fp (TextEnvelopeAesonDecodeError _)) ->
      firstExceptT (ShelleyTxCmdTextEnvCddlError e)
                     $ newExceptT $ readFileTextEnvelopeCddlAnyOf teTypes fp
    ShelleyTxCmdReadTextViewFileError e@(FileError fp (TextEnvelopeTypeError _ _)) ->
      firstExceptT (ShelleyTxCmdTextEnvCddlError e)
                     $ newExceptT $ readFileTextEnvelopeCddlAnyOf teTypes fp

    _ -> left err
 where
  teTypes = [ FromCDDLTx "Witnessed Tx ByronEra" CddlTx
            , FromCDDLTx "Witnessed Tx ShelleyEra" CddlTx
            , FromCDDLTx "Witnessed Tx AllegraEra" CddlTx
            , FromCDDLTx "Witnessed Tx MaryEra" CddlTx
            , FromCDDLTx "Witnessed Tx AlonzoEra" CddlTx
            , FromCDDLTx "Witnessed Tx BabbageEra" CddlTx
            , FromCDDLTx "Unwitnessed Tx ByronEra" CddlTx
            , FromCDDLTx "Unwitnessed Tx ShelleyEra" CddlTx
            , FromCDDLTx "Unwitnessed Tx AllegraEra" CddlTx
            , FromCDDLTx "Unwitnessed Tx MaryEra" CddlTx
            , FromCDDLTx "Unwitnessed Tx AlonzoEra" CddlTx
            , FromCDDLTx "Unwitnessed Tx BabbageEra" CddlTx
            ]

readFileTx :: FilePath -> ExceptT ShelleyTxCmdError IO (InAnyCardanoEra Tx)
readFileTx fp =
  handleLeftT
    (\e -> unCddlTx <$> acceptTxCDDLSerialisation e)
    (readFileInAnyCardanoEra AsTx fp)


readFileInAnyCardanoEra
  :: ( HasTextEnvelope (thing ByronEra)
     , HasTextEnvelope (thing ShelleyEra)
     , HasTextEnvelope (thing AllegraEra)
     , HasTextEnvelope (thing MaryEra)
     , HasTextEnvelope (thing AlonzoEra)
     , HasTextEnvelope (thing BabbageEra)
     )
  => (forall era. AsType era -> AsType (thing era))
  -> FilePath
  -> ExceptT ShelleyTxCmdError IO
            (InAnyCardanoEra thing)
readFileInAnyCardanoEra asThing file =
    firstExceptT ShelleyTxCmdReadTextViewFileError
  . newExceptT
  $ readFileTextEnvelopeAnyOf
      [ FromSomeType (asThing AsByronEra)   (InAnyCardanoEra ByronEra)
      , FromSomeType (asThing AsShelleyEra) (InAnyCardanoEra ShelleyEra)
      , FromSomeType (asThing AsAllegraEra) (InAnyCardanoEra AllegraEra)
      , FromSomeType (asThing AsMaryEra)    (InAnyCardanoEra MaryEra)
      , FromSomeType (asThing AsAlonzoEra)  (InAnyCardanoEra AlonzoEra)
      , FromSomeType (asThing AsBabbageEra) (InAnyCardanoEra BabbageEra)
      ]
      file

readRequiredSigner :: RequiredSigner -> ExceptT ShelleyTxCmdError IO (Hash PaymentKey)
readRequiredSigner (RequiredSignerHash h) = return h
readRequiredSigner (RequiredSignerSkeyFile skFile) = do
  keyWit <- firstExceptT ShelleyTxCmdReadRequiredSignerError
              . newExceptT
              $ readSigningKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
  case categoriseSomeWitness keyWit of
    AByronWitness _ ->
      left $ ShelleyTxCmdRequiredSignerByronKeyError skFile
    AShelleyKeyWitness skey ->
      return . getHash $ toShelleySigningKey skey
 where
   textEnvFileTypes =
     [ FromSomeType (AsSigningKey AsPaymentKey) APaymentSigningKey
     , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
     ]
   bech32FileTypes = []

   getHash :: ShelleySigningKey -> Hash PaymentKey
   getHash (ShelleyExtendedSigningKey sk) =
     let extSKey = PaymentExtendedSigningKey sk
         payVKey = castVerificationKey $ getVerificationKey extSKey
     in verificationKeyHash payVKey
   getHash (ShelleyNormalSigningKey sk) =
     verificationKeyHash . getVerificationKey $ PaymentSigningKey sk

-- | Constrain the era to be Shelley based. Fail for the Byron era.
--
onlyInShelleyBasedEras :: Text
                       -> InAnyCardanoEra a
                       -> ExceptT ShelleyTxCmdError IO
                                  (InAnyShelleyBasedEra a)
onlyInShelleyBasedEras notImplMsg (InAnyCardanoEra era x) =
    case cardanoEraStyle era of
      LegacyByronEra       -> left (ShelleyTxCmdNotImplemented notImplMsg)
      ShelleyBasedEra era' -> return (InAnyShelleyBasedEra era' x)


-- ----------------------------------------------------------------------------
-- Reading other files
--

validateScriptSupportedInEra :: CardanoEra era
                             -> ScriptInAnyLang
                             -> ExceptT ShelleyTxCmdError IO (ScriptInEra era)
validateScriptSupportedInEra era script@(ScriptInAnyLang lang _) =
    case toScriptInEra era script of
      Nothing -> left $ ShelleyTxCmdScriptLanguageNotSupportedInEra
                          (AnyScriptLanguage lang) (anyCardanoEra era)
      Just script' -> pure script'


-- ----------------------------------------------------------------------------
-- Transaction metadata
--

readFileTxMetadata :: TxMetadataJsonSchema -> MetadataFile
                   -> ExceptT ShelleyTxCmdError IO TxMetadata
readFileTxMetadata mapping (MetadataFileJSON fp) = do
    bs <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fp) $
          LBS.readFile fp
    v  <- firstExceptT (ShelleyTxCmdMetadataJsonParseError fp) $
          hoistEither $
            Aeson.eitherDecode' bs
    txMetadata <- firstExceptT (ShelleyTxCmdMetadataConversionError fp) $ hoistEither $
      metadataFromJson mapping v
    firstExceptT (ShelleyTxCmdMetaValidationError fp) $ hoistEither $ do
        validateTxMetadata txMetadata
        return txMetadata

readFileTxMetadata _ (MetadataFileCBOR fp) = do
    bs <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fp) $
          BS.readFile fp
    txMetadata <- firstExceptT (ShelleyTxCmdMetaDecodeError fp) $ hoistEither $
      deserialiseFromCBOR AsTxMetadata bs
    firstExceptT (ShelleyTxCmdMetaValidationError fp) $ hoistEither $ do
        validateTxMetadata txMetadata
        return txMetadata

