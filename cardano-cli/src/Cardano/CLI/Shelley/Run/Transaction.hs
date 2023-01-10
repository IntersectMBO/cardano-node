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

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, hoistMaybe, left,
                   newExceptT)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Data ((:~:) (..))
import           Data.Foldable (Foldable (..))
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Type.Equality (TestEquality (..))
import qualified System.IO as IO

import           Cardano.Api
import           Cardano.Api.Byron hiding (SomeByronSigningKey (..))
import           Cardano.Api.Shelley

import           Cardano.CLI.Helpers (printWarning)
import           Cardano.CLI.Run.Friendly (friendlyTxBS, friendlyTxBodyBS)
import           Cardano.CLI.Shelley.Output
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Shelley.Run.Genesis
import           Cardano.CLI.Shelley.Run.Read
import           Cardano.CLI.Shelley.Run.Validate
import           Cardano.CLI.Types

import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

{- HLINT ignore "Use let" -}

data ShelleyTxCmdError
  = ShelleyTxCmdMetadataError MetadataError
  | ShelleyTxCmdScriptWitnessError ScriptWitnessError
  | ShelleyTxCmdProtocolParamsError ProtocolParamsError
  | ShelleyTxCmdScriptFileError (FileError ScriptDecodeError)
  | ShelleyTxCmdReadTextViewFileError !(FileError TextEnvelopeError)
  | ShelleyTxCmdReadWitnessSigningDataError !ReadWitnessSigningDataError
  | ShelleyTxCmdRequiredSignerByronKeyError !SigningKeyFile
  | ShelleyTxCmdWriteFileError !(FileError ())
  | ShelleyTxCmdEraConsensusModeMismatch
      !(Maybe FilePath)
      !AnyConsensusMode
      !AnyCardanoEra
      -- ^ Era
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
  | ShelleyTxCmdQueryNotScriptLocked !ScriptLockedTxInsError
  | ShelleyTxCmdScriptDataError !ScriptDataError
  | ShelleyTxCmdCddlError CddlError
  | ShelleyTxCmdCddlWitnessError CddlWitnessError
  | ShelleyTxCmdRequiredSignerError RequiredSignerError
  -- Validation errors
  | ShelleyTxCmdAuxScriptsValidationError TxAuxScriptsValidationError
  | ShelleyTxCmdTotalCollateralValidationError TxTotalCollateralValidationError
  | ShelleyTxCmdReturnCollateralValidationError TxReturnCollateralValidationError
  | ShelleyTxCmdTxFeeValidationError TxFeeValidationError
  | ShelleyTxCmdTxValidityLowerBoundValidationError TxValidityLowerBoundValidationError
  | ShelleyTxCmdTxValidityUpperBoundValidationError TxValidityUpperBoundValidationError
  | ShelleyTxCmdRequiredSignersValidationError TxRequiredSignersValidationError
  | ShelleyTxCmdProtocolParametersValidationError TxProtocolParametersValidationError
  | ShelleyTxCmdTxWithdrawalsValidationError TxWithdrawalsValidationError
  | ShelleyTxCmdTxCertificatesValidationError TxCertificatesValidationError
  | ShelleyTxCmdTxUpdateProposalValidationError TxUpdateProposalValidationError
  | ShelleyTxCmdScriptValidityValidationError TxScriptValidityValidationError

renderShelleyTxCmdError :: ShelleyTxCmdError -> Text
renderShelleyTxCmdError err =
  case err of
    ShelleyTxCmdReadTextViewFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdScriptFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdReadWitnessSigningDataError witSignDataErr ->
      renderReadWitnessSigningDataError witSignDataErr
    ShelleyTxCmdRequiredSignerByronKeyError (SigningKeyFile fp) ->
      "Byron key witness was used as a required signer: " <> textShow fp
    ShelleyTxCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdSocketEnvError envSockErr -> renderEnvSocketError envSockErr
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
      "witness in " <> textShow file <> " is for the " <> renderEra era' <> " era."

    ShelleyTxCmdScriptLanguageNotSupportedInEra (AnyScriptLanguage lang) era ->
      "The script language " <> textShow lang <> " is not supported in the " <>
      renderEra era <> " era."

    ShelleyTxCmdEraConsensusModeMismatch fp mode era ->
       "Submitting " <> renderEra era <> " era transaction (" <> textShow fp <>
       ") is not supported in the " <> renderMode mode <> " consensus mode."
    ShelleyTxCmdPolicyIdsMissing policyids -> mconcat
      [ "The \"--mint\" flag specifies an asset with a policy Id, but no "
      , "corresponding monetary policy script has been provided as a witness "
      , "(via the \"--mint-script-file\" flag). The policy Id in question is: "
      , Text.intercalate ", " (map serialiseToRawBytesHexText policyids)
      ]

    ShelleyTxCmdPolicyIdsExcess policyids -> mconcat
      [ "A script provided to witness minting does not correspond to the policy "
      , "id of any asset specified in the \"--mint\" field. The script hash is: "
      , Text.intercalate ", " (map serialiseToRawBytesHexText policyids)
      ]
    ShelleyTxCmdUnsupportedMode mode -> "Unsupported mode: " <> renderMode mode
    ShelleyTxCmdByronEra -> "This query cannot be used for the Byron era"
    ShelleyTxCmdEraConsensusModeMismatchTxBalance fp mode era ->
       "Cannot balance " <> renderEra era <> " era transaction body (" <> textShow fp <>
       ") because is not supported in the " <> renderMode mode <> " consensus mode."
    ShelleyTxCmdBalanceTxBody err' -> Text.pack $ displayError err'
    ShelleyTxCmdTxInsDoNotExist e ->
      renderTxInsExistError e
    ShelleyTxCmdMinimumUTxOErr err' -> Text.pack $ displayError err'
    ShelleyTxCmdPParamsErr err' -> Text.pack $ displayError err'
    ShelleyTxCmdTextEnvCddlError textEnvErr cddlErr -> mconcat
      [ "Failed to decode neither the cli's serialisation format nor the ledger's "
      , "CDDL serialisation format. TextEnvelope error: " <> Text.pack (displayError textEnvErr) <> "\n"
      , "TextEnvelopeCddl error: " <> Text.pack (displayError cddlErr)
      ]
    ShelleyTxCmdTxExecUnitsErr err' ->  Text.pack $ displayError err'
    ShelleyTxCmdPlutusScriptCostErr err'-> Text.pack $ displayError err'
    ShelleyTxCmdPParamExecutionUnitsNotAvailable -> mconcat
      [ "Execution units not available in the protocol parameters. This is "
      , "likely due to not being in the Alonzo era"
      ]
    ShelleyTxCmdReferenceScriptsNotSupportedInEra (AnyCardanoEra era) ->
      "TxCmd: Reference scripts not supported in era: " <> textShow era
    ShelleyTxCmdTxEraCastErr (EraCastError value fromEra toEra) ->
      "Unable to cast era from " <> textShow fromEra <> " to " <> textShow toEra <> " the value " <> textShow value
    ShelleyTxCmdQueryConvenienceError e ->
      renderQueryConvenienceError e
    ShelleyTxCmdQueryNotScriptLocked e ->
      renderNotScriptLockedTxInsError e
    ShelleyTxCmdPlutusScriptsRequireCardanoMode ->
      "Plutus scripts are only available in CardanoMode"
    ShelleyTxCmdProtocolParametersNotPresentInTxBody ->
      "Protocol parameters were not found in transaction body"
    ShelleyTxCmdMetadataError e -> renderMetadataError e
    ShelleyTxCmdScriptWitnessError e -> renderScriptWitnessError e
    ShelleyTxCmdScriptDataError e -> renderScriptDataError e
    ShelleyTxCmdProtocolParamsError e -> renderProtocolParamsError e
    ShelleyTxCmdCddlError _ -> error "TODO"
    ShelleyTxCmdCddlWitnessError _ -> error "TODO"
    ShelleyTxCmdRequiredSignerError _ -> error ""
    -- Validation errors
    ShelleyTxCmdAuxScriptsValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTotalCollateralValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdReturnCollateralValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTxFeeValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTxValidityLowerBoundValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTxValidityUpperBoundValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdRequiredSignersValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdProtocolParametersValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTxWithdrawalsValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTxCertificatesValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdTxUpdateProposalValidationError e ->
      Text.pack $ displayError e
    ShelleyTxCmdScriptValidityValidationError e ->
      Text.pack $ displayError e

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
    TxBuild era consensusModeParams nid mScriptValidity mOverrideWits txins readOnlyRefIns
            reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
            mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mPparams
            mUpProp outputOptions -> do
      runTxBuildCmd era consensusModeParams nid mScriptValidity mOverrideWits txins readOnlyRefIns
            reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
            mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mPparams
            mUpProp outputOptions
    TxBuildRaw era mScriptValidity txins readOnlyRefIns txinsc mReturnColl
               mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
               metadataSchema scriptFiles metadataFiles mpparams mUpProp out -> do
      runTxBuildRawCmd era mScriptValidity txins readOnlyRefIns txinsc mReturnColl
               mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
               metadataSchema scriptFiles metadataFiles mpparams mUpProp out
    TxSign txinfile skfiles network txoutfile ->
      runTxSign txinfile skfiles network txoutfile
    TxSubmit anyConsensusModeParams network txFp ->
      runTxSubmit anyConsensusModeParams network txFp
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

runTxBuildCmd
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> Maybe ScriptValidity
  -> Maybe Word -- ^ Override the required number of tx witnesses
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))] -- ^ Transaction inputs with optional spending scripts
  -> [TxIn] -- ^ Read only reference inputs
  -> [RequiredSigner] -- ^ Required signers
  -> [TxIn] -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
  -> Maybe TxOutAnyEra -- ^ Return collateral
  -> Maybe Lovelace -- ^ Total collateral
  -> [TxOutAnyEra]
  -> TxOutChangeAddress
  -> Maybe (Value, [ScriptWitnessFiles WitCtxMint])
  -> Maybe SlotNo -- ^ Validity lower bound
  -> Maybe SlotNo -- ^ Validity upper bound
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))] -- ^ Withdrawals with potential script witness
  -> TxMetadataJsonSchema
  -> [ScriptFile]
  -> [MetadataFile]
  -> Maybe ProtocolParamsSourceSpec
  -> Maybe UpdateProposalFile
  -> TxBuildOutputOptions
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuildCmd
  (AnyCardanoEra cEra) consensusModeParams@(AnyConsensusModeParams cModeParams) nid mScriptValidity mOverrideWits txins readOnlyRefIns
  reqSigners txinsc mReturnColl mTotCollateral txouts changeAddr mValue mLowBound
  mUpperBound certs wdrls metadataSchema scriptFiles metadataFiles mPparams mUpProp outputOptions = do
  -- The user can specify an era prior to the era that the node is currently in.
  -- We cannot use the user specified era to construct a query against a node because it may differ
  -- from the node's era and this will result in the 'QueryEraMismatch' failure.

  SocketPath sockPath <- firstExceptT ShelleyTxCmdSocketEnvError
                           $ newExceptT readEnvSocketPath

  let localNodeConnInfo = LocalNodeConnectInfo
                            { localConsensusModeParams = cModeParams
                            , localNodeNetworkId = nid
                            , localNodeSocketPath = sockPath
                            }

  AnyCardanoEra nodeEra
    <- firstExceptT (ShelleyTxCmdQueryConvenienceError . AcqFailure)
         . newExceptT $ determineEra cModeParams localNodeConnInfo

  inputsAndMaybeScriptWits <- firstExceptT ShelleyTxCmdScriptWitnessError $ readScriptWitnessFiles cEra txins
  certFilesAndMaybeScriptWits <- firstExceptT ShelleyTxCmdScriptWitnessError $ readScriptWitnessFiles cEra certs
  certsAndMaybeScriptWits <- sequence
             [ fmap (,mSwit) (firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                 readFileTextEnvelope AsCertificate certFile)
             | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
             ]
  withdrawalsAndMaybeScriptWits <- firstExceptT ShelleyTxCmdScriptWitnessError
                                     $ readScriptWitnessFilesThruple cEra wdrls
  txMetadata <- firstExceptT ShelleyTxCmdMetadataError
                  . newExceptT $ readTxMetadata cEra metadataSchema metadataFiles
  valuesWithScriptWits <- readValueScriptWitnesses cEra $ fromMaybe (mempty, []) mValue
  scripts <- firstExceptT ShelleyTxCmdScriptFileError $
                     mapM (readFileScriptInAnyLang . unScriptFile) scriptFiles
  txAuxScripts <- hoistEither $ first ShelleyTxCmdAuxScriptsValidationError $ validateTxAuxScripts cEra scripts
  mpparams <- case mPparams of
                Just ppFp -> Just <$> firstExceptT ShelleyTxCmdProtocolParamsError (readProtocolParametersSourceSpec ppFp)
                Nothing -> return Nothing

  mProp <- case mUpProp of
             Just (UpdateProposalFile upFp) ->
              Just <$> firstExceptT ShelleyTxCmdReadTextViewFileError
                         (newExceptT $ readFileTextEnvelope AsUpdateProposal upFp)
             Nothing -> return Nothing
  requiredSigners  <- mapM (firstExceptT ShelleyTxCmdRequiredSignerError .  newExceptT . readRequiredSigner) reqSigners
  mReturnCollateral <- case mReturnColl of
                         Just retCol -> Just <$> toTxOutInAnyEra cEra retCol
                         Nothing -> return Nothing

  txOuts <- mapM (toTxOutInAnyEra cEra) txouts

  -- the same collateral input can be used for several plutus scripts
  let filteredTxinsc = Set.toList $ Set.fromList txinsc

  -- We need to construct the txBodycontent outside of runTxBuild
  BalancedTxBody txBodycontent balancedTxBody _ _
    <- runTxBuild cEra consensusModeParams nid mScriptValidity inputsAndMaybeScriptWits readOnlyRefIns filteredTxinsc
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
      allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ filteredTxinsc

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
                 (nodeEraUTxO, _, eraHistory, systemStart, _)
                   <- firstExceptT ShelleyTxCmdQueryConvenienceError
                         . newExceptT $ queryStateForBalancedTx nodeEra nid allTxInputs
                 case toEraInMode cEra CardanoMode of
                   Just eInMode -> do
                     -- Why do we cast the era? The user can specify an era prior to the era that the node is currently in.
                     -- We cannot use the user specified era to construct a query against a node because it may differ
                     -- from the node's era and this will result in the 'QueryEraMismatch' failure.
                     txEraUtxo <- case first ShelleyTxCmdTxEraCastErr (eraCast cEra nodeEraUTxO) of
                                    Right txEraUtxo -> return txEraUtxo
                                    Left e -> left e

                     scriptExecUnitsMap <- firstExceptT ShelleyTxCmdTxExecUnitsErr $ hoistEither
                                             $ evaluateTransactionExecutionUnits
                                                 eInMode systemStart (toLedgerEpochInfo eraHistory)
                                                 pparams txEraUtxo balancedTxBody
                     scriptCostOutput <- firstExceptT ShelleyTxCmdPlutusScriptCostErr $ hoistEither
                                           $ renderScriptCosts
                                               txEraUtxo
                                               executionUnitPrices
                                               (collectTxBodyScriptWitnesses txBodycontent)
                                               scriptExecUnitsMap
                     liftIO $ LBS.writeFile fp $ encodePretty scriptCostOutput
                   Nothing -> left $ ShelleyTxCmdUnsupportedMode (AnyConsensusMode consensusMode)
               _ -> left ShelleyTxCmdPlutusScriptsRequireCardanoMode
           Nothing -> left ShelleyTxCmdPParamExecutionUnitsNotAvailable
        Nothing -> left ShelleyTxCmdProtocolParametersNotPresentInTxBody
    OutputTxBodyOnly (TxBodyFile fpath) ->
      let noWitTx = makeSignedTransaction [] balancedTxBody
      in firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
           writeTxFileTextEnvelopeCddl fpath noWitTx

runTxBuildRawCmd
  :: AnyCardanoEra
  -> Maybe ScriptValidity
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -> [TxIn] -- ^ Read only reference inputs
  -> [TxIn] -- ^ Transaction inputs for collateral, only key witnesses, no scripts.
  -> Maybe TxOutAnyEra
  -> Maybe Lovelace -- ^ Total collateral
  -> [RequiredSigner]
  -> [TxOutAnyEra]
  -> Maybe (Value, [ScriptWitnessFiles WitCtxMint]) -- ^ Multi-Asset value with script witness
  -> Maybe SlotNo -- ^ Validity lower bound
  -> Maybe SlotNo -- ^ Validity upper bound
  -> Maybe Lovelace -- ^ Tx fee
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> TxMetadataJsonSchema
  -> [ScriptFile]
  -> [MetadataFile]
  -> Maybe ProtocolParamsSourceSpec
  -> Maybe UpdateProposalFile
  -> TxBodyFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuildRawCmd
  (AnyCardanoEra cEra) mScriptValidity txins readOnlyRefIns txinsc mReturnColl
  mTotColl reqSigners txouts mValue mLowBound mUpperBound fee certs wdrls
  metadataSchema scriptFiles metadataFiles mpparams mUpProp (TxBodyFile out) = do
  inputsAndMaybeScriptWits <- firstExceptT ShelleyTxCmdScriptWitnessError
                                $ readScriptWitnessFiles cEra txins
  certFilesAndMaybeScriptWits <- firstExceptT ShelleyTxCmdScriptWitnessError
                                   $ readScriptWitnessFiles cEra certs
  certsAndMaybeScriptWits <- sequence
             [ fmap (,mSwit) (firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                 readFileTextEnvelope AsCertificate certFile)
             | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
             ]
  withdrawalsAndMaybeScriptWits <- firstExceptT ShelleyTxCmdScriptWitnessError
                                     $ readScriptWitnessFilesThruple cEra wdrls
  txMetadata <- firstExceptT ShelleyTxCmdMetadataError
                  . newExceptT $ readTxMetadata cEra metadataSchema metadataFiles
  valuesWithScriptWits <- readValueScriptWitnesses cEra $ fromMaybe (mempty, []) mValue
  scripts <- firstExceptT ShelleyTxCmdScriptFileError $
                     mapM (readFileScriptInAnyLang . unScriptFile) scriptFiles
  txAuxScripts <- hoistEither $ first ShelleyTxCmdAuxScriptsValidationError $ validateTxAuxScripts cEra scripts
  pparams <- case mpparams of
              Just ppFp -> Just <$> firstExceptT ShelleyTxCmdProtocolParamsError (readProtocolParametersSourceSpec ppFp)
              Nothing -> return Nothing
  mProp <- case mUpProp of
             Just (UpdateProposalFile upFp) ->
              Just <$> firstExceptT ShelleyTxCmdReadTextViewFileError
                         (newExceptT $ readFileTextEnvelope AsUpdateProposal upFp)
             Nothing -> return Nothing
  requiredSigners  <- mapM (firstExceptT ShelleyTxCmdRequiredSignerError .  newExceptT . readRequiredSigner) reqSigners
  mReturnCollateral <- case mReturnColl of
                         Just retCol -> Just <$> toTxOutInAnyEra cEra retCol
                         Nothing -> return Nothing
  txOuts <- mapM (toTxOutInAnyEra cEra) txouts

    -- the same collateral input can be used for several plutus scripts
  let filteredTxinsc = Set.toList $ Set.fromList txinsc

  txBody <- hoistEither $ runTxBuildRaw cEra mScriptValidity inputsAndMaybeScriptWits readOnlyRefIns filteredTxinsc
                          mReturnCollateral mTotColl txOuts mLowBound mUpperBound fee valuesWithScriptWits
                          certsAndMaybeScriptWits withdrawalsAndMaybeScriptWits requiredSigners txAuxScripts
                          txMetadata pparams mProp

  let noWitTx = makeSignedTransaction [] txBody
  firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
    getIsCardanoEraConstraint cEra $ writeTxFileTextEnvelopeCddl out noWitTx

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
    validatedTotCollateral
      <- first ShelleyTxCmdTotalCollateralValidationError $ validateTxTotalCollateral era mTotCollateral
    validatedRetCol
      <- first ShelleyTxCmdReturnCollateralValidationError $ validateTxReturnCollateral era mReturnCollateral
    validatedFee
      <- first ShelleyTxCmdTxFeeValidationError $ validateTxFee era mFee
    validatedBounds <- (,) <$> first ShelleyTxCmdTxValidityLowerBoundValidationError (validateTxValidityLowerBound era mLowerBound)
                           <*> first ShelleyTxCmdTxValidityUpperBoundValidationError (validateTxValidityUpperBound era mUpperBound)
    validatedReqSigners
      <- first ShelleyTxCmdRequiredSignersValidationError $ validateRequiredSigners era reqSigners
    validatedPParams
      <- first ShelleyTxCmdProtocolParametersValidationError $ validateProtocolParameters era mpparams
    validatedTxWtdrwls
      <- first ShelleyTxCmdTxWithdrawalsValidationError $ validateTxWithdrawals era withdrawals
    validatedTxCerts
      <- first ShelleyTxCmdTxCertificatesValidationError $ validateTxCertificates era certsAndMaybeSriptWits
    validatedTxUpProp
      <- first ShelleyTxCmdTxUpdateProposalValidationError $ validateTxUpdateProposal era mUpdateProp
    validatedMintValue
      <- createTxMintValue era valuesWithScriptWits
    validatedTxScriptValidity
      <- first ShelleyTxCmdScriptValidityValidationError $ validateTxScriptValidity era mScriptValidity

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
      getIsCardanoEraConstraint era $ createAndValidateTransactionBody txBodyContent

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

  liftIO $ forM_ mpparams $ \_ ->
    printWarning "'--protocol-params-file' for 'transaction build' is deprecated"

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
  validatedTotCollateral
    <- hoistEither $ first ShelleyTxCmdTotalCollateralValidationError $ validateTxTotalCollateral era mTotCollateral
  validatedRetCol
    <- hoistEither $ first ShelleyTxCmdReturnCollateralValidationError $ validateTxReturnCollateral era mReturnCollateral
  dFee <- hoistEither $ first ShelleyTxCmdTxFeeValidationError $ validateTxFee era dummyFee
  validatedBounds <- (,) <$> hoistEither (first ShelleyTxCmdTxValidityLowerBoundValidationError $ validateTxValidityLowerBound era mLowerBound)
                         <*> hoistEither (first ShelleyTxCmdTxValidityUpperBoundValidationError $ validateTxValidityUpperBound era mUpperBound)
  validatedReqSigners <- hoistEither (first ShelleyTxCmdRequiredSignersValidationError $ validateRequiredSigners era reqSigners)
  validatedTxWtdrwls <- hoistEither (first ShelleyTxCmdTxWithdrawalsValidationError $ validateTxWithdrawals era withdrawals)
  validatedTxCerts <- hoistEither (first ShelleyTxCmdTxCertificatesValidationError $ validateTxCertificates era certsAndMaybeScriptWits)
  validatedTxUpProp <- hoistEither (first ShelleyTxCmdTxUpdateProposalValidationError $ validateTxUpdateProposal era mUpdatePropF)
  validatedMintValue <- hoistEither $ createTxMintValue era valuesWithScriptWits
  validatedTxScriptValidity <- hoistEither (first ShelleyTxCmdScriptValidityValidationError $ validateTxScriptValidity era mScriptValidity)

  case (consensusMode, cardanoEraStyle era) of
    (CardanoMode, ShelleyBasedEra _sbe) -> do
      eInMode <- case toEraInMode era CardanoMode of
                   Just result -> return result
                   Nothing ->
                     left (ShelleyTxCmdEraConsensusModeMismatchTxBalance outputOptions
                            (AnyConsensusMode CardanoMode) (AnyCardanoEra era))

      SocketPath sockPath <- firstExceptT ShelleyTxCmdSocketEnvError
                             $ newExceptT readEnvSocketPath

      let allTxInputs = inputsThatRequireWitnessing ++ allReferenceInputs ++ txinsc
          localNodeConnInfo = LocalNodeConnectInfo
                                     { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
                                     , localNodeNetworkId = networkId
                                     , localNodeSocketPath = sockPath
                                     }
      AnyCardanoEra nodeEra
        <- firstExceptT (ShelleyTxCmdQueryConvenienceError . AcqFailure)
             . newExceptT $ determineEra cModeParams localNodeConnInfo

      (nodeEraUTxO, pparams, eraHistory, systemStart, stakePools) <-
        firstExceptT ShelleyTxCmdQueryConvenienceError . newExceptT
          $ queryStateForBalancedTx nodeEra networkId allTxInputs

      validatedPParams <- hoistEither $ first ShelleyTxCmdProtocolParametersValidationError
                                      $ validateProtocolParameters era (Just pparams)

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

      firstExceptT ShelleyTxCmdTxInsDoNotExist
        . hoistEither $ txInsExistInUTxO allTxInputs nodeEraUTxO
      firstExceptT ShelleyTxCmdQueryNotScriptLocked
        . hoistEither $ notScriptLockedTxIns txinsc nodeEraUTxO

      let cAddr = case anyAddressInEra era changeAddr of
                    Right addr -> addr
                    Left _ -> error $ "runTxBuild: Byron address used: " <> show changeAddr

      -- Why do we cast the era? The user can specify an era prior to the era that the node is currently in.
      -- We cannot use the user specified era to construct a query against a node because it may differ
      -- from the node's era and this will result in the 'QueryEraMismatch' failure.
      txEraUtxo <- case first ShelleyTxCmdTxEraCastErr (eraCast era nodeEraUTxO) of
                     Right txEraUtxo -> return txEraUtxo
                     Left e -> left e


      balancedTxBody@(BalancedTxBody _ _ _ fee) <-
        firstExceptT ShelleyTxCmdBalanceTxBody
          . hoistEither
          $ makeTransactionBodyAutoBalance eInMode systemStart (toLedgerEpochInfo eraHistory)
                                           pparams stakePools txEraUtxo txBodyContent
                                           cAddr mOverrideWits

      liftIO $ putStrLn $ "Estimated transaction fee: " <> (show fee :: String)

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


validateTxInsCollateral :: CardanoEra era
                        -> [TxIn]
                        -> Either ShelleyTxCmdError (TxInsCollateral era)
validateTxInsCollateral _   []    = return TxInsCollateralNone
validateTxInsCollateral era txins =
    case collateralSupportedInEra era of
      Nothing -> txFeatureMismatchPure era TxFeatureCollateral
      Just supported -> return (TxInsCollateral supported txins)

validateTxInsReference
  :: CardanoEra era
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
      SimpleScriptWitness _ (SReferenceScript refIn _)  -> Just refIn
      SimpleScriptWitness _ SScript{}  -> Nothing

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
  -> Either ShelleyTxCmdError (TxOutValue era)
toTxOutValueInAnyEra era val =
  case multiAssetSupportedInEra era of
    Left adaOnlyInEra ->
      case valueToLovelace val of
        Just l  -> return (TxOutAdaOnly adaOnlyInEra l)
        Nothing -> txFeatureMismatchPure era TxFeatureMultiAssetOutputs
    Right multiAssetInEra -> return (TxOutValue multiAssetInEra val)

toTxOutInAnyEra :: CardanoEra era
                -> TxOutAnyEra
                -> ExceptT ShelleyTxCmdError IO (TxOut CtxTx era)
toTxOutInAnyEra era (TxOutAnyEra addr' val' mDatumHash refScriptFp) = do
  addr <- hoistEither $ toAddressInAnyEra era addr'
  val <- hoistEither $ toTxOutValueInAnyEra era val'
  (datum, refScript)
    <- case (scriptDataSupportedInEra era, refInsScriptsAndInlineDatsSupportedInEra era) of
         (Nothing, Nothing) -> pure (TxOutDatumNone, ReferenceScriptNone)
         (Just sup, Nothing)->
           (,) <$> toTxAlonzoDatum sup mDatumHash <*> pure ReferenceScriptNone
         (Just sup, Just inlineDatumRefScriptSupp) ->
           toTxDatumReferenceScriptBabbage sup inlineDatumRefScriptSupp mDatumHash refScriptFp
         (Nothing, Just _) ->
           -- TODO: Figure out how to make this state unrepresentable
           error "toTxOutInAnyEra: Should not be possible that inline datums are allowed but datums are not"
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
         sData <- firstExceptT ShelleyTxCmdScriptDataError
                    $ readScriptDataOrFile fileOrSdata
         pure (TxOutDatumHash sDataSupp $ hashScriptDataBytes sData, refScript)
       TxOutDatumByValue fileOrSdata -> do
         sData <- firstExceptT ShelleyTxCmdScriptDataError
                    $ readScriptDataOrFile fileOrSdata
         pure (TxOutDatumInTx sDataSupp sData, refScript)
       TxOutInlineDatumByValue fileOrSdata -> do
         sData <- firstExceptT ShelleyTxCmdScriptDataError
                    $ readScriptDataOrFile fileOrSdata
         pure (TxOutDatumInline inlineRefSupp sData, refScript)

  toTxAlonzoDatum
    :: ScriptDataSupportedInEra era
    -> TxOutDatumAnyEra
    -> ExceptT ShelleyTxCmdError IO (TxOutDatum CtxTx era)
  toTxAlonzoDatum supp cliDatum =
    case cliDatum of
      TxOutDatumByHashOnly h -> pure (TxOutDatumHash supp h)
      TxOutDatumByHashOf sDataOrFile -> do
        sData <- firstExceptT ShelleyTxCmdScriptDataError
                   $ readScriptDataOrFile sDataOrFile
        pure (TxOutDatumHash supp $ hashScriptDataBytes sData)
      TxOutDatumByValue sDataOrFile -> do
        sData <- firstExceptT ShelleyTxCmdScriptDataError
                   $ readScriptDataOrFile sDataOrFile
        pure (TxOutDatumInTx supp sData)
      TxOutInlineDatumByValue _ ->
        txFeatureMismatch era TxFeatureInlineDatums
      TxOutDatumByNone -> pure TxOutDatumNone


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
scriptWitnessPolicyId (SimpleScriptWitness _ (SScript script)) =
   Just . scriptPolicyId $ SimpleScript script
scriptWitnessPolicyId (SimpleScriptWitness _ (SReferenceScript _ mPid)) =
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
  sWits <- mapM (firstExceptT ShelleyTxCmdScriptWitnessError . readScriptWitness era) sWitFiles
  return (v, sWits)

-- ----------------------------------------------------------------------------
-- Transaction signing
--

runTxSign :: InputTxBodyOrTxFile
          -> [WitnessSigningData]
          -> Maybe NetworkId
          -> TxFile
          -> ExceptT ShelleyTxCmdError IO ()
runTxSign txOrTxBody witSigningData mnw (TxFile outTxFile) = do
  sks <-  mapM (firstExceptT ShelleyTxCmdReadWitnessSigningDataError . newExceptT . readWitnessSigningData) witSigningData

  let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeWitness sks

  case txOrTxBody of
    (InputTxFile (TxFile inputTxFilePath)) -> do
      inputTxFile <- liftIO $ fileOrPipe inputTxFilePath
      anyTx <- firstExceptT ShelleyTxCmdCddlError . newExceptT $ readFileTx inputTxFile

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
        writeTxFileTextEnvelopeCddl outTxFile signedTx

    (InputTxBodyFile (TxBodyFile txbodyFilePath)) -> do
      txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
      unwitnessed <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                       $ readFileTxBody txbodyFile

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
runTxSubmit (AnyConsensusModeParams cModeParams) network txFilePath = do

    SocketPath sockPath <- firstExceptT ShelleyTxCmdSocketEnvError
                             $ newExceptT readEnvSocketPath

    txFile <- liftIO $ fileOrPipe txFilePath
    InAnyCardanoEra era tx <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                                $ readFileTx txFile
    let cMode = AnyConsensusMode $ consensusModeOnly cModeParams
    eraInMode <- hoistMaybe
                   (ShelleyTxCmdEraConsensusModeMismatch (Just txFilePath) cMode (AnyCardanoEra era))
                   (toEraInMode era $ consensusModeOnly cModeParams)
    let txInMode = TxInMode tx eraInMode
        localNodeConnInfo = LocalNodeConnectInfo
                              { localConsensusModeParams = cModeParams
                              , localNodeNetworkId = network
                              , localNodeSocketPath = sockPath
                              }

    res <- liftIO $ submitTxToNodeLocal localNodeConnInfo txInMode
    case res of
      Net.Tx.SubmitSuccess -> liftIO $ Text.putStrLn "Transaction successfully submitted."
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
runTxCalculateMinFee (TxBodyFile txbodyFilePath) nw protocolParamsSourceSpec
                     (TxInCount nInputs) (TxOutCount nOutputs)
                     (TxShelleyWitnessCount nShelleyKeyWitnesses)
                     (TxByronWitnessCount nByronKeyWitnesses) = do

    txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
    unwitnessed <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                     $ readFileTxBody txbodyFile
    pparams <- firstExceptT ShelleyTxCmdProtocolParamsError
                 $ readProtocolParametersSourceSpec protocolParamsSourceSpec
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
  pp <- firstExceptT ShelleyTxCmdProtocolParamsError
          $ readProtocolParametersSourceSpec protocolParamsSourceSpec
  out <- toTxOutInAnyEra era txOut
  case cardanoEraStyle era of
    LegacyByronEra -> error "runTxCalculateMinRequiredUTxO: Byron era not implemented yet"
    ShelleyBasedEra sbe -> do
      firstExceptT ShelleyTxCmdPParamsErr . hoistEither
        $ checkProtocolParameters sbe pp
      minValue <- firstExceptT ShelleyTxCmdMinimumUTxOErr
                    . hoistEither $ calculateMinimumUTxO sbe out pp
      liftIO . IO.print $ minValue

runTxCreatePolicyId :: ScriptFile -> ExceptT ShelleyTxCmdError IO ()
runTxCreatePolicyId (ScriptFile sFile) = do
  ScriptInAnyLang _ script <- firstExceptT ShelleyTxCmdScriptFileError $
                                readFileScriptInAnyLang sFile
  liftIO . Text.putStrLn . serialiseToRawBytesHexText $ hashScript script


-- | Error reading the data required to construct a key witness.


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
    d <- firstExceptT ShelleyTxCmdScriptDataError $ readScriptDataOrFile scriptDataOrFile
    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (hashScriptDataBytes d)

runTxGetTxId :: InputTxBodyOrTxFile -> ExceptT ShelleyTxCmdError IO ()
runTxGetTxId txfile = do
    InAnyCardanoEra _era txbody <-
      case txfile of
        InputTxBodyFile (TxBodyFile txbodyFilePath) -> do
          txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
          unwitnessed <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                           $ readFileTxBody txbodyFile
          case unwitnessed of
            UnwitnessedCliFormattedTxBody anyTxBody -> return anyTxBody
            IncompleteCddlFormattedTx (InAnyCardanoEra era tx) ->
              return (InAnyCardanoEra era (getTxBody tx))

        InputTxFile (TxFile txFilePath) -> do
          txFile <- liftIO $ fileOrPipe txFilePath
          InAnyCardanoEra era tx <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                                      $ readFileTx txFile
          return . InAnyCardanoEra era $ getTxBody tx

    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (getTxId txbody)

runTxView :: InputTxBodyOrTxFile -> ExceptT ShelleyTxCmdError IO ()
runTxView = \case
  InputTxBodyFile (TxBodyFile txbodyFilePath) -> do
    txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
    unwitnessed <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                     $ readFileTxBody txbodyFile
    InAnyCardanoEra era txbody <-
      case unwitnessed of
        UnwitnessedCliFormattedTxBody anyTxBody -> pure anyTxBody
        IncompleteCddlFormattedTx (InAnyCardanoEra era tx) ->
          pure $ InAnyCardanoEra era (getTxBody tx)
    --TODO: Why are we maintaining friendlyTxBodyBS and friendlyTxBS?
    -- In the case of a transaction body, we can simply call makeSignedTransaction []
    -- to get a transaction which allows us to reuse friendlyTxBS!
    liftIO $ BS.putStr $ friendlyTxBodyBS era txbody
  InputTxFile (TxFile txFilePath) -> do
    txFile <- liftIO $ fileOrPipe txFilePath
    InAnyCardanoEra era tx <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                                $ readFileTx txFile
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
runTxCreateWitness (TxBodyFile txbodyFilePath) witSignData mbNw (OutputFile oFile) = do
  txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
  unwitnessed <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                   $ readFileTxBody txbodyFile
  case unwitnessed of
    IncompleteCddlFormattedTx anyTx -> do
     InAnyShelleyBasedEra sbe cddlTx <-
       onlyInShelleyBasedEras "sign for Byron era transactions" anyTx

     let txbody = getTxBody cddlTx
     someWit <- firstExceptT ShelleyTxCmdReadWitnessSigningDataError
                  . newExceptT $ readWitnessSigningData witSignData
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
                   . newExceptT $ readWitnessSigningData witSignData

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
runTxSignWitness (TxBodyFile txbodyFilePath) witnessFiles (OutputFile oFp) = do
    txbodyFile <- liftIO $ fileOrPipe txbodyFilePath
    unwitnessed <- firstExceptT ShelleyTxCmdCddlError . newExceptT
                     $ readFileTxBody txbodyFile
    case unwitnessed of
      UnwitnessedCliFormattedTxBody (InAnyCardanoEra era txbody) -> do
        witnesses <-
          sequence
            [ do InAnyCardanoEra era' witness <- firstExceptT ShelleyTxCmdCddlWitnessError . newExceptT
                                                   $ readFileTxKeyWitness file
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
            [ do InAnyCardanoEra era' witness <- firstExceptT ShelleyTxCmdCddlWitnessError . newExceptT
                                                   $ readFileTxKeyWitness file
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


