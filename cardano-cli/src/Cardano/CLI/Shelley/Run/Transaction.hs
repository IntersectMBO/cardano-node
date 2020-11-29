{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( ShelleyTxCmdError
  , renderShelleyTxCmdError
  , runTransactionCmd
  ) where

import           Cardano.Prelude hiding (All, Any)
import           Prelude (String)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import           Data.Type.Equality (TestEquality(..))

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left,
                     newExceptT)

--TODO: do this nicely via the API too:
import qualified Cardano.Binary as CBOR

import qualified Shelley.Spec.Ledger.PParams as Shelley
--TODO: following import needed for orphan Eq Script instance
import           Cardano.Ledger.ShelleyMA.TxBody ()
import           Shelley.Spec.Ledger.Scripts ()

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..), HardForkApplyTxErr (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Shelley.Key (InputDecodeError, readSigningKeyFileAnyOf)
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Types

import           Cardano.Api.Typed as Api
import           Cardano.Api.Protocol
import           Cardano.Api.TxSubmit as Api

data ShelleyTxCmdError
  = ShelleyTxCmdAesonDecodeProtocolParamsError !FilePath !Text
  | ShelleyTxCmdReadFileError !(FileError ())
  | ShelleyTxCmdReadTextViewFileError !(FileError TextEnvelopeError)
  | ShelleyTxCmdReadWitnessSigningDataError !ReadWitnessSigningDataError
  | ShelleyTxCmdWriteFileError !(FileError ())
  | ShelleyTxCmdMetaDataJsonParseError !FilePath !String
  | ShelleyTxCmdMetaDataConversionError !FilePath !TxMetadataJsonError
  | ShelleyTxCmdMetaValidationError !FilePath !TxMetadataRangeError
  | ShelleyTxCmdMetaDecodeError !FilePath !CBOR.DecoderError
  | ShelleyTxCmdBootstrapWitnessError !ShelleyBootstrapWitnessError
  | ShelleyTxCmdSocketEnvError !EnvSocketError
  | ShelleyTxCmdTxSubmitErrorByron !(ApplyTxErr ByronBlock)
  | ShelleyTxCmdTxSubmitErrorShelley !(ApplyTxErr (ShelleyBlock StandardShelley))
  | ShelleyTxCmdTxSubmitErrorAllegra !(ApplyTxErr (ShelleyBlock StandardAllegra))
  | ShelleyTxCmdTxSubmitErrorMary !(ApplyTxErr (ShelleyBlock StandardMary))
  | ShelleyTxCmdTxSubmitErrorEraMismatch !EraMismatch
  | ShelleyTxCmdTxFeatureMismatch AnyCardanoEra TxFeature
  | ShelleyTxCmdTxBodyError SomeTxBodyError
  | ShelleyTxCmdNotImplemented Text
  | ShelleyTxCmdWitnessEraMismatch AnyCardanoEra AnyCardanoEra WitnessFile
  deriving Show

data SomeTxBodyError where
     SomeTxBodyError :: TxBodyError era -> SomeTxBodyError

deriving instance Show SomeTxBodyError


renderShelleyTxCmdError :: ShelleyTxCmdError -> Text
renderShelleyTxCmdError err =
  case err of
    ShelleyTxCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdReadTextViewFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdReadWitnessSigningDataError witSignDataErr ->
      renderReadWitnessSigningDataError witSignDataErr
    ShelleyTxCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdMetaDataJsonParseError fp jsonErr ->
       "Invalid JSON format in file: " <> show fp
                <> "\nJSON parse error: " <> Text.pack jsonErr
    ShelleyTxCmdMetaDataConversionError fp metaDataErr ->
       "Error reading metadata at: " <> show fp
                             <> "\n" <> Text.pack (displayError metaDataErr)
    ShelleyTxCmdMetaDecodeError fp metaDataErr ->
       "Error decoding CBOR metadata at: " <> show fp
                             <> " Error: " <> show metaDataErr
    ShelleyTxCmdMetaValidationError fp valErr ->
      "Error validating transaction metadata at: " <> show fp
                                           <> "\n" <> Text.pack (displayError valErr)
    ShelleyTxCmdSocketEnvError envSockErr -> renderEnvSocketError envSockErr
    ShelleyTxCmdAesonDecodeProtocolParamsError fp decErr ->
      "Error while decoding the protocol parameters at: " <> show fp
                                            <> " Error: " <> show decErr
    ShelleyTxCmdTxSubmitErrorByron res ->
      "Error while submitting tx: " <> Text.pack (show res)
    ShelleyTxCmdTxSubmitErrorShelley res ->
      "Error while submitting tx: " <> Text.pack (show res)
    ShelleyTxCmdTxSubmitErrorAllegra res ->
      "Error while submitting tx: " <> Text.pack (show res)
    ShelleyTxCmdTxSubmitErrorMary res ->
      "Error while submitting tx: " <> Text.pack (show res)
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

    ShelleyTxCmdTxBodyError (SomeTxBodyError err') ->
      "TxBody error: " <> renderTxBodyError err'

    ShelleyTxCmdNotImplemented msg ->
      "Feature not yet implemented: " <> msg

    ShelleyTxCmdWitnessEraMismatch era era' (WitnessFile file) ->
      "The era of a witness does not match the era of the transaction. " <>
      "The transaction is for the " <> renderEra era <> " era, but the " <>
      "witness in " <> show file <> " is for the " <> renderEra era' <> " era."

renderEra :: AnyCardanoEra -> Text
renderEra (AnyCardanoEra ByronEra)   = "Byron"
renderEra (AnyCardanoEra ShelleyEra) = "Shelley"
renderEra (AnyCardanoEra AllegraEra) = "Allegra"
renderEra (AnyCardanoEra MaryEra)    = "Mary"

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

renderTxBodyError :: TxBodyError era -> Text
renderTxBodyError TxBodyEmptyTxIns = "Transaction body has no inputs"
renderTxBodyError TxBodyEmptyTxOuts = "Transaction body has no outputs"
renderTxBodyError (TxBodyLovelaceOverflow txout) =
  "Lovelace overflow error: " <> show txout

runTransactionCmd :: TransactionCmd -> ExceptT ShelleyTxCmdError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuildRaw era txins txouts mValue mLowBound mUpperBound
               fee certs wdrls metadataSchema scriptFiles
               metadataFiles mUpProp out ->
      runTxBuildRaw era txins txouts mLowBound mUpperBound
                    fee mValue certs wdrls metadataSchema
                    scriptFiles metadataFiles mUpProp out
    TxSign txinfile skfiles network txoutfile ->
      runTxSign txinfile skfiles network txoutfile
    TxSubmit protocol network txFp ->
      runTxSubmit protocol network txFp
    TxCalculateMinFee txbody mnw pParamsFile nInputs nOutputs
                      nShelleyKeyWitnesses nByronKeyWitnesses ->
      runTxCalculateMinFee txbody mnw pParamsFile nInputs nOutputs
                           nShelleyKeyWitnesses nByronKeyWitnesses
    TxGetTxId txinfile ->
      runTxGetTxId txinfile
    TxMintedPolicyId sFile -> runTxCreatePolicyId sFile
    TxCreateWitness txBodyfile witSignData mbNw outFile ->
      runTxCreateWitness txBodyfile witSignData mbNw outFile
    TxAssembleTxBodyWitness txBodyFile witnessFile outFile ->
      runTxSignWitness txBodyFile witnessFile outFile


-- ----------------------------------------------------------------------------
-- Building transactions
--

runTxBuildRaw
  :: AnyCardanoEra
  -> [Api.TxIn]
  -> [TxOutAnyEra]
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> Maybe SlotNo
  -- ^ Tx upper bound
  -> Maybe Api.Lovelace
  -- ^ Tx fee
  -> Maybe Value
  -- ^ Multi-Asset value
  -> [CertificateFile]
  -> [(Api.StakeAddress, Api.Lovelace)]
  -> TxMetadataJsonSchema
  -> [ScriptFile]
  -> [MetaDataFile]
  -> Maybe UpdateProposalFile
  -> TxBodyFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuildRaw (AnyCardanoEra era) txins txouts mLowerBound
              mUpperBound mFee mValue
              certFiles withdrawals
              metadataSchema scriptFiles
              metadataFiles mUpdatePropFile
              (TxBodyFile fpath) = do

    txBodyContent <-
      TxBodyContent
        <$> validateTxIns  era txins
        <*> validateTxOuts era txouts
        <*> validateTxFee  era mFee
        <*> ((,) <$> validateTxValidityLowerBound era mLowerBound
                 <*> validateTxValidityUpperBound era mUpperBound)
        <*> validateTxMetadataInEra  era metadataSchema metadataFiles
        <*> validateTxAuxScripts     era scriptFiles
        <*> validateTxWithdrawals    era withdrawals
        <*> validateTxCertificates   era certFiles
        <*> validateTxUpdateProposal era mUpdatePropFile
        <*> validateTxMintValue      era mValue

    txBody <-
      firstExceptT (ShelleyTxCmdTxBodyError . SomeTxBodyError) . hoistEither $
        makeTransactionBody txBodyContent

    firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
      Api.writeFileTextEnvelope fpath Nothing txBody


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
  deriving Show

txFeatureMismatch :: CardanoEra era
                  -> TxFeature
                  -> ExceptT ShelleyTxCmdError IO a
txFeatureMismatch era feature =
    left (ShelleyTxCmdTxFeatureMismatch (anyCardanoEra era) feature)

validateTxIns :: CardanoEra era
              -> [TxIn]
              -> ExceptT ShelleyTxCmdError IO [TxIn]
validateTxIns _ = return -- no validation or era-checking needed

validateTxOuts :: forall era.
                  CardanoEra era
               -> [TxOutAnyEra]
               -> ExceptT ShelleyTxCmdError IO [TxOut era]
validateTxOuts era = mapM toTxOutInAnyEra
  where
    toTxOutInAnyEra :: TxOutAnyEra
                    -> ExceptT ShelleyTxCmdError IO (TxOut era)
    toTxOutInAnyEra (TxOutAnyEra addr val) = TxOut <$> toAddressInAnyEra addr
                                                   <*> toTxOutValueInAnyEra val

    toAddressInAnyEra :: AddressAny -> ExceptT ShelleyTxCmdError IO (AddressInEra era)
    toAddressInAnyEra addrAny =
      case addrAny of
        AddressByron   bAddr -> return (AddressInEra ByronAddressInAnyEra bAddr)
        AddressShelley sAddr ->
          case cardanoEraStyle era of
            LegacyByronEra -> txFeatureMismatch era TxFeatureShelleyAddresses

            ShelleyBasedEra era' ->
              return (AddressInEra (ShelleyAddressInEra era') sAddr)

    toTxOutValueInAnyEra :: Value -> ExceptT ShelleyTxCmdError IO (TxOutValue era)
    toTxOutValueInAnyEra val =
      case multiAssetSupportedInEra era of
        Left adaOnlyInEra ->
          case valueToLovelace val of
            Just l  -> return (TxOutAdaOnly adaOnlyInEra l)
            Nothing -> txFeatureMismatch era TxFeatureMultiAssetOutputs
        Right multiAssetInEra -> return (TxOutValue multiAssetInEra val)


validateTxFee :: CardanoEra era
              -> Maybe Lovelace
              -> ExceptT ShelleyTxCmdError IO (TxFee era)
validateTxFee era mfee =
    case (txFeesExplicitInEra era, mfee) of
      (Left  implicit, Nothing)  -> return (TxFeeImplicit implicit)
      (Right explicit, Just fee) -> return (TxFeeExplicit explicit fee)

      (Right _, Nothing) -> txFeatureMismatch era TxFeatureImplicitFees
      (Left  _, Just _)  -> txFeatureMismatch era TxFeatureExplicitFees


validateTxValidityLowerBound :: CardanoEra era
                             -> Maybe SlotNo
                             -> ExceptT ShelleyTxCmdError IO
                                        (TxValidityLowerBound era)
validateTxValidityLowerBound _ Nothing = return TxValidityNoLowerBound
validateTxValidityLowerBound era (Just slot) =
    case validityLowerBoundSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureValidityLowerBound
      Just supported -> return (TxValidityLowerBound supported slot)


validateTxValidityUpperBound :: CardanoEra era
                             -> Maybe SlotNo
                             -> ExceptT ShelleyTxCmdError IO
                                        (TxValidityUpperBound era)
validateTxValidityUpperBound era Nothing =
    case validityNoUpperBoundSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureValidityNoUpperBound
      Just supported -> return (TxValidityNoUpperBound supported)
validateTxValidityUpperBound era (Just slot) =
    case validityUpperBoundSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureValidityUpperBound
      Just supported -> return (TxValidityUpperBound supported slot)


validateTxMetadataInEra :: CardanoEra era
                        -> TxMetadataJsonSchema
                        -> [MetaDataFile]
                        -> ExceptT ShelleyTxCmdError IO (TxMetadataInEra era)
validateTxMetadataInEra _ _ [] = return TxMetadataNone
validateTxMetadataInEra era schema files =
    case txMetadataSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureTxMetadata
      Just supported -> do
        metadata <- mconcat <$> mapM (readFileTxMetaData schema) files
        return (TxMetadataInEra supported metadata)


validateTxAuxScripts :: CardanoEra era
                     -> [ScriptFile]
                     -> ExceptT ShelleyTxCmdError IO (TxAuxScripts era)
validateTxAuxScripts _ [] = return TxAuxScriptsNone
validateTxAuxScripts era files =
  case auxScriptsSupportedInEra era of
    Nothing -> txFeatureMismatch era TxFeatureAuxScripts
    Just AuxScriptsInAllegraEra -> do
      scripts <- sequence
        [ firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT
            $ readFileTextEnvelope (AsScript AsAllegraEra) file
        | ScriptFile file <- files ]
      return $ TxAuxScripts AuxScriptsInAllegraEra scripts
    Just AuxScriptsInMaryEra -> do
      scripts <- sequence
        [ firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT
            $ readFileTextEnvelope (AsScript AsMaryEra) file
        | ScriptFile file <- files ]
      return (TxAuxScripts AuxScriptsInMaryEra scripts)

validateTxWithdrawals :: CardanoEra era
                      -> [(StakeAddress, Lovelace)]
                      -> ExceptT ShelleyTxCmdError IO (TxWithdrawals era)
validateTxWithdrawals _ [] = return TxWithdrawalsNone
validateTxWithdrawals era withdrawals =
    case withdrawalsSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureWithdrawals
      Just supported -> return (TxWithdrawals supported withdrawals)


validateTxCertificates :: CardanoEra era
                       -> [CertificateFile]
                       -> ExceptT ShelleyTxCmdError IO (TxCertificates era)
validateTxCertificates era certFiles =
  case certificatesSupportedInEra era of
    Nothing
      | null certFiles -> return TxCertificatesNone
      | otherwise      -> txFeatureMismatch era TxFeatureCertificates
    Just supported -> do
      certs <- sequence
                 [ firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                     readFileTextEnvelope AsCertificate certFile
                 | CertificateFile certFile <- certFiles ]
      return $ TxCertificates supported certs


validateTxUpdateProposal :: CardanoEra era
                         -> Maybe UpdateProposalFile
                         -> ExceptT ShelleyTxCmdError IO (TxUpdateProposal era)
validateTxUpdateProposal _ Nothing = return TxUpdateProposalNone
validateTxUpdateProposal era (Just (UpdateProposalFile file)) =
    case updateProposalSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureCertificates
      Just supported -> do
         prop <- firstExceptT ShelleyTxCmdReadTextViewFileError $ newExceptT $
                   readFileTextEnvelope AsUpdateProposal file
         return (TxUpdateProposal supported prop)


validateTxMintValue :: CardanoEra era
                    -> Maybe Value
                    -> ExceptT ShelleyTxCmdError IO (TxMintValue era)
validateTxMintValue _ Nothing = return TxMintNone
validateTxMintValue era (Just v) =
    case multiAssetSupportedInEra era of
       Left _ -> txFeatureMismatch era TxFeatureMintValue
       Right supported -> return (TxMintValue supported v)


-- ----------------------------------------------------------------------------
-- Transaction signing
--

runTxSign :: TxBodyFile
          -> [WitnessSigningData]
          -> Maybe Api.NetworkId
          -> TxFile
          -> ExceptT ShelleyTxCmdError IO ()
runTxSign (TxBodyFile txbodyFile) witSigningData mnw (TxFile txFile) = do

  InAnyShelleyBasedEra era txbody <-
        --TODO: in principle we should be able to support Byron era txs too
        onlyInShelleyBasedEras "sign for Byron era transactions"
    =<< readFileTxBody txbodyFile

  sks    <- firstExceptT ShelleyTxCmdReadWitnessSigningDataError $
              mapM readWitnessSigningData witSigningData

  let (sksByron, sksShelley, scsShelley) = partitionSomeWitnesses $ map categoriseSomeWitness sks

  -- Byron witnesses require the network ID. This can either be provided
  -- directly or derived from a provided Byron address.
  byronWitnesses <- firstExceptT ShelleyTxCmdBootstrapWitnessError
    . hoistEither
    $ mkShelleyBootstrapWitnesses mnw txbody sksByron

  let shelleyKeyWitnesses = map (Api.makeShelleyKeyWitness txbody) sksShelley
      shelleyScriptWitnesses =
        recoverHasScriptFeatures era $
          map (makeScriptWitness
             . SimpleScript
             . coerceSimpleScriptEra era) scsShelley
      shelleyWitnesses = shelleyKeyWitnesses ++ shelleyScriptWitnesses
      tx = Api.makeSignedTransaction (byronWitnesses ++ shelleyWitnesses) txbody

  firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
    Api.writeFileTextEnvelope txFile Nothing tx


-- ----------------------------------------------------------------------------
-- Transaction submission
--

runTxSubmit :: Protocol -> NetworkId -> FilePath
            -> ExceptT ShelleyTxCmdError IO ()
runTxSubmit protocol network txFile = do
    SocketPath sockPath <- firstExceptT ShelleyTxCmdSocketEnvError readEnvSocketPath

    InAnyCardanoEra era tx <- readFileTx txFile

    withlocalNodeConnectInfo protocol network sockPath $ \connectInfo ->
      case (localNodeConsensusMode connectInfo, era) of
        (ByronMode{}, ByronEra) -> do
          result <- liftIO $ Api.submitTx connectInfo (TxForByronMode tx)
          case result of
            TxSubmitSuccess -> return ()
            TxSubmitFailureByronMode err ->
              left (ShelleyTxCmdTxSubmitErrorByron err)

        (ByronMode{}, _) ->
          left $ ShelleyTxCmdTxSubmitErrorEraMismatch EraMismatch {
                   ledgerEraName = "Byron",
                   otherEraName  = show era
                 }

        (ShelleyMode{}, ShelleyEra) -> do
          result <- liftIO $ Api.submitTx connectInfo (TxForShelleyMode tx)
          case result of
            TxSubmitSuccess -> return ()
            TxSubmitFailureShelleyMode err ->
              left (ShelleyTxCmdTxSubmitErrorShelley err)

        (ShelleyMode{}, _) ->
          left $ ShelleyTxCmdTxSubmitErrorEraMismatch EraMismatch {
                   ledgerEraName = "Shelley",
                   otherEraName  = show era
                 }

        (CardanoMode{}, _) -> do
          result <- liftIO $ Api.submitTx connectInfo
                               (TxForCardanoMode (InAnyCardanoEra era tx))
          case result of
            TxSubmitSuccess -> return ()
            TxSubmitFailureCardanoMode (ApplyTxErrByron err) ->
              left (ShelleyTxCmdTxSubmitErrorByron err)
            TxSubmitFailureCardanoMode (ApplyTxErrShelley err) ->
              left (ShelleyTxCmdTxSubmitErrorShelley err)
            TxSubmitFailureCardanoMode (ApplyTxErrAllegra err) ->
              left (ShelleyTxCmdTxSubmitErrorAllegra err)
            TxSubmitFailureCardanoMode (ApplyTxErrMary err) ->
              left (ShelleyTxCmdTxSubmitErrorMary err)
            TxSubmitFailureCardanoMode (ApplyTxErrWrongEra mismatch) ->
              left (ShelleyTxCmdTxSubmitErrorEraMismatch mismatch)


-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTxCalculateMinFee
  :: TxBodyFile
  -> Maybe Api.NetworkId
  -> ProtocolParamsFile
  -> TxInCount
  -> TxOutCount
  -> TxShelleyWitnessCount
  -> TxByronWitnessCount
  -> ExceptT ShelleyTxCmdError IO ()
runTxCalculateMinFee (TxBodyFile txbodyFile) nw pParamsFile
                     (TxInCount nInputs) (TxOutCount nOutputs)
                     (TxShelleyWitnessCount nShelleyKeyWitnesses)
                     (TxByronWitnessCount nByronKeyWitnesses) = do

    InAnyShelleyBasedEra _era txbody <-
          --TODO: in principle we should be able to support Byron era txs too
          onlyInShelleyBasedEras "calculate-min-fee for Byron era transactions"
      =<< readFileTxBody txbodyFile

    pparams <- readProtocolParameters pParamsFile

    let tx = Api.makeSignedTransaction [] txbody
        Api.Lovelace fee = Api.estimateTransactionFee
                             (fromMaybe Api.Mainnet nw)
                             (Shelley._minfeeB pparams) --TODO: do this better
                             (Shelley._minfeeA pparams)
                             tx
                             nInputs nOutputs
                             nByronKeyWitnesses nShelleyKeyWitnesses

    liftIO $ putStrLn $ (show fee :: String) <> " Lovelace"

runTxCreatePolicyId :: ScriptFile -> ExceptT ShelleyTxCmdError IO ()
runTxCreatePolicyId (ScriptFile _sFile) =
  -- Here we would decode the JSON script file and then hash.
  liftIO $ putTextLn "Not implemented yet"

--TODO: eliminate this and get only the necessary params, and get them in a more
-- helpful way rather than requiring them as a local file.
readProtocolParameters :: ProtocolParamsFile
                       -> ExceptT ShelleyTxCmdError IO (Shelley.PParams StandardShelley)
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (ShelleyTxCmdAesonDecodeProtocolParamsError fpath . Text.pack) . hoistEither $
    Aeson.eitherDecode' pparams

data SomeWitness
  = AByronSigningKey           (Api.SigningKey Api.ByronKey) (Maybe (Address ByronAddr))
  | APaymentSigningKey         (Api.SigningKey Api.PaymentKey)
  | APaymentExtendedSigningKey (Api.SigningKey Api.PaymentExtendedKey)
  | AStakeSigningKey           (Api.SigningKey Api.StakeKey)
  | AStakeExtendedSigningKey   (Api.SigningKey Api.StakeExtendedKey)
  | AStakePoolSigningKey       (Api.SigningKey Api.StakePoolKey)
  | AGenesisSigningKey         (Api.SigningKey Api.GenesisKey)
  | AGenesisExtendedSigningKey (Api.SigningKey Api.GenesisExtendedKey)
  | AGenesisDelegateSigningKey (Api.SigningKey Api.GenesisDelegateKey)
  | AGenesisDelegateExtendedSigningKey
                               (Api.SigningKey Api.GenesisDelegateExtendedKey)
  | AGenesisUTxOSigningKey     (Api.SigningKey Api.GenesisUTxOKey)

    --TODO switch from multi-sig specifically, and in Shelley era only,
    -- to a universal type with the union of any script language, which
    -- we can later convert to a script in an era.
  | AShelleyMultiSigScript     (Api.MultiSigScript ShelleyEra)

-- | Error deserialising a JSON-encoded script.
newtype ScriptJsonDecodeError = ScriptJsonDecodeError String
  deriving Show

instance Error ScriptJsonDecodeError where
  displayError (ScriptJsonDecodeError errStr) = errStr

-- | Error reading the data required to construct a key witness.
data ReadWitnessSigningDataError
  = ReadWitnessSigningDataSigningKeyDecodeError !(FileError InputDecodeError)
  | ReadWitnessSigningDataScriptError !(FileError ScriptJsonDecodeError)
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
readWitnessSigningData (ScriptWitnessSigningData (ScriptFile fp)) = do
  msJson <- handleIOExceptT (ReadWitnessSigningDataScriptError . FileIOError fp)
    $ LBS.readFile fp

  hoistEither $ bimap
    (ReadWitnessSigningDataScriptError . FileError fp . ScriptJsonDecodeError)
    AShelleyMultiSigScript
    (Aeson.eitherDecode' msJson)

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
      [ Api.FromSomeType (Api.AsSigningKey Api.AsByronKey)
                          (`AByronSigningKey` mbByronAddr)
      , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey)
                          APaymentSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakeKey)
                          AStakeSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakeExtendedKey)
                          AStakeExtendedSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakePoolKey)
                          AStakePoolSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisKey)
                          AGenesisSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisExtendedKey)
                          AGenesisExtendedSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisDelegateKey)
                          AGenesisDelegateSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisDelegateExtendedKey)
                          AGenesisDelegateExtendedSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisUTxOKey)
                          AGenesisUTxOSigningKey
      ]

    bech32FileTypes =
      [ Api.FromSomeType (Api.AsSigningKey Api.AsByronKey)
                          (`AByronSigningKey` mbByronAddr)
      , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey)
                          APaymentSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakeKey)
                          AStakeSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakeExtendedKey)
                          AStakeExtendedSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakePoolKey)
                          AStakePoolSigningKey
      ]

partitionSomeWitnesses
  :: [ByronOrShelleyWitness]
  -> ( [ShelleyBootstrapWitnessSigningKeyData]
     , [Api.ShelleyWitnessSigningKey]
     , [Api.MultiSigScript ShelleyEra]
     )
partitionSomeWitnesses = reversePartitionedWits . foldl' go mempty
  where
    reversePartitionedWits (bw, skw, ssw) =
      (reverse bw, reverse skw, reverse ssw)

    go (byronAcc, shelleyKeyAcc, shelleyScriptAcc) byronOrShelleyWit =
      case byronOrShelleyWit of
        AByronWitness byronWit ->
          (byronWit:byronAcc, shelleyKeyAcc, shelleyScriptAcc)
        AShelleyKeyWitness shelleyKeyWit ->
          (byronAcc, shelleyKeyWit:shelleyKeyAcc, shelleyScriptAcc)
        AShelleyScriptWitness shelleyScriptWit ->
          (byronAcc, shelleyKeyAcc, shelleyScriptWit:shelleyScriptAcc)


-- | Some kind of Byron or Shelley witness.
data ByronOrShelleyWitness
  = AByronWitness !ShelleyBootstrapWitnessSigningKeyData
  | AShelleyKeyWitness !Api.ShelleyWitnessSigningKey

    --TODO switch from multi-sig specifically, and in Shelley era only,
    -- to a universal type with the union of any script language, which
    -- we can later convert to a script in an era.
  | AShelleyScriptWitness !(Api.MultiSigScript ShelleyEra)

categoriseSomeWitness :: SomeWitness -> ByronOrShelleyWitness
categoriseSomeWitness swsk =
  case swsk of
    AByronSigningKey           sk addr -> AByronWitness (ShelleyBootstrapWitnessSigningKeyData sk addr)
    APaymentSigningKey         sk      -> AShelleyKeyWitness (Api.WitnessPaymentKey         sk)
    APaymentExtendedSigningKey sk      -> AShelleyKeyWitness (Api.WitnessPaymentExtendedKey sk)
    AStakeSigningKey           sk      -> AShelleyKeyWitness (Api.WitnessStakeKey           sk)
    AStakeExtendedSigningKey   sk      -> AShelleyKeyWitness (Api.WitnessStakeExtendedKey   sk)
    AStakePoolSigningKey       sk      -> AShelleyKeyWitness (Api.WitnessStakePoolKey       sk)
    AGenesisSigningKey         sk      -> AShelleyKeyWitness (Api.WitnessGenesisKey sk)
    AGenesisExtendedSigningKey sk      -> AShelleyKeyWitness (Api.WitnessGenesisExtendedKey sk)
    AGenesisDelegateSigningKey sk      -> AShelleyKeyWitness (Api.WitnessGenesisDelegateKey sk)
    AGenesisDelegateExtendedSigningKey sk
                                       -> AShelleyKeyWitness (Api.WitnessGenesisDelegateExtendedKey sk)
    AGenesisUTxOSigningKey     sk      -> AShelleyKeyWitness (Api.WitnessGenesisUTxOKey     sk)
    AShelleyMultiSigScript scr         -> AShelleyScriptWitness scr

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
  -> Either ShelleyBootstrapWitnessError (Witness era)
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
  -> Either ShelleyBootstrapWitnessError [Witness era]
mkShelleyBootstrapWitnesses mnw txBody =
  mapM (mkShelleyBootstrapWitness mnw txBody)


runTxGetTxId :: TxBodyFile -> ExceptT ShelleyTxCmdError IO ()
runTxGetTxId (TxBodyFile txbodyFile) = do
  InAnyCardanoEra _era txbody <- readFileTxBody txbodyFile
  liftIO $ BS.putStrLn $ Api.serialiseToRawBytesHex (Api.getTxId txbody)

runTxCreateWitness
  :: TxBodyFile
  -> WitnessSigningData
  -> Maybe NetworkId
  -> OutputFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxCreateWitness (TxBodyFile txbodyFile) witSignData mbNw (OutputFile oFile) = do

  InAnyShelleyBasedEra era txbody <-
        --TODO: in principle we should be able to support Byron era txs too
        onlyInShelleyBasedEras "witness for Byron era transactions"
    =<< readFileTxBody txbodyFile
  -- We use the era of the tx we read to determine the era we use for the rest:

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
      AShelleyScriptWitness scShelley ->
          recoverHasScriptFeatures era $
          pure
        . makeScriptWitness
        . SimpleScript
        . coerceSimpleScriptEra era
        $ scShelley

  firstExceptT ShelleyTxCmdWriteFileError
    . newExceptT
    $ Api.writeFileTextEnvelope oFile Nothing witness

--TODO: eliminate the need for this hack as part of the Script API refactor
recoverHasScriptFeatures :: forall era a.
                            ShelleyBasedEra era
                         -> (HasScriptFeatures era => a)
                         -> a
recoverHasScriptFeatures ShelleyBasedEraShelley f = f
recoverHasScriptFeatures ShelleyBasedEraAllegra f = f
recoverHasScriptFeatures ShelleyBasedEraMary    f = f


runTxSignWitness
  :: TxBodyFile
  -> [WitnessFile]
  -> OutputFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxSignWitness (TxBodyFile txbodyFile) witnessFiles (OutputFile oFp) = do

    InAnyCardanoEra era txbody  <- readFileTxBody txbodyFile
    InAnyShelleyBasedEra _ _ <-
          --TODO: in principle we should be able to support Byron era txs too
          onlyInShelleyBasedEras "sign for Byron era transactions"
                                 (InAnyCardanoEra era txbody)

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

    let tx = Api.makeSignedTransaction witnesses txbody
    firstExceptT ShelleyTxCmdWriteFileError
      . newExceptT
      $ Api.writeFileTextEnvelope oFp Nothing tx


-- ----------------------------------------------------------------------------
-- Reading files in any era
--

_readFileScript :: FilePath
               -> ExceptT ShelleyTxCmdError IO (InAnyShelleyBasedEra Script)
_readFileScript = readFileInAnyShelleyBasedEra AsScript


readFileWitness :: FilePath
                -> ExceptT ShelleyTxCmdError IO (InAnyCardanoEra Witness)
readFileWitness = readFileInAnyCardanoEra AsWitness


readFileTxBody :: FilePath
               -> ExceptT ShelleyTxCmdError IO (InAnyCardanoEra TxBody)
readFileTxBody = readFileInAnyCardanoEra AsTxBody


readFileTx :: FilePath -> ExceptT ShelleyTxCmdError IO (InAnyCardanoEra Tx)
readFileTx = readFileInAnyCardanoEra AsTx


readFileInAnyCardanoEra
  :: ( HasTextEnvelope (thing ByronEra)
     , HasTextEnvelope (thing ShelleyEra)
     , HasTextEnvelope (thing AllegraEra)
     , HasTextEnvelope (thing MaryEra)
     )
  => (forall era. AsType era -> AsType (thing era))
  -> FilePath
  -> ExceptT ShelleyTxCmdError IO
            (InAnyCardanoEra thing)
readFileInAnyCardanoEra asThing file =
    firstExceptT ShelleyTxCmdReadTextViewFileError
  . newExceptT
  $ Api.readFileTextEnvelopeAnyOf
      [ Api.FromSomeType (asThing AsByronEra)   (InAnyCardanoEra ByronEra)
      , Api.FromSomeType (asThing AsShelleyEra) (InAnyCardanoEra ShelleyEra)
      , Api.FromSomeType (asThing AsAllegraEra) (InAnyCardanoEra AllegraEra)
      , Api.FromSomeType (asThing AsMaryEra)    (InAnyCardanoEra MaryEra)
      ]
      file

readFileInAnyShelleyBasedEra
  :: ( HasTextEnvelope (thing ShelleyEra)
     , HasTextEnvelope (thing AllegraEra)
     , HasTextEnvelope (thing MaryEra)
     )
  => (forall era. AsType era -> AsType (thing era))
  -> FilePath
  -> ExceptT ShelleyTxCmdError IO
            (InAnyShelleyBasedEra thing)
readFileInAnyShelleyBasedEra asThing file =
    firstExceptT ShelleyTxCmdReadTextViewFileError
  . newExceptT
  $ Api.readFileTextEnvelopeAnyOf
      [ Api.FromSomeType (asThing AsShelleyEra)
                         (InAnyShelleyBasedEra ShelleyBasedEraShelley)
      , Api.FromSomeType (asThing AsAllegraEra)
                         (InAnyShelleyBasedEra ShelleyBasedEraAllegra)
      , Api.FromSomeType (asThing AsMaryEra)
                         (InAnyShelleyBasedEra ShelleyBasedEraMary)
      ]
      file

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
-- Transaction metadata
--

readFileTxMetaData :: TxMetadataJsonSchema -> MetaDataFile
                   -> ExceptT ShelleyTxCmdError IO Api.TxMetadata
readFileTxMetaData mapping (MetaDataFileJSON fp) = do
    bs <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fp) $
          LBS.readFile fp
    v  <- firstExceptT (ShelleyTxCmdMetaDataJsonParseError fp) $
          hoistEither $
            Aeson.eitherDecode' bs
    firstExceptT (ShelleyTxCmdMetaDataConversionError fp) $ hoistEither $
      metadataFromJson mapping v
readFileTxMetaData _ (MetaDataFileCBOR fp) = do
    bs <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fp) $
          BS.readFile fp
    txMetadata <- firstExceptT (ShelleyTxCmdMetaDecodeError fp) $ hoistEither $
      Api.deserialiseFromCBOR Api.AsTxMetadata bs
    firstExceptT (ShelleyTxCmdMetaValidationError fp . NE.head) $ hoistEither $
      validateTxMetadata txMetadata
