{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( ShelleyTxCmdError
  , renderShelleyTxCmdError
  , runTransactionCmd
  ) where

import           Cardano.Prelude hiding (All, Any)
import           Prelude (String)

import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left,
                     newExceptT, right)

--TODO: do this nicely via the API too:
import qualified Cardano.Binary as CBOR

import qualified Shelley.Spec.Ledger.PParams as Shelley

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..),
                     HardForkApplyTxErr (ApplyTxErrByron, ApplyTxErrShelley, ApplyTxErrWrongEra))
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardShelley)

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Shelley.Key (SigningKeyDecodeError (..), readSigningKeyFileAnyOf)
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Types

import           Cardano.Api.MetaData
import           Cardano.Api.Protocol
import           Cardano.Api.TxSubmit as Api
import           Cardano.Api.Typed as Api

data ShelleyTxCmdError
  = ShelleyTxCmdAesonDecodeProtocolParamsError !FilePath !Text
  | ShelleyTxCmdReadFileError !(FileError ())
  | ShelleyTxCmdReadTextViewFileError !(FileError TextEnvelopeError)
  | ShelleyTxCmdReadSigningKeyFileError !(FileError SigningKeyDecodeError)
  | ShelleyTxCmdWriteFileError !(FileError ())
  | ShelleyTxCmdMetaDataJsonParseError !FilePath !String
  | ShelleyTxCmdMetaDataConversionError !FilePath !TxMetadataJsonError
  | ShelleyTxCmdMetaValidationError !FilePath !TxMetadataRangeError
  | ShelleyTxCmdMetaDecodeError !FilePath !CBOR.DecoderError
  | ShelleyTxCmdMissingNetworkId
  | ShelleyTxCmdSocketEnvError !EnvSocketError
  | ShelleyTxCmdTxSubmitErrorByron !(ApplyTxErr ByronBlock)
  | ShelleyTxCmdTxSubmitErrorShelley !(ApplyTxErr (ShelleyBlock StandardShelley))
  | ShelleyTxCmdTxSubmitErrorEraMismatch !EraMismatch
  deriving Show

renderShelleyTxCmdError :: ShelleyTxCmdError -> Text
renderShelleyTxCmdError err =
  case err of
    ShelleyTxCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdReadTextViewFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdReadSigningKeyFileError fileErr -> Text.pack (displayError fileErr)
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
    ShelleyTxCmdTxSubmitErrorEraMismatch EraMismatch{ledgerEraName, otherEraName} ->
      "The era of the node and the tx do not match. " <>
      "The node is running in the " <> ledgerEraName <>
      " era, but the transaction is for the " <> otherEraName <> " era."
    ShelleyTxReadFileException fileErr -> Text.pack (Api.displayError fileErr)
    ShelleyTxReadFileError fileErr -> Text.pack (Api.displayError fileErr)
    ShelleyTxWriteFileError fileErr -> Text.pack (Api.displayError fileErr)
    ShelleyTxMissingNetworkId -> "Please enter network id with your byron transaction"

runTransactionCmd :: TransactionCmd -> ExceptT ShelleyTxCmdError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuildRaw txins txouts ttl fee certs wdrls
               metadataSchema metadataFiles mUpProp out ->
      runTxBuildRaw txins txouts ttl fee certs wdrls
                    metadataSchema metadataFiles mUpProp out
    TxBuildMultiSig mScriptObj mOutputFile -> runTxBuildMultiSig mScriptObj mOutputFile
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
    TxCreateWitness txBodyfile sKeyOrScript mbNw outFile ->
      runTxCreateWitness txBodyfile sKeyOrScript mbNw outFile
    TxAssembleTxBodyWitness txBodyFile witnessFile outFile ->
      runTxSignWitness txBodyFile witnessFile outFile

runTxBuildRaw
  :: [Api.TxIn]
  -> [Api.TxOut Api.Shelley]
  -> SlotNo
  -> Api.Lovelace
  -> [CertificateFile]
  -> [(Api.StakeAddress, Api.Lovelace)]
  -> TxMetadataJsonSchema
  -> [MetaDataFile]
  -> Maybe UpdateProposalFile
  -> TxBodyFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuildRaw txins txouts ttl fee
              certFiles withdrawals
              metadataSchema metadataFiles
              mUpdatePropFile
              (TxBodyFile fpath) = do

    certs <- sequence
               [ firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                   Api.readFileTextEnvelope Api.AsCertificate certFile
               | CertificateFile certFile <- certFiles ]


    mMetaData <- case metadataFiles of
      []    -> return Nothing
      files -> Just . mconcat <$> mapM (readFileTxMetaData metadataSchema) files
               -- read all the files and merge their metadata maps
               -- in case of clashes earlier entries take precedence

    mUpdateProp <-
      case mUpdatePropFile of
        Nothing                        -> return Nothing
        Just (UpdateProposalFile file) ->
          fmap Just <$> firstExceptT ShelleyTxCmdReadTextViewFileError $ newExceptT $
            Api.readFileTextEnvelope Api.AsUpdateProposal file

    let txBody = Api.makeShelleyTransaction
                   Api.txExtraContentEmpty {
                     Api.txCertificates   = certs,
                     Api.txWithdrawals    = withdrawals,
                     Api.txMetadata       = mMetaData,
                     Api.txUpdateProposal = mUpdateProp
                   }
                   ttl
                   fee
                   txins
                   txouts

    firstExceptT ShelleyTxCmdWriteFileError
      . newExceptT
      $ Api.writeFileTextEnvelope fpath Nothing txBody


runTxSign :: TxBodyFile
          -> [SigningKeyFile]
          -> Maybe Api.NetworkId
          -> TxFile
          -> ExceptT ShelleyTxCmdError IO ()
runTxSign (TxBodyFile txbodyFile) skFiles mNw (TxFile txFile) = do
  txbody <- firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
              Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
  sks    <- firstExceptT ShelleyTxCmdReadSigningKeyFileError $
              mapM readSigningKeyFile skFiles

  let (sksByron, sksShelley, scsShelley) = partitionSomeWitnesses $ map categoriseSomeWitness sks

  byronWitnesses <- case (sksByron, mNw) of
                      ([], Nothing) -> return []
                      (_, Nothing) -> left ShelleyTxCmdMissingNetworkId
                      (_, Just nw) -> return $ map (Api.makeShelleyBootstrapWitness nw txbody) sksByron

  let shelleyKeyWitnesses = map (Api.makeShelleyKeyWitness txbody) sksShelley
      shelleyScriptWitnesses = map (makeShelleyScriptWitness . makeMultiSigScript) scsShelley
      shelleyWitnesses = shelleyKeyWitnesses ++ shelleyScriptWitnesses
      tx = Api.makeSignedTransaction (byronWitnesses ++ shelleyWitnesses) txbody


  firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
    Api.writeFileTextEnvelope txFile Nothing tx

runTxSubmit :: Protocol -> NetworkId -> FilePath
            -> ExceptT ShelleyTxCmdError IO ()
runTxSubmit protocol network txFile = do
    SocketPath sockPath <- firstExceptT ShelleyTxCmdSocketEnvError readEnvSocketPath
    tx <- firstExceptT ShelleyTxCmdReadTextViewFileError
      . newExceptT
      $ Api.readFileTextEnvelopeAnyOf
          [ Api.FromSomeType Api.AsByronTx   Left
          , Api.FromSomeType Api.AsShelleyTx Right ]
          txFile

    withlocalNodeConnectInfo protocol network sockPath $ \connectInfo ->
      case (localNodeConsensusMode connectInfo, tx) of
        (ByronMode{}, Left tx') -> do
          result <- liftIO $ Api.submitTx connectInfo (TxForByronMode tx')
          case result of
            TxSubmitSuccess -> return ()
            TxSubmitFailureByronMode err ->
              left (ShelleyTxCmdTxSubmitErrorByron err)

        (ByronMode{}, Right{}) ->
          left $ ShelleyTxCmdTxSubmitErrorEraMismatch EraMismatch {
                   ledgerEraName = "Byron",
                   otherEraName  = "Shelley"
                 }

        (ShelleyMode{}, Right tx') -> do
          result <- liftIO $ Api.submitTx connectInfo (TxForShelleyMode tx')
          case result of
            TxSubmitSuccess -> return ()
            TxSubmitFailureShelleyMode err ->
              left (ShelleyTxCmdTxSubmitErrorShelley err)

        (ShelleyMode{}, Left{}) ->
          left $ ShelleyTxCmdTxSubmitErrorEraMismatch EraMismatch {
                   ledgerEraName = "Shelley",
                   otherEraName  = "Byron"
                 }

        (CardanoMode{}, tx') -> do
          result <- liftIO $ Api.submitTx connectInfo (TxForCardanoMode tx')
          case result of
            TxSubmitSuccess -> return ()
            TxSubmitFailureCardanoMode (ApplyTxErrByron err) ->
              left (ShelleyTxCmdTxSubmitErrorByron err)
            TxSubmitFailureCardanoMode (ApplyTxErrShelley err) ->
              left (ShelleyTxCmdTxSubmitErrorShelley err)
            TxSubmitFailureCardanoMode (ApplyTxErrWrongEra mismatch) ->
              left (ShelleyTxCmdTxSubmitErrorEraMismatch mismatch)


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

    txbody <- firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile

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

--TODO: eliminate this and get only the necessary params, and get them in a more
-- helpful way rather than requiring them as a local file.
readProtocolParameters :: ProtocolParamsFile
                       -> ExceptT ShelleyTxCmdError IO (Shelley.PParams StandardShelley)
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (ShelleyTxCmdAesonDecodeProtocolParamsError fpath . Text.pack) . hoistEither $
    Aeson.eitherDecode' pparams

data SomeWitness
  = AByronSigningKey           (Api.SigningKey Api.ByronKey)
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
  | AShelleyMultiSigScript     Api.MultiSigScript

readSomeWitness
  :: SigningKeyOrScriptFile
  -> ExceptT ShelleyTxCmdError IO SomeWitness
readSomeWitness scriptOrSkey =
   case scriptOrSkey of
     SigningKeyFileForWitness fp ->
      firstExceptT ShelleyTxCmdReadSigningKeyFileError $
        readSigningKeyFile (SigningKeyFile fp)
     ScriptFileForWitness fp -> do
       msJson <- handleIOExceptT (ShelleyTxReadFileException . FileIOError fp)
                   $ LBS.readFile fp
       case Aeson.eitherDecode msJson of
         Right ms -> right $ AShelleyMultiSigScript ms
         Left decErr -> left . ShelleyTxAesonDecodeMultiSigScriptError fp $ Text.pack decErr


readSigningKeyFile
  :: SigningKeyFile
  -> ExceptT (Api.FileError SigningKeyDecodeError) IO SomeWitnessSigningKey
readSigningKeyFile skFile =
    newExceptT $
      readSigningKeyFileAnyOf textEnvFileTypes bech32FileTypes skFile
  where
    textEnvFileTypes =
      [ Api.FromSomeType (Api.AsSigningKey Api.AsByronKey)
                          AByronSigningKey
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
                          AByronSigningKey
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
  -> ( [Api.SigningKey Api.ByronKey]
     , [Api.ShelleyWitnessSigningKey]
     , [Api.MultiSigScript]
     )
partitionSomeWitnesses [] = ([], [], [])
partitionSomeWitnesses (wt' : rest') =
    go wt' rest' ([], [], [])
  where
    go wt [] (bwl, skwl, sswl) =
      case wt of
        AByronWitness bw -> (bwl ++ [bw], skwl, sswl)
        AShelleyKeyWitness skw -> (bwl, skwl ++ [skw], sswl)
        AShelleyScriptWitness ssw -> (bwl, skwl, sswl ++ [ssw])
    go wt (next : rest) (bwl, skwl, sswl) =
      case wt of
        AByronWitness bw -> go next rest (bwl ++ [bw], skwl, sswl)
        AShelleyKeyWitness skw -> go next rest (bwl, skwl ++ [skw], sswl)
        AShelleyScriptWitness ssw -> go next rest (bwl, skwl, sswl ++ [ssw])

data ByronOrShelleyWitness
  = AByronWitness !(Api.SigningKey Api.ByronKey)
  | AShelleyKeyWitness !Api.ShelleyWitnessSigningKey
  | AShelleyScriptWitness !Api.MultiSigScript

categoriseSomeWitness :: SomeWitness -> ByronOrShelleyWitness
categoriseSomeWitness swsk =
  case swsk of
    AByronSigningKey           sk -> AByronWitness sk
    APaymentSigningKey         sk -> AShelleyKeyWitness (Api.WitnessPaymentKey         sk)
    APaymentExtendedSigningKey sk -> AShelleyKeyWitness (Api.WitnessPaymentExtendedKey sk)
    AStakeSigningKey           sk -> AShelleyKeyWitness (Api.WitnessStakeKey           sk)
    AStakeExtendedSigningKey   sk -> AShelleyKeyWitness (Api.WitnessStakeExtendedKey   sk)
    AStakePoolSigningKey       sk -> AShelleyKeyWitness (Api.WitnessStakePoolKey       sk)
    AGenesisSigningKey         sk -> AShelleyKeyWitness (Api.WitnessGenesisKey sk)
    AGenesisExtendedSigningKey sk -> AShelleyKeyWitness (Api.WitnessGenesisExtendedKey sk)
    AGenesisDelegateSigningKey sk -> AShelleyKeyWitness (Api.WitnessGenesisDelegateKey sk)
    AGenesisDelegateExtendedSigningKey sk
                                  -> AShelleyKeyWitness (Api.WitnessGenesisDelegateExtendedKey sk)
    AGenesisUTxOSigningKey     sk -> AShelleyKeyWitness (Api.WitnessGenesisUTxOKey     sk)
    AShelleyMultiSigScript scr    -> AShelleyScriptWitness scr

runTxGetTxId :: TxBodyFile -> ExceptT ShelleyTxCmdError IO ()
runTxGetTxId (TxBodyFile txbodyFile) = do
  txbody <- firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
              Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
  liftIO $ BS.putStrLn $ Api.serialiseToRawBytesHex (Api.getTxId txbody)

runTxCreateWitness
  :: TxBodyFile
  -> SigningKeyOrScriptFile
  -> Maybe NetworkId
  -> OutputFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxCreateWitness (TxBodyFile txbodyFile) sKeyOrScript (Just nw) (OutputFile oFile) = do
  txbody <- firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
              Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
  someWit <- readSomeWitness sKeyOrScript

  witness <- case categoriseSomeWitness someWit of
               AShelleyKeyWitness skShelley -> return $ makeShelleyKeyWitness txbody skShelley
               AShelleyScriptWitness scShelley ->
                 return $ makeShelleyScriptWitness (makeMultiSigScript scShelley)
               AByronWitness skByron -> return $ makeShelleyBootstrapWitness nw txbody skByron

  firstExceptT ShelleyTxCmdWriteFileError
    . newExceptT
    $ Api.writeFileTextEnvelope oFile Nothing witness

runTxCreateWitness (TxBodyFile txbodyFile) sKeyOrScript Nothing (OutputFile oFile) = do
  txbody <- firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
              Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
  someWit <- readSomeWitness sKeyOrScript

  witness <- case categoriseSomeWitness someWit of
               AShelleyKeyWitness skShelley -> return $ makeShelleyKeyWitness txbody skShelley
               AShelleyScriptWitness scShelley ->
                 return $ makeShelleyScriptWitness (makeMultiSigScript scShelley)
               AByronWitness _ -> throwError ShelleyTxCmdMissingNetworkId

  firstExceptT ShelleyTxWriteFileError
    . newExceptT
    $ Api.writeFileTextEnvelope oFile Nothing witness

runTxSignWitness
  :: TxBodyFile
  -> [WitnessFile]
  -> OutputFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxSignWitness (TxBodyFile txBodyFile) witnessFiles (OutputFile oFp) = do
    txBody <- firstExceptT ShelleyTxCmdReadTextViewFileError
      . newExceptT
      $ Api.readFileTextEnvelope Api.AsShelleyTxBody txBodyFile
    witnesses <- firstExceptT ShelleyTxCmdReadTextViewFileError
      $ mapM readWitnessFile witnessFiles
    let tx = Api.makeSignedTransaction witnesses txBody
    firstExceptT ShelleyTxCmdWriteFileError
      . newExceptT
      $ Api.writeFileTextEnvelope oFp Nothing tx

readWitnessFile :: WitnessFile -> ExceptT ShelleyTxCmdError IO (Witness Shelley)
readWitnessFile (WitnessFile fp) =
  firstExceptT ShelleyTxReadFileError $ newExceptT (Api.readFileTextEnvelope AsShelleyWitness fp)

runTxBuildMultiSig :: MultiSigScriptObject -> Maybe OutputFile -> ExceptT ShelleyTxCmdError IO ()
runTxBuildMultiSig msso mOutputFile = do
  ms <- convertToMultiSig msso
  case mOutputFile of
    Just (OutputFile outFp) -> liftIO $ LBS.writeFile outFp $ encodePretty ms
    Nothing -> liftIO . C8.putStrLn $ encodePretty ms
 where
  readPaymentVerificationKeys :: [VerificationKeyFile] -> ExceptT ShelleyTxCmdError IO [VerificationKey PaymentKey]
  readPaymentVerificationKeys fps = do
    eVerKeys <- liftIO $ mapM (readFileTextEnvelope (AsVerificationKey AsPaymentKey) . unVerificationKeyFile) fps
    sequence $ map (firstExceptT ShelleyTxReadFileError . hoistEither) eVerKeys

  convertToMultiSig :: MultiSigScriptObject -> ExceptT ShelleyTxCmdError IO MultiSigScript
  convertToMultiSig so =
    case so of
      All payKeyfps -> do payKeys <- readPaymentVerificationKeys payKeyfps
                          right . RequireAllOf $ map (RequireSignature . verificationKeyHash) payKeys
      Any payKeyfps -> do payKeys <- readPaymentVerificationKeys payKeyfps
                          right . RequireAnyOf $ map (RequireSignature . verificationKeyHash) payKeys
      AtLeast req payKeyFps -> do payKeys <- readPaymentVerificationKeys payKeyFps
                                  right . RequireMOf req $ map (RequireSignature . verificationKeyHash) payKeys
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
