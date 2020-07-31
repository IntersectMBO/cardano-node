{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( ShelleyTxCmdError
  , renderShelleyTxCmdError
  , runTransactionCmd
  ) where

import           Cardano.Prelude
import           Prelude (String)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left,
                     newExceptT)

--TODO: do this nicely via the API too:
import qualified Cardano.Binary as CBOR

import qualified Shelley.Spec.Ledger.PParams as Shelley

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..),
                     HardForkApplyTxErr (ApplyTxErrByron, ApplyTxErrShelley, ApplyTxErrWrongEra))
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
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
  | ShelleyTxCmdWriteFileError !(FileError ())
  | ShelleyTxCmdMetaDataConversionError !FilePath !MetaDataJsonConversionError
  | ShelleyTxCmdMetaDecodeError !FilePath !CBOR.DecoderError
  | ShelleyTxCmdMissingNetworkId
  | ShelleyTxCmdSocketEnvError !EnvSocketError
  | ShelleyTxCmdTxSubmitErrorByron !(ApplyTxErr ByronBlock)
  | ShelleyTxCmdTxSubmitErrorShelley !(ApplyTxErr (ShelleyBlock TPraosStandardCrypto))
  | ShelleyTxCmdTxSubmitErrorEraMismatch !EraMismatch
  deriving Show

renderShelleyTxCmdError :: ShelleyTxCmdError -> Text
renderShelleyTxCmdError err =
  case err of
    ShelleyTxCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdReadTextViewFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdMetaDataConversionError fp metaDataErr ->
       "Error reading metadata at: " <> show fp
                       <> " Error: " <> renderMetaDataJsonConversionError metaDataErr
    ShelleyTxCmdMetaDecodeError fp metaDataErr ->
       "Error decoding CBOR metadata at: " <> show fp
                             <> " Error: " <> show metaDataErr
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
    ShelleyTxCmdMissingNetworkId -> "Please enter network id with your byron transaction"

runTransactionCmd :: TransactionCmd -> ExceptT ShelleyTxCmdError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuildRaw txins txouts ttl fee certs wdrls mMetaData mUpProp out ->
      runTxBuildRaw txins txouts ttl fee certs wdrls mMetaData mUpProp out
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
    TxWitness txBodyfile witSignKeyFile mbNw outFile ->
      runTxWitness txBodyfile witSignKeyFile mbNw outFile
    TxSignWitness txBodyFile witnessFile outFile ->
      runTxSignWitness txBodyFile witnessFile outFile

runTxBuildRaw
  :: [Api.TxIn]
  -> [Api.TxOut Api.Shelley]
  -> SlotNo
  -> Api.Lovelace
  -> [CertificateFile]
  -> [(Api.StakeAddress, Api.Lovelace)]
  -> [MetaDataFile]
  -> Maybe UpdateProposalFile
  -> TxBodyFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuildRaw txins txouts ttl fee
              certFiles withdrawals metaDataFiles mUpdatePropFile
              (TxBodyFile fpath) = do

    certs <- sequence
               [ firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                   Api.readFileTextEnvelope Api.AsCertificate certFile
               | CertificateFile certFile <- certFiles ]


    mMetaData <- case metaDataFiles of
      []    -> return Nothing
      files -> Just . mconcat <$> mapM readFileTxMetaData files
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
runTxSign (TxBodyFile txbodyFile) skFiles mnw (TxFile txFile) = do
    txbody <- firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
    sks    <- firstExceptT ShelleyTxCmdReadTextViewFileError $
                mapM readSigningKeyFile skFiles

    -- We have to handle Byron and Shelley key witnesses slightly differently
    let (sksByron, sksShelley) = partitionEithers (map categoriseWitnessSigningKey sks)

    -- Byron witnesses need the network id
    witnessesByron <-
      case (sksByron, mnw) of
        ([], Nothing) -> return []
        (_,  Nothing) -> throwError ShelleyTxCmdMissingNetworkId
        (_,  Just nw) ->
          return $ map (Api.makeShelleyBootstrapWitness nw txbody) sksByron

    let witnesses :: [Api.Witness Api.Shelley]
        witnesses = witnessesByron
                 ++ map (Api.makeShelleyKeyWitness txbody) sksShelley

        tx        :: Api.Tx Api.Shelley
        tx        = Api.makeSignedTransaction witnesses txbody

    firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
      Api.writeFileTextEnvelope txFile Nothing tx

runTxSubmit :: Protocol -> NetworkId -> FilePath
            -> ExceptT ShelleyTxCmdError IO ()
runTxSubmit protocol network txFile = do
    SocketPath sockPath <- firstExceptT ShelleyTxCmdSocketEnvError $ readEnvSocketPath
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
                       -> ExceptT ShelleyTxCmdError IO Shelley.PParams
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (ShelleyTxCmdAesonDecodeProtocolParamsError fpath . Text.pack) . hoistEither $
    Aeson.eitherDecode' pparams

data SomeWitnessSigningKey
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

readSigningKeyFile
  :: SigningKeyFile
  -> ExceptT (Api.FileError Api.TextEnvelopeError) IO SomeWitnessSigningKey
readSigningKeyFile (SigningKeyFile skfile) =
    newExceptT $
      Api.readFileTextEnvelopeAnyOf fileTypes skfile
  where
    fileTypes =
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

categoriseWitnessSigningKey :: SomeWitnessSigningKey
                            -> Either (Api.SigningKey Api.ByronKey)
                                       Api.ShelleyWitnessSigningKey
categoriseWitnessSigningKey swsk =
  case swsk of
    AByronSigningKey           sk -> Left sk
    APaymentSigningKey         sk -> Right (Api.WitnessPaymentKey         sk)
    APaymentExtendedSigningKey sk -> Right (Api.WitnessPaymentExtendedKey sk)
    AStakeSigningKey           sk -> Right (Api.WitnessStakeKey           sk)
    AStakeExtendedSigningKey   sk -> Right (Api.WitnessStakeExtendedKey   sk)
    AStakePoolSigningKey       sk -> Right (Api.WitnessStakePoolKey       sk)
    AGenesisSigningKey         sk -> Right (Api.WitnessGenesisKey sk)
    AGenesisExtendedSigningKey sk -> Right (Api.WitnessGenesisExtendedKey sk)
    AGenesisDelegateSigningKey sk -> Right (Api.WitnessGenesisDelegateKey sk)
    AGenesisDelegateExtendedSigningKey sk
                                  -> Right (Api.WitnessGenesisDelegateExtendedKey sk)
    AGenesisUTxOSigningKey     sk -> Right (Api.WitnessGenesisUTxOKey     sk)

runTxGetTxId :: TxBodyFile -> ExceptT ShelleyTxCmdError IO ()
runTxGetTxId (TxBodyFile txbodyFile) = do
  txbody <- firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
              Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
  liftIO $ BS.putStrLn $ Api.serialiseToRawBytesHex (Api.getTxId txbody)

runTxWitness
  :: TxBodyFile
  -> SigningKeyFile
  -> Maybe NetworkId
  -> OutputFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxWitness (TxBodyFile txbodyFile) witSignKeyFile mbNw (OutputFile oFile) = do
  txbody <- firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
              Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
  someWitSignKey <- firstExceptT ShelleyTxCmdReadTextViewFileError $ readSigningKeyFile witSignKeyFile

  witness <-
    case (categoriseWitnessSigningKey someWitSignKey, mbNw) of
      -- Byron witnesses require the network ID.
      (Left _, Nothing) -> throwError ShelleyTxCmdMissingNetworkId
      (Left byronSk, Just nw) -> pure $ makeShelleyBootstrapWitness nw txbody byronSk
      (Right shelleySk, _) -> pure $ makeShelleyKeyWitness txbody shelleySk

  firstExceptT ShelleyTxCmdWriteFileError
    . newExceptT
    $ Api.writeFileTextEnvelope oFile Nothing witness

runTxSignWitness :: TxBodyFile -> [WitnessFile] -> OutputFile -> ExceptT ShelleyTxCmdError IO ()
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
  where
    readWitnessFile
      :: WitnessFile
      -> ExceptT (Api.FileError Api.TextEnvelopeError) IO (Witness Shelley)
    readWitnessFile (WitnessFile fp) =
      newExceptT (Api.readFileTextEnvelope AsShelleyWitness fp)

-- ----------------------------------------------------------------------------
-- Transaction metadata
--

readFileTxMetaData :: MetaDataFile
                   -> ExceptT ShelleyTxCmdError IO Api.TxMetadata
readFileTxMetaData (MetaDataFileJSON fp) = do
    bs <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fp) $
          LBS.readFile fp
    v  <- firstExceptT (ShelleyTxCmdMetaDataConversionError fp . ConversionErrDecodeJSON) $
          hoistEither $
            Aeson.eitherDecode' bs
    firstExceptT (ShelleyTxCmdMetaDataConversionError fp) $ hoistEither $
      jsonToMetadata v
readFileTxMetaData (MetaDataFileCBOR fp) = do
    bs <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fp) $
          BS.readFile fp
    firstExceptT (ShelleyTxCmdMetaDecodeError fp) $ hoistEither $
      Api.deserialiseFromCBOR Api.AsTxMetadata bs
