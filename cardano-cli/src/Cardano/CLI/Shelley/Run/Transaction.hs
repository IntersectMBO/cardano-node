{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( ShelleyTxCmdError
  , renderShelleyTxCmdError
  , runTransactionCmd
  ) where

import           Cardano.Prelude

import           Cardano.Api hiding (textShow)
import qualified Cardano.Api.Typed as Api
import           Cardano.Api.TextView
import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath,
                   renderEnvSocketError)

import           Cardano.Config.Types

import           Cardano.CLI.Shelley.Parsers
import           Cardano.Config.Types (CertificateFile (..))

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, hoistEither,
                    left, newExceptT)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
--import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

--import qualified Shelley.Spec.Ledger.Address as Shelley
--import qualified Ouroboros.Consensus.Shelley.Protocol.Crypto as Shelley
--import           Shelley.Spec.Ledger.MetaData (hashMetaData)
import           Shelley.Spec.Ledger.PParams (PParams)
--import qualified Shelley.Spec.Ledger.PParams as Shelley


data ShelleyTxCmdError
  = ShelleyTxAesonDecodeProtocolParamsError !FilePath !Text
  | ShelleyTxMetaDataError !FilePath !MetaDataError
  | ShelleyTxMissingNetworkId
  | ShelleyTxSocketEnvError !EnvSocketError
  | ShelleyTxReadProtocolParamsError !FilePath !IOException
  | ShelleyTxReadSignedTxError !ApiError
  | ShelleyTxReadUpdateError !ApiError
  | ShelleyTxReadUnsignedTxError !(Api.FileError Api.TextEnvelopeError)
  | ShelleyTxCertReadError !FilePath !ApiError
  | ShelleyTxCertReadError' !(Api.FileError Api.TextEnvelopeError)
  | ShelleyTxWriteSignedTxError !(Api.FileError ())
  | ShelleyTxWriteUnsignedTxError !(Api.FileError ())
  | ShelleyTxSubmitError !TxSubmitResult
  | ShelleyTxReadFileError !(Api.FileError Api.TextEnvelopeError)
  deriving Show

renderShelleyTxCmdError :: ShelleyTxCmdError -> Text
renderShelleyTxCmdError err =
  case err of
    ShelleyTxReadProtocolParamsError fp ioException ->
      "Error while reading protocol parameters at: " <> textShow fp <> " Error: " <> textShow ioException
    ShelleyTxMetaDataError fp metaDataErr ->
       "Error reading metadata at: " <> textShow fp <> " Error: " <> renderMetaDataError metaDataErr
    ShelleyTxReadUnsignedTxError err' ->
      "Error while reading unsigned shelley tx: " <> Text.pack (Api.displayError err')
    ShelleyTxReadSignedTxError apiError ->
      "Error while reading signed shelley tx: " <> renderApiError apiError
    ShelleyTxReadUpdateError apiError ->
      "Error while reading shelley update: " <> renderApiError apiError
    ShelleyTxSocketEnvError envSockErr -> renderEnvSocketError envSockErr
    ShelleyTxAesonDecodeProtocolParamsError fp decErr ->
      "Error while decoding the protocol parameters at: " <> textShow fp <> " Error: " <> textShow decErr
    ShelleyTxCertReadError fp apiErr ->
      "Error reading shelley certificate at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyTxCertReadError' err' ->
      "Error reading shelley certificate at: " <> Text.pack (Api.displayError err')
    ShelleyTxWriteSignedTxError err' ->
      "Error while writing signed shelley tx: " <> Text.pack (Api.displayError err')
    ShelleyTxWriteUnsignedTxError err' ->
      "Error while writing unsigned shelley tx: " <> Text.pack (Api.displayError err')
    ShelleyTxSubmitError res ->
      "Error while submitting tx: " <> renderTxSubmitResult res
    ShelleyTxReadFileError fileErr -> Text.pack (Api.displayError fileErr)
    ShelleyTxMissingNetworkId -> "Please enter network id with your byron transaction"

runTransactionCmd :: TransactionCmd -> ExceptT ShelleyTxCmdError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuildRaw txins txouts ttl fee certs wdrls mMetaData mUpProp out ->
      runTxBuildRaw txins txouts ttl fee certs wdrls mMetaData mUpProp out
    TxSign txinfile skfiles network txoutfile ->
      runTxSign txinfile skfiles network txoutfile
    TxSubmit txFp network ->
      runTxSubmit txFp network
    TxCalculateMinFee txInCount txOutCount ttl network skFiles certFiles wdrls hasMD pParamsFile ->
      runTxCalculateMinFee txInCount txOutCount ttl network skFiles certFiles wdrls hasMD pParamsFile
    TxGetTxId txinfile ->
      runTxGetTxId txinfile

    _ -> liftIO $ putStrLn $ "runTransactionCmd: " ++ show cmd

runTxBuildRaw
  :: [Api.TxIn]
  -> [Api.TxOut Api.Shelley]
  -> SlotNo
  -> Api.Lovelace
  -> [CertificateFile]
  -> Withdrawals
  -> Maybe MetaDataFile
  -> Maybe UpdateProposalFile
  -> TxBodyFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuildRaw txins txouts ttl fee
              certFiles _wdrls _mMetaData@Nothing _mUpdateProp@Nothing
              (TxBodyFile fpath) = do

    --TODO: reinstate withdrawal, metadata and protocol updates
    --mUpProp <- maybeUpdate mUpdateProp
    --txMetadata <- maybeMetaData mMetaData

    certs <- sequence
               [ firstExceptT ShelleyTxCertReadError' . newExceptT $
                   Api.readFileTextEnvelope Api.AsCertificate certFile
               | CertificateFile certFile <- certFiles ]

    let txBody = Api.makeShelleyTransaction
                   Api.txExtraContentEmpty {
                     Api.txCertificates = certs
                   }
                   ttl
                   fee
                   txins
                   txouts

    firstExceptT ShelleyTxWriteUnsignedTxError
      . newExceptT
      $ Api.writeFileTextEnvelope fpath Nothing txBody

runTxBuildRaw _ _ _ _ _ _ _ _ _ =
    panic "TODO: reinstate support for withdrawals, certificates, metadata and protocol updates"

runTxSign :: TxBodyFile
          -> [SigningKeyFile]
          -> Maybe Api.NetworkId
          -> TxFile
          -> ExceptT ShelleyTxCmdError IO ()
runTxSign (TxBodyFile txbodyFile) skFiles mnw (TxFile txFile) = do
    txbody <- firstExceptT ShelleyTxReadUnsignedTxError . newExceptT $
                Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
    sks    <- firstExceptT ShelleyTxReadFileError $
                mapM readSigningKeyFile skFiles

    -- We have to handle Byron and Shelley key witnesses slightly differently
    let (sksByron, sksShelley) = partitionEithers (map categoriseSigningKey sks)

    -- Byron witnesses need the network id
    witnessesByron <-
      case (sksByron, mnw) of
        ([], Nothing) -> return []
        (_,  Nothing) -> throwError ShelleyTxMissingNetworkId
        (_,  Just nw) ->
          return $ map (Api.makeShelleyBootstrapWitness nw txbody) sksByron

    let witnesses :: [Api.Witness Api.Shelley]
        witnesses = witnessesByron
                 ++ map (Api.makeShelleyKeyWitness txbody) sksShelley

        tx        :: Api.Tx Api.Shelley
        tx        = Api.makeSignedTransaction witnesses txbody

    firstExceptT ShelleyTxWriteSignedTxError . newExceptT $
      Api.writeFileTextEnvelope txFile Nothing tx
  where
    categoriseSigningKey :: SomeWitnessSigningKey
                         -> Either (Api.SigningKey Api.ByronKey)
                                    Api.ShelleyWitnessSigningKey
    categoriseSigningKey swsk =
      case swsk of
        AByronSigningKey           sk -> Left sk
        APaymentSigningKey         sk -> Right (Api.WitnessPaymentKey         sk)
        AStakeSigningKey           sk -> Right (Api.WitnessStakeKey           sk)
        AStakePoolSigningKey       sk -> Right (Api.WitnessStakePoolKey       sk)
        AGenesisDelegateSigningKey sk -> Right (Api.WitnessGenesisDelegateKey sk)
        AGenesisUTxOSigningKey     sk -> Right (Api.WitnessGenesisUTxOKey     sk)


runTxSubmit :: FilePath -> Network -> ExceptT ShelleyTxCmdError IO ()
runTxSubmit txFp network = do
  sktFp <- firstExceptT ShelleyTxSocketEnvError $ readEnvSocketPath
  signedTx <- firstExceptT ShelleyTxReadSignedTxError . newExceptT $ readTxSigned txFp
  result   <- liftIO $ submitTx network sktFp signedTx
  case result of
    TxSubmitSuccess          -> return ()
    TxSubmitFailureShelley _ -> left (ShelleyTxSubmitError result)
    TxSubmitFailureByron   _ -> left (ShelleyTxSubmitError result)

runTxCalculateMinFee
  :: TxInCount
  -> TxOutCount
  -> SlotNo
  -> Network
  -> [SigningKeyFile]
  -> [CertificateFile]
  -> Withdrawals
  -> HasMetaData
  -> ProtocolParamsFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxCalculateMinFee (TxInCount txInCount) (TxOutCount txOutCount)
                     txTtl network skFiles certFiles wdrls hasMD
                     pParamsFile = do
    skeys <- firstExceptT ShelleyTxReadFileError $
               mapM readSigningKeyFile skFiles
    certs <- mapM readShelleyCert certFiles
    pparams <- readProtocolParameters pParamsFile
    liftIO $ putStrLn
      $  "runTxCalculateMinFee: "
      ++ show (calculateShelleyMinFee pparams (dummyShelleyTxForFeeCalc skeys certs))
  where
    dummyShelleyTxForFeeCalc skeys certs =
      buildDummyShelleyTxForFeeCalc
        txInCount
        txOutCount
        txTtl
        network
        (map toOldApiSigningKey skeys)
        certs
        wdrls
        hasMD

readProtocolParameters :: ProtocolParamsFile -> ExceptT ShelleyTxCmdError IO PParams
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (ShelleyTxReadProtocolParamsError fpath) $ LBS.readFile fpath
  firstExceptT (ShelleyTxAesonDecodeProtocolParamsError fpath . Text.pack) . hoistEither $
    Aeson.eitherDecode' pparams

-- TODO: This should exist in its own module along with
-- a custom error type and an error rendering function.
readShelleyCert :: CertificateFile -> ExceptT ShelleyTxCmdError IO Certificate
readShelleyCert (CertificateFile fp) =
  firstExceptT (ShelleyTxCertReadError fp) . newExceptT $ readCertificate fp


data SomeWitnessSigningKey
  = AByronSigningKey           (Api.SigningKey Api.ByronKey)
  | APaymentSigningKey         (Api.SigningKey Api.PaymentKey)
  | AStakeSigningKey           (Api.SigningKey Api.StakeKey)
  | AStakePoolSigningKey       (Api.SigningKey Api.StakePoolKey)
  | AGenesisDelegateSigningKey (Api.SigningKey Api.GenesisDelegateKey)
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
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakeKey)
                          AStakeSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakePoolKey)
                          AStakePoolSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisDelegateKey)
                          AGenesisDelegateSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisUTxOKey)
                          AGenesisUTxOSigningKey
      ]

-- | Convert a 'SomeSigningKey' (which wraps a new-style typed API signing
-- key) to a 'SigningKey' (old-style API signing key).
toOldApiSigningKey :: SomeWitnessSigningKey -> SigningKey
toOldApiSigningKey someSKey =
  case someSKey of
    AByronSigningKey (Api.ByronSigningKey sk) ->
      SigningKeyByron sk
    APaymentSigningKey (Api.PaymentSigningKey sk) ->
      SigningKeyShelley sk
    AStakeSigningKey (Api.StakeSigningKey sk) ->
      SigningKeyShelley sk
    AStakePoolSigningKey (Api.StakePoolSigningKey sk) ->
      SigningKeyShelley sk
    AGenesisDelegateSigningKey (Api.GenesisDelegateSigningKey sk) ->
      SigningKeyShelley sk
    AGenesisUTxOSigningKey (Api.GenesisUTxOSigningKey sk) ->
      SigningKeyShelley sk


runTxGetTxId :: TxBodyFile -> ExceptT ShelleyTxCmdError IO ()
runTxGetTxId (TxBodyFile txbodyFile) = do
    txbody <- firstExceptT ShelleyTxReadUnsignedTxError . newExceptT $
                Api.readFileTextEnvelope Api.AsShelleyTxBody txbodyFile
    liftIO $ BS.putStrLn $ Api.serialiseToRawBytesHex (Api.getTxId txbody)
