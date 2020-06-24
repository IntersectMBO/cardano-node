{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( ShelleyTxCmdError
  , renderShelleyTxCmdError
  , runTransactionCmd
  ) where

import           Cardano.Prelude

import           Cardano.Api hiding (textShow)
import           Cardano.Api.TextView
import qualified Cardano.Api.Typed as Api
import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath,
                   renderEnvSocketError)

import           Cardano.Config.Types

import           Cardano.CLI.Shelley.Parsers
import           Cardano.Config.Types (CertificateFile (..))

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
                   (bimapExceptT, firstExceptT, handleIOExceptT, hoistEither,
                    left, newExceptT, right)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text

import           Shelley.Spec.Ledger.MetaData (hashMetaData)
import           Shelley.Spec.Ledger.PParams (PParams)


data ShelleyTxCmdError
  = ShelleyTxAesonDecodeProtocolParamsError !FilePath !Text
  | ShelleyTxMetaDataError !FilePath !MetaDataError
  | ShelleyTxSocketEnvError !EnvSocketError
  | ShelleyTxReadProtocolParamsError !FilePath !IOException
  | ShelleyTxReadSignedTxError !ApiError
  | ShelleyTxReadUpdateError !ApiError
  | ShelleyTxReadUnsignedTxError !ApiError
  | ShelleyTxCertReadError !FilePath !ApiError
  | ShelleyTxWriteSignedTxError !ApiError
  | ShelleyTxWriteUnsignedTxError !ApiError
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
    ShelleyTxReadUnsignedTxError apiError ->
      "Error while reading unsigned shelley tx: " <> renderApiError apiError
    ShelleyTxReadSignedTxError apiError ->
      "Error while reading signed shelley tx: " <> renderApiError apiError
    ShelleyTxReadUpdateError apiError ->
      "Error while reading shelley update: " <> renderApiError apiError
    ShelleyTxSocketEnvError envSockErr -> renderEnvSocketError envSockErr
    ShelleyTxAesonDecodeProtocolParamsError fp decErr ->
      "Error while decoding the protocol parameters at: " <> textShow fp <> " Error: " <> textShow decErr
    ShelleyTxCertReadError fp apiErr ->
      "Error reading shelley certificate at: " <> textShow fp <> " Error: " <> renderApiError apiErr
    ShelleyTxWriteSignedTxError apiError ->
      "Error while writing signed shelley tx: " <> renderApiError apiError
    ShelleyTxWriteUnsignedTxError apiError ->
      "Error while writing unsigned shelley tx: " <> renderApiError apiError
    ShelleyTxSubmitError res ->
      "Error while submitting tx: " <> renderTxSubmitResult res
    ShelleyTxReadFileError fileErr -> Text.pack (Api.displayError fileErr)

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
  :: [TxIn]
  -> [TxOut]
  -> SlotNo
  -> Lovelace
  -> [CertificateFile]
  -> Withdrawals
  -> Maybe MetaDataFile
  -> Maybe UpdateProposalFile
  -> TxBodyFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuildRaw txins txouts ttl fee certFps wdrls mMetaData mUpdateProp (TxBodyFile fpath) = do
  certs <- mapM readShelleyCert certFps
  mUpProp <- maybeUpdate mUpdateProp
  mDataHash <- maybeMetaData mMetaData
  firstExceptT ShelleyTxWriteUnsignedTxError
    . newExceptT
    . writeTxUnsigned fpath
    $ buildShelleyTransaction txins txouts ttl fee certs wdrls mUpProp mDataHash
  where
    maybeMetaData :: Maybe MetaDataFile -> ExceptT ShelleyTxCmdError IO (Maybe ShelleyMetaDataHash)
    maybeMetaData (Just (MetaDataFile mFp)) =
      bimapExceptT (ShelleyTxMetaDataError mFp) (Just . hashMetaData) . newExceptT $ readJSONMetaData mFp
    maybeMetaData Nothing = right Nothing


    maybeUpdate :: Maybe UpdateProposalFile -> ExceptT ShelleyTxCmdError IO (Maybe Update)
    maybeUpdate (Just (UpdateProposalFile uFp )) =
      bimapExceptT ShelleyTxReadUpdateError Just . newExceptT $ readUpdate uFp
    maybeUpdate Nothing = right Nothing

runTxSign :: TxBodyFile -> [SigningKeyFile] -> Network -> TxFile -> ExceptT ShelleyTxCmdError IO ()
runTxSign (TxBodyFile infile) skfiles  network (TxFile outfile) = do
    txu <- firstExceptT ShelleyTxReadUnsignedTxError . newExceptT $ readTxUnsigned infile
    sks <- readSigningKeyFiles skfiles
    firstExceptT ShelleyTxWriteSignedTxError
      . newExceptT
      . writeTxSigned outfile
      $ signTransaction txu network (map toOldApiSigningKey sks)

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
    skeys <- readSigningKeyFiles skFiles
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


data SomeSigningKey
  = ApiPaymentSigningKey !(Api.SigningKey Api.PaymentKey)
  | ApiGenesisUTxOSigningKey !(Api.SigningKey Api.GenesisUTxOKey)
  | ApiStakePoolSigningKey !(Api.SigningKey Api.StakePoolKey)

readSigningKeyFiles :: [SigningKeyFile] -> ExceptT ShelleyTxCmdError IO [SomeSigningKey]
readSigningKeyFiles files =
  newExceptT $ do
    xs <- mapM readSigningKeyFile files
    case partitionEithers xs of
      -- TODO: Would be nice to also provide a filepath to the signing key
      -- resulting in the error.
      (e:_, _) -> pure $ Left (ShelleyTxReadFileError e)
      ([], ys) -> pure $ Right ys

readSigningKeyFile
  :: SigningKeyFile
  -> IO (Either (Api.FileError Api.TextEnvelopeError) SomeSigningKey)
readSigningKeyFile (SigningKeyFile skfile) =
    Api.readFileTextEnvelopeAnyOf fileTypes skfile
  where
    fileTypes =
      [ Api.FromSomeType (Api.AsSigningKey Api.AsPaymentKey) ApiPaymentSigningKey
      -- TODO: Byron signing key
      , Api.FromSomeType (Api.AsSigningKey Api.AsGenesisUTxOKey) ApiGenesisUTxOSigningKey
      , Api.FromSomeType (Api.AsSigningKey Api.AsStakePoolKey) ApiStakePoolSigningKey
      ]

-- | Convert a 'SomeSigningKey' (which wraps a new-style typed API signing
-- key) to a 'SigningKey' (old-style API signing key).
toOldApiSigningKey :: SomeSigningKey -> SigningKey
toOldApiSigningKey someSKey =
  case someSKey of
    ApiPaymentSigningKey (Api.PaymentSigningKey sk) ->
      SigningKeyShelley sk
    ApiGenesisUTxOSigningKey (Api.GenesisUTxOSigningKey sk) ->
      SigningKeyShelley sk
    ApiStakePoolSigningKey (Api.StakePoolSigningKey sk) ->
      SigningKeyShelley sk


runTxGetTxId :: TxBodyFile -> ExceptT ShelleyTxCmdError IO ()
runTxGetTxId (TxBodyFile infile) = do
    tx <- firstExceptT ShelleyTxReadUnsignedTxError . newExceptT $
      readTxUnsigned infile
    liftIO $ putStrLn $ renderTxId (getTransactionId tx)
