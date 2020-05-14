{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( ShelleyTxCmdError
  , runTransactionCmd
  ) where

import           Cardano.Prelude
import           Cardano.Binary (FromCBOR(..))

import           Cardano.Api
import           Cardano.Config.Shelley.ColdKeys
                   (KeyType(..), KeyRole(..), KeyError(..), renderKeyType)
import           Cardano.Config.TextView
import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath)

import           Cardano.Config.Types hiding (Update)

import           Cardano.CLI.Shelley.Parsers
import           Cardano.Config.Types (CertificateFile (..))

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
                   (bimapExceptT, firstExceptT, handleIOExceptT, hoistEither,
                    newExceptT, right)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text

import           Shelley.Spec.Ledger.PParams (PParams)


data ShelleyTxCmdError
  = ShelleyTxSocketEnvError !EnvSocketError
  | ShelleyTxCardanoApiError !ApiError
  | ShelleyTxCardanoIOError !FilePath !IOException
  | ShelleyTxCardanoAesonDecode !FilePath !Text
  | ShelleyTxCertReadError !ApiError
  | ShelleyTxTxKeyError !KeyError
  deriving Show



runTransactionCmd :: TransactionCmd -> ExceptT ShelleyTxCmdError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuildRaw txins txouts ttl fee out certs mUpProp ->
      runTxBuildRaw txins txouts ttl fee out certs mUpProp
    TxSign txinfile skfiles network txoutfile ->
      runTxSign txinfile skfiles network txoutfile
    TxSubmit txFp network ->
      runTxSubmit txFp network
    TxCalculateMinFee txInCount txOutCount ttl network skFiles certFiles pParamsFile ->
      runTxCalculateMinFee txInCount txOutCount ttl network skFiles certFiles pParamsFile

    _ -> liftIO $ putStrLn $ "runTransactionCmd: " ++ show cmd

runTxBuildRaw
  :: [TxIn]
  -> [TxOut]
  -> SlotNo
  -> Lovelace
  -> TxBodyFile
  -> [CertificateFile]
  -> Maybe UpdateProposalFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuildRaw txins txouts ttl fee (TxBodyFile fpath) certFps mUpdateProp  = do
  certs <- mapM readShelleyCert certFps
  upUpProp <- maybeUpdate mUpdateProp
  firstExceptT ShelleyTxCardanoApiError
    . newExceptT
    . writeTxUnsigned fpath
    $ buildShelleyTransaction txins txouts ttl fee certs upUpProp
  where
    maybeUpdate :: Maybe UpdateProposalFile -> ExceptT ShelleyTxCmdError IO (Maybe Update)
    maybeUpdate (Just (UpdateProposalFile uFp )) = bimapExceptT ShelleyTxCardanoApiError Just . newExceptT $ readUpdate uFp
    maybeUpdate Nothing = right Nothing

runTxSign :: TxBodyFile -> [SigningKeyFile] -> Network -> TxFile -> ExceptT ShelleyTxCmdError IO ()
runTxSign (TxBodyFile infile) skfiles  network (TxFile outfile) = do
    txu <- firstExceptT ShelleyTxCardanoApiError . newExceptT $ readTxUnsigned infile
    sks <- readSigningKeyFiles skfiles
    firstExceptT ShelleyTxCardanoApiError
      . newExceptT
      . writeTxSigned outfile
      $ signTransaction txu network sks

runTxSubmit :: FilePath -> Network -> ExceptT ShelleyTxCmdError IO ()
runTxSubmit txFp network = do
  sktFp <- firstExceptT ShelleyTxSocketEnvError $ readEnvSocketPath
  signedTx <- firstExceptT ShelleyTxCardanoApiError . newExceptT $ readTxSigned txFp
  result   <- liftIO $ submitTx network sktFp signedTx
  case result of
    TxSubmitSuccess            -> return ()
    --TODO: use the Cardano.Api.TxSubmit.ErrorRender here
    TxSubmitFailureByron   err -> liftIO $ print err
    TxSubmitFailureShelley err -> liftIO $ print err

runTxCalculateMinFee
  :: TxInCount
  -> TxOutCount
  -> SlotNo
  -> Network
  -> [SigningKeyFile]
  -> [CertificateFile]
  -> ProtocolParamsFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxCalculateMinFee (TxInCount txInCount) (TxOutCount txOutCount)
                     txTtl network skFiles certFiles pParamsFile = do
    skeys <- readSigningKeyFiles skFiles
    certs <- mapM readShelleyCert certFiles
    pparams <- readProtocolParameters pParamsFile
    liftIO $ putStrLn
      $  "runTxCalculateMinFee: "
      ++ show (calculateShelleyMinFee pparams (dummyShelleyTxForFeeCalc skeys certs))
  where
    dummyShelleyTxForFeeCalc skeys certs =
      buildDummyShelleyTxForFeeCalc txInCount txOutCount txTtl network skeys certs

readProtocolParameters :: ProtocolParamsFile -> ExceptT ShelleyTxCmdError IO PParams
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (ShelleyTxCardanoIOError fpath) $ LBS.readFile fpath
  firstExceptT (ShelleyTxCardanoAesonDecode fpath . Text.pack) . hoistEither $
    Aeson.eitherDecode' pparams

-- TODO: This should exist in its own module along with
-- a custom error type and an error rendering function.
readShelleyCert :: CertificateFile -> ExceptT ShelleyTxCmdError IO Certificate
readShelleyCert (CertificateFile fp) =
  firstExceptT ShelleyTxCertReadError . newExceptT $ readCertificate fp

-- TODO : This is nuts. The 'cardano-api' and 'cardano-config' packages both have functions
-- for reading/writing keys, but they are incompatible.
-- The 'config' version just operates on Shelley only 'SignKey's, but 'api' operates on
-- 'SigningKey's which have a Byron and a Shelley constructor.
readSigningKeyFiles :: [SigningKeyFile] -> ExceptT ShelleyTxCmdError IO [SigningKey]
readSigningKeyFiles files =
  newExceptT $ do
    xs <- mapM readSigningKeyFile files
    case partitionEithers xs of
      (e:_, _) -> pure $ Left (ShelleyTxTxKeyError (ReadSigningKeyError e))
      ([], ys) -> pure $ Right ys

readSigningKeyFile :: SigningKeyFile -> IO (Either TextViewFileError SigningKey)
readSigningKeyFile (SigningKeyFile skfile) =
      readTextViewEncodedFile decodeAddressSigningKey skfile

-- The goal here is to read either a Byron or Shelley address signing key or a
-- Genesis UTxO signing key. The TextView provides functions to read one of
-- several file types, however this does not currently mesh well with the
-- key reading functions from the API package.
--
-- The API package provides parseSigningKeyView but this takes the whole
-- ByteString textview, rather than the structured TextView, so we cannot
-- compose that with readTextViewEncodedFile. It provides the lower level
-- signingKeyFromCBOR which returns too big an error type to fit here, so
-- we have to fudge it with a partial conversion.
decodeAddressSigningKey :: TextView -> Either TextViewError SigningKey
decodeAddressSigningKey tView = do
    isGenesisUTxOKey <- expectTextViewOfTypes fileTypes tView
    if isGenesisUTxOKey
      then decodeFromTextView (SigningKeyShelley <$> fromCBOR) tView
      else first (\(ApiTextView tve) -> tve)
                 (signingKeyFromCBOR (tvRawCBOR tView))
  where
    fileTypes =
      [ ("SigningKeyShelley", False)
      , ("SigningKeyByron",   False)
      , (renderKeyType (KeyTypeSigning GenesisUTxOKey), True) ]
