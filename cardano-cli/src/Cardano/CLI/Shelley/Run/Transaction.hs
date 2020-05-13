{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( runTransactionCmd
  ) where

import           Cardano.Prelude
import           Cardano.Binary (FromCBOR(..))

import           Cardano.Api
import           Cardano.Config.Shelley.ColdKeys
                   (KeyType(..), KeyRole(..), KeyError(..), renderKeyType)
import           Cardano.Config.TextView
import           Cardano.CLI.Environment (readEnvSocketPath)
import           Cardano.CLI.Ops (CliError (..))

import           Cardano.Config.Types

import           Cardano.CLI.Shelley.Parsers
import           Cardano.Config.Types (CertificateFile (..))

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, hoistEither, newExceptT)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text

import           Shelley.Spec.Ledger.PParams (PParams)


runTransactionCmd :: TransactionCmd -> ExceptT CliError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuildRaw txins txouts ttl fee out certs ->
      runTxBuildRaw txins txouts ttl fee out certs
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
  -> ExceptT CliError IO ()
runTxBuildRaw txins txouts ttl fee (TxBodyFile fpath) certFps = do
  certs <- mapM readShelleyCert certFps
  firstExceptT CardanoApiError
    . newExceptT
    . writeTxUnsigned fpath
    $ buildShelleyTransaction txins txouts ttl fee certs


runTxSign :: TxBodyFile -> [SigningKeyFile] -> Network -> TxFile -> ExceptT CliError IO ()
runTxSign (TxBodyFile infile) skfiles  network (TxFile outfile) = do
    txu <- firstExceptT CardanoApiError . newExceptT $ readTxUnsigned infile
    sks <- readSigningKeyFiles skfiles
    firstExceptT CardanoApiError
      . newExceptT
      . writeTxSigned outfile
      $ signTransaction txu network sks

runTxSubmit :: FilePath -> Network -> ExceptT CliError IO ()
runTxSubmit txFp network = do
  sktFp <- readEnvSocketPath
  signedTx <- firstExceptT CardanoApiError . newExceptT $ readTxSigned txFp
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
  -> ExceptT CliError IO ()
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

readProtocolParameters :: ProtocolParamsFile -> ExceptT CliError IO PParams
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (IOError fpath) $ LBS.readFile fpath
  firstExceptT (AesonDecode fpath . Text.pack) . hoistEither $
    Aeson.eitherDecode' pparams

-- TODO: This should exist in its own module along with
-- a custom error type and an error rendering function.
readShelleyCert :: CertificateFile -> ExceptT CliError IO Certificate
readShelleyCert (CertificateFile fp) =
  firstExceptT ShelleyCertReadError . newExceptT $ readCertificate fp

-- TODO : This is nuts. The 'cardano-api' and 'cardano-config' packages both have functions
-- for reading/writing keys, but they are incompatible.
-- The 'config' version just operates on Shelley only 'SignKey's, but 'api' operates on
-- 'SigningKey's which have a Byron and a Shelley constructor.
readSigningKeyFiles :: [SigningKeyFile] -> ExceptT CliError IO [SigningKey]
readSigningKeyFiles files =
  newExceptT $ do
    xs <- mapM readSigningKeyFile files
    case partitionEithers xs of
      (e:_, _) -> pure $ Left (KeyCliError (ReadSigningKeyError e))
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
