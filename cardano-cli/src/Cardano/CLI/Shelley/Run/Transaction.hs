{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

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

import           Cardano.Config.Protocol (mkConsensusProtocol)
import           Cardano.Config.Types
import           Cardano.CLI.Ops (withIOManagerE)

import           Cardano.CLI.Shelley.Parsers
import qualified Ouroboros.Consensus.Cardano as Consensus
import           Ouroboros.Consensus.Node.ProtocolInfo (pInfoConfig)
import           Cardano.Config.Types (CertificateFile (..))


import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT)
import           Control.Tracer (nullTracer)

runTransactionCmd :: TransactionCmd -> ExceptT CliError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuildRaw txins txouts ttl fee out certs ->
      runTxBuildRaw txins txouts ttl fee out certs
    TxSign txinfile skfiles network txoutfile ->
      runTxSign txinfile skfiles network txoutfile
    TxSubmit txFp configFp ->
      runTxSubmit txFp configFp

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
 where
   -- TODO: This should exist in its own module along with
   -- a custom error type and an error rendering function.
   readShelleyCert :: CertificateFile -> ExceptT CliError IO Certificate
   readShelleyCert (CertificateFile fp) =
      firstExceptT ShelleyCertReadError . newExceptT $ readCertificate fp


runTxSign :: TxBodyFile -> [SigningKeyFile] -> Network -> TxFile -> ExceptT CliError IO ()
runTxSign (TxBodyFile infile) skfiles  network (TxFile outfile) = do
    txu <- firstExceptT CardanoApiError . newExceptT $ readTxUnsigned infile
    sks <- readSigningKeyFiles skfiles
    firstExceptT CardanoApiError
      . newExceptT
      . writeTxSigned outfile
      $ signTransaction txu network sks

runTxSubmit :: FilePath -> ConfigYamlFilePath -> ExceptT CliError IO ()
runTxSubmit txFp configFp =
  withIOManagerE $ \iocp -> do
    sktFp <- readEnvSocketPath
    nc <- liftIO $ parseNodeConfigurationFP configFp
    SomeConsensusProtocol p <- firstExceptT ProtocolError $ mkConsensusProtocol nc Nothing
    signedTx <- firstExceptT CardanoApiError . newExceptT $ readTxSigned txFp
    case p of
      Consensus.ProtocolRealTPraos{} -> do
        let config = pInfoConfig $ Consensus.protocolInfo p
        liftIO $ submitTx
                   nullTracer -- tracer needed
                   iocp
                   config
                   sktFp
                   (prepareTxShelley signedTx)
      _ -> left $ IncorrectProtocolSpecifiedError (ncProtocol nc)



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
