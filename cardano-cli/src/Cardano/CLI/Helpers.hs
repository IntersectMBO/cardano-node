{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Helpers
  ( HelpersError(..)
  , ensureNewFile
  , ensureNewFileLBS
  , pPrintCBOR
  , readCBOR
  , renderHelpersError
  , serialiseSigningKey
  , validateCBOR
  , writeProtocolParameters
  ) where

import           Cardano.Prelude

import           Codec.CBOR.Pretty (prettyHexEnc)
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Term (decodeTerm, encodeTerm)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad.Trans.Except.Extra (handleIOExceptT, left)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text
import           System.Directory (doesPathExist)

import           Cardano.Binary (Decoder, fromCBOR)
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Update as Update
import           Cardano.Chain.Block (fromCBORABlockOrBoundary)
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.CLI.Shelley.Commands (OutputFile(..))
import           Cardano.Config.Protocol (CardanoEra(..))
import           Cardano.Config.Types
import           Cardano.Crypto (SigningKey(..))
import qualified Cardano.Crypto as Crypto

import           Shelley.Spec.Ledger.PParams (PParams)

data HelpersError
  = CardanoEraNotSupportedFail !CardanoEra
  | CBORPrettyPrintError !DeserialiseFailure
  | CBORDecodingError !DeserialiseFailure
  | IOError' !FilePath !IOException
  | OutputMustNotAlreadyExist FilePath
  | ReadCBORFileFailure !FilePath !Text
  deriving Show

renderHelpersError :: HelpersError -> Text
renderHelpersError err =
  case err of
    CardanoEraNotSupportedFail era -> "Cardano era not supported: " <> (Text.pack $ show era)
    OutputMustNotAlreadyExist fp -> "Output file/directory must not already exist: " <> Text.pack fp
    ReadCBORFileFailure fp err' -> "CBOR read failure at: " <> Text.pack fp <> (Text.pack $ show err')
    CBORPrettyPrintError err' -> "Error with CBOR decoding: " <> (Text.pack $ show err')
    CBORDecodingError err' -> "Error with CBOR decoding: " <> (Text.pack $ show err')
    IOError' fp ioE -> "Error at: " <> (Text.pack fp) <> " Error: " <> (Text.pack $ show ioE)

decodeCBOR
  :: LByteString
  -> (forall s. Decoder s a)
  -> Either HelpersError (LB.ByteString, a)
decodeCBOR bs decoder =
  first CBORDecodingError $ deserialiseFromBytes decoder bs

-- | Checks if a path exists and throws and error if it does.
ensureNewFile :: (FilePath -> a -> IO ()) -> FilePath -> a -> ExceptT HelpersError IO ()
ensureNewFile writer outFile blob = do
  exists <- liftIO $ doesPathExist outFile
  when exists $
    left $ OutputMustNotAlreadyExist outFile
  liftIO $ writer outFile blob

ensureNewFileLBS :: FilePath -> LB.ByteString -> ExceptT HelpersError IO ()
ensureNewFileLBS = ensureNewFile LB.writeFile

serialiseSigningKey
  :: CardanoEra
  -> SigningKey
  -> Either HelpersError LB.ByteString
serialiseSigningKey ByronEraLegacy (SigningKey k) = pure $ toLazyByteString (Crypto.toCBORXPrv k)
serialiseSigningKey ByronEra (SigningKey k) = pure $ toLazyByteString (Crypto.toCBORXPrv k)
serialiseSigningKey ShelleyEra _ = Left $ CardanoEraNotSupportedFail ShelleyEra

pPrintCBOR :: LByteString -> ExceptT HelpersError IO ()
pPrintCBOR bs = do
  case deserialiseFromBytes decodeTerm bs of
    Left err -> left $ CBORPrettyPrintError err
    Right (remaining, decodedVal) -> do
      liftIO . putTextLn . toS . prettyHexEnc $ encodeTerm decodedVal
      unless (LB.null remaining) $
        pPrintCBOR remaining

readCBOR :: FilePath -> ExceptT HelpersError IO LByteString
readCBOR fp =
  handleIOExceptT
    (ReadCBORFileFailure fp . toS . displayException)
    (LB.readFile fp)

validateCBOR :: CBORObject -> LByteString -> Either HelpersError Text
validateCBOR cborObject bs =
  case cborObject of
    CBORBlockByron epochSlots -> do
      (const () ) <$> decodeCBOR bs (fromCBORABlockOrBoundary epochSlots)
      Right "Valid Byron block."

    CBORDelegationCertificateByron -> do
      (const () ) <$> decodeCBOR bs (fromCBOR :: Decoder s Delegation.Certificate)
      Right "Valid Byron delegation certificate."

    CBORTxByron -> do
      (const () ) <$> decodeCBOR bs (fromCBOR :: Decoder s UTxO.Tx)
      Right "Valid Byron Tx."

    CBORUpdateProposalByron -> do
      (const () ) <$> decodeCBOR bs (fromCBOR :: Decoder s Update.Proposal)
      Right "Valid Byron update proposal."

    CBORVoteByron -> do
      (const () ) <$> decodeCBOR bs (fromCBOR :: Decoder s Update.Vote)
      Right "Valid Byron vote."

writeProtocolParameters :: Maybe OutputFile -> PParams -> ExceptT HelpersError IO ()
writeProtocolParameters mOutFile pparams =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
    Just (OutputFile fpath) ->
      handleIOExceptT (IOError' fpath) $ LBS.writeFile fpath (encodePretty pparams)
