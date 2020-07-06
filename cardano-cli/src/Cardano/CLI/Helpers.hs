{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Helpers
  ( ConversionError(..)
  , HelpersError(..)
  , convertITNVerificationKey
  , convertITNSigningKey
  , dataPartToBase16
  , decodeBech32Key
  , ensureNewFile
  , ensureNewFileLBS
  , pPrintCBOR
  , readCBOR
  , readBech32
  , renderConversionError
  , renderHelpersError
  , validateCBOR
  ) where

import           Cardano.Prelude

import           Codec.Binary.Bech32 (DataPart, HumanReadablePart, dataPartToBytes,
                   dataPartToText)
import qualified Codec.Binary.Bech32 as Bech32
import           Codec.CBOR.Pretty (prettyHexEnc)
import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Term (decodeTerm, encodeTerm)
import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Control.Monad.Trans.Except.Extra (handleIOExceptT, left)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import           System.Directory (doesPathExist)

import           Cardano.Api (textShow)
import           Cardano.Api.Typed (SigningKey(..), StakeKey, VerificationKey(..))
import           Cardano.Binary (Decoder, fromCBOR)
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Update as Update
import           Cardano.Chain.Block (fromCBORABlockOrBoundary)
import qualified Cardano.Chain.UTxO as UTxO
import           Cardano.Config.Types
import qualified Cardano.Crypto.DSIGN as DSIGN

import qualified Shelley.Spec.Ledger.Keys as Shelley

data HelpersError
  = CBORPrettyPrintError !DeserialiseFailure
  | CBORDecodingError !DeserialiseFailure
  | IOError' !FilePath !IOException
  | OutputMustNotAlreadyExist FilePath
  | ReadCBORFileFailure !FilePath !Text
  deriving Show

renderHelpersError :: HelpersError -> Text
renderHelpersError err =
  case err of
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

--------------------------------------------------------------------------------
-- ITN verification/signing key conversion to Haskell verficiation/signing keys
--------------------------------------------------------------------------------

data ConversionError
  = Bech32DecodingError
      -- ^ Bech32 key
      !Text
      !Bech32.DecodingError
  | Bech32ErrorExtractingByes !DataPart
  | Bech32ReadError !FilePath !Text
  | ITNError !HumanReadablePart !DataPart
  | SigningKeyDeserializationError !ByteString
  | VerificationKeyDeserializationError !ByteString
  deriving Show

renderConversionError :: ConversionError -> Text
renderConversionError err =
  case err of
    Bech32DecodingError key decErr ->
      "Error decoding Bech32 key: " <> key <> " Error: " <> textShow decErr
    Bech32ErrorExtractingByes dp ->
      "Unable to extract bytes from: " <> dataPartToText dp
    Bech32ReadError fp readErr ->
      "Error reading bech32 key at: " <> textShow fp <> " Error: " <> readErr
    ITNError hRpart dp ->
      "Error extracting a ByteString from DataPart: " <> Bech32.dataPartToText dp <>
      " With human readable part: " <> Bech32.humanReadablePartToText hRpart
    SigningKeyDeserializationError sKey ->
      "Error deserialising signing key: " <> textShow (SC.unpack sKey)
    VerificationKeyDeserializationError vKey ->
      "Error deserialising verification key: " <> textShow (SC.unpack vKey)

-- | Convert public ed25519 key to a Shelley stake verification key
convertITNVerificationKey :: Text -> Either ConversionError (VerificationKey StakeKey)
convertITNVerificationKey pubKey = do
  (_, _, keyBS) <- decodeBech32Key pubKey
  case DSIGN.rawDeserialiseVerKeyDSIGN keyBS of
    Just verKey -> Right . StakeVerificationKey $ Shelley.VKey verKey
    Nothing -> Left $ VerificationKeyDeserializationError keyBS

-- | Convert private ed22519 key to a Shelley signing key.
convertITNSigningKey :: Text -> Either ConversionError (SigningKey StakeKey)
convertITNSigningKey privKey = do
  (_, _, keyBS) <- decodeBech32Key privKey
  case DSIGN.rawDeserialiseSignKeyDSIGN keyBS of
    Just signKey -> Right $ StakeSigningKey signKey
    Nothing -> Left $ SigningKeyDeserializationError keyBS

-- | Convert ITN Bech32 public or private keys to 'ByteString's
decodeBech32Key :: Text -> Either ConversionError (HumanReadablePart, DataPart, ByteString)
decodeBech32Key key =
  case Bech32.decodeLenient key of
    Left err -> Left $ Bech32DecodingError key err
    Right (hRpart, dataPart) -> case Bech32.dataPartToBytes dataPart of
                                  Nothing -> Left $ ITNError hRpart dataPart
                                  Just bs -> Right (hRpart, dataPart, bs)

dataPartToBase16 :: DataPart -> Either ConversionError ByteString
dataPartToBase16 dp = case dataPartToBytes dp of
                        Just bs -> Right $ Base16.encode bs
                        Nothing -> Left $ Bech32ErrorExtractingByes dp

readBech32 :: FilePath -> IO (Either ConversionError Text)
readBech32 fp = do
  eStr <- Exception.try $ readFile fp
  case eStr of
    Left e -> return . Left $ Bech32ReadError fp $ handler e
    Right str -> return . Right . Text.concat $ Text.words str
 where
  handler :: IOException -> Text
  handler e = Text.pack $ "Cardano.Api.Convert.readBech32: "
                        ++ displayException e
