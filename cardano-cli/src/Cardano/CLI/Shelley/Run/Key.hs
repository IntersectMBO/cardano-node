module Cardano.CLI.Shelley.Run.Key
  ( ShelleyKeyCmdError
  , renderShelleyKeyCmdError
  , runKeyCmd
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT,
                   handleIOExceptT, hoistEither, hoistMaybe, newExceptT)
import qualified Control.Exception as Exception

import qualified Codec.Binary.Bech32 as Bech32

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Signing as Byron.Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD

import qualified Shelley.Spec.Ledger.Keys as Shelley

import           Cardano.Api.TextView (TextViewDescription (..))
import           Cardano.Api.Typed hiding (Bech32DecodeError(..))

import           Cardano.CLI.Byron.Key (ByronKeyFailure, CardanoEra (..),
                   readEraSigningKey, renderByronKeyFailure)
import           Cardano.CLI.Helpers (textShow)
import           Cardano.CLI.Shelley.Parsers (ITNKeyFile (..), KeyCmd (..),
                   OutputFile (..), SigningKeyFile (..),
                   VerificationKeyFile (..))


data ShelleyKeyCmdError
  = ShelleyKeyCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyKeyCmdWriteFileError !(FileError ())
  | ShelleyKeyCmdByronKeyFailure !ByronKeyFailure
  | ShelleyKeyCmdByronKeyParseError
      !Text
      -- ^ Text representation of the parse error. Unfortunately, the actual
      -- error type isn't exported.
  | ShelleyKeyCmdDeserialiseByronVerKeyError
  | ShelleyKeyCmdItnKeyConvError !ConversionError
  deriving Show

renderShelleyKeyCmdError :: ShelleyKeyCmdError -> Text
renderShelleyKeyCmdError err =
  case err of
    ShelleyKeyCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyKeyCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyKeyCmdByronKeyFailure e -> renderByronKeyFailure e
    ShelleyKeyCmdByronKeyParseError errTxt -> errTxt
    ShelleyKeyCmdDeserialiseByronVerKeyError ->
      "Failed to deserialise the provided Byron verification key."
    ShelleyKeyCmdItnKeyConvError convErr -> renderConversionError convErr

runKeyCmd :: KeyCmd -> ExceptT ShelleyKeyCmdError IO ()
runKeyCmd cmd =
  case cmd of
    KeyConvertByronPaymentKey skfOld skfNew ->
      runConvertByronPaymentKey skfOld skfNew
    KeyConvertByronGenesisVerificationKey oldVkf newVkf ->
      runConvertByronGenesisVerificationKey oldVkf newVkf
    KeyConvertITNStakeKey itnKeyFile mOutFile ->
      runSingleITNKeyConversion itnKeyFile mOutFile

runConvertByronPaymentKey
  :: SigningKeyFile -- ^ Input file: old format
  -> SigningKeyFile -- ^ Output file: new format
  -> ExceptT ShelleyKeyCmdError IO ()
runConvertByronPaymentKey skeyPathOld (SigningKeyFile skeyPathNew) = do
    sk <- firstExceptT ShelleyKeyCmdByronKeyFailure $
            readEraSigningKey ByronEra skeyPathOld
    firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
      writeFileTextEnvelope skeyPathNew (Just skeyDesc) (ByronSigningKey sk)
  where
    skeyDesc = TextViewDescription "Payment Signing Key"

runConvertByronGenesisVerificationKey
  :: VerificationKeyFile -- ^ Input file: old format
  -> VerificationKeyFile -- ^ Output file: new format
  -> ExceptT ShelleyKeyCmdError IO ()
runConvertByronGenesisVerificationKey (VerificationKeyFile oldVkFp)
                                      (VerificationKeyFile newVkFp) = do
  b64ByronVKey <- handleIOExceptT (ShelleyKeyCmdReadFileError . FileIOError oldVkFp) $
    Text.readFile oldVkFp
  byronVKey <- firstExceptT (ShelleyKeyCmdByronKeyParseError . show)
    . hoistEither
    . Byron.Crypto.parseFullVerificationKey
    $ b64ByronVKey
  shelleyGenesisVKey <- hoistMaybe ShelleyKeyCmdDeserialiseByronVerKeyError
    . fmap (GenesisVerificationKey . Shelley.VKey)
    . Crypto.rawDeserialiseVerKeyDSIGN
    . Crypto.HD.xpubPublicKey
    . Byron.Crypto.unVerificationKey
    $ byronVKey
  firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
    writeFileTextEnvelope newVkFp Nothing shelleyGenesisVKey


--------------------------------------------------------------------------------
-- ITN verification/signing key conversion to Haskell verficiation/signing keys
--------------------------------------------------------------------------------

runSingleITNKeyConversion
  :: ITNKeyFile
  -> Maybe OutputFile
  -> ExceptT ShelleyKeyCmdError IO ()
runSingleITNKeyConversion (ITNVerificationKeyFile (VerificationKeyFile vk))
                          mOutFile = do
  bech32publicKey <- firstExceptT ShelleyKeyCmdItnKeyConvError . newExceptT $
                     readFileITNKey vk
  vkey <- hoistEither
    . first ShelleyKeyCmdItnKeyConvError
    $ convertITNVerificationKey bech32publicKey
  case mOutFile of
    Just (OutputFile fp) ->
      firstExceptT ShelleyKeyCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope fp Nothing vkey
    Nothing -> print vkey

runSingleITNKeyConversion (ITNSigningKeyFile (SigningKeyFile sk)) mOutFile = do
  bech32privateKey <- firstExceptT ShelleyKeyCmdItnKeyConvError . newExceptT $
                      readFileITNKey sk
  skey <- hoistEither
    . first ShelleyKeyCmdItnKeyConvError
    $ convertITNSigningKey bech32privateKey
  case mOutFile of
    Just (OutputFile fp) ->
      firstExceptT ShelleyKeyCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope fp Nothing skey
    Nothing -> print skey


data ConversionError
  = Bech32DecodingError
      -- ^ Bech32 key
      !Text
      !Bech32.DecodingError
  | Bech32ErrorExtractingByes !Bech32.DataPart
  | Bech32ReadError !FilePath !IOException
  | ITNError !Bech32.HumanReadablePart !Bech32.DataPart
  | SigningKeyDeserializationError !ByteString
  | VerificationKeyDeserializationError !ByteString
  deriving Show

renderConversionError :: ConversionError -> Text
renderConversionError err =
  case err of
    Bech32DecodingError key decErr ->
      "Error decoding Bech32 key: " <> key <> " Error: " <> textShow decErr
    Bech32ErrorExtractingByes dp ->
      "Unable to extract bytes from: " <> Bech32.dataPartToText dp
    Bech32ReadError fp readErr ->
      "Error reading bech32 key at: " <> textShow fp
                        <> " Error: " <> Text.pack (displayException readErr)
    ITNError hRpart dp ->
      "Error extracting a ByteString from DataPart: " <> Bech32.dataPartToText dp <>
      " With human readable part: " <> Bech32.humanReadablePartToText hRpart
    SigningKeyDeserializationError sKey ->
      "Error deserialising signing key: " <> textShow (BS.unpack sKey)
    VerificationKeyDeserializationError vKey ->
      "Error deserialising verification key: " <> textShow (BS.unpack vKey)

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
decodeBech32Key :: Text
                -> Either ConversionError
                          (Bech32.HumanReadablePart, Bech32.DataPart, ByteString)
decodeBech32Key key =
  case Bech32.decodeLenient key of
    Left err -> Left $ Bech32DecodingError key err
    Right (hRpart, dataPart) -> case Bech32.dataPartToBytes dataPart of
                                  Nothing -> Left $ ITNError hRpart dataPart
                                  Just bs -> Right (hRpart, dataPart, bs)

readFileITNKey :: FilePath -> IO (Either ConversionError Text)
readFileITNKey fp = do
  eStr <- Exception.try $ readFile fp
  case eStr of
    Left e -> return . Left $ Bech32ReadError fp e
    Right str -> return . Right . Text.concat $ Text.words str

