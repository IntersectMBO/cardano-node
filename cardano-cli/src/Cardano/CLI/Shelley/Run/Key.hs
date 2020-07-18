{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Shelley.Run.Key
  ( ShelleyKeyCmdError
  , renderShelleyKeyCmdError
  , runKeyCmd

    -- * Exports for testing
  , decodeBech32Key
  ) where

import           Cardano.Prelude

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, hoistEither, newExceptT)
import qualified Control.Exception as Exception

import qualified Codec.Binary.Bech32 as Bech32

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Signing as Byron.Crypto

import qualified Cardano.Crypto.Signing as Byron
import qualified Shelley.Spec.Ledger.Keys as Shelley

import           Cardano.Api.Typed hiding (Bech32DecodeError(..))

import qualified Cardano.CLI.Byron.Key as Byron
import           Cardano.CLI.Byron.Key (CardanoEra(..))
import           Cardano.CLI.Helpers (textShow)
import           Cardano.CLI.Shelley.Commands


data ShelleyKeyCmdError
  = ShelleyKeyCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyKeyCmdWriteFileError !(FileError ())
  | ShelleyKeyCmdByronKeyFailure !Byron.ByronKeyFailure
  | ShelleyKeyCmdByronKeyParseError
      !Text
      -- ^ Text representation of the parse error. Unfortunately, the actual
      -- error type isn't exported.
  | ShelleyKeyCmdItnKeyConvError !ConversionError
  deriving Show

renderShelleyKeyCmdError :: ShelleyKeyCmdError -> Text
renderShelleyKeyCmdError err =
  case err of
    ShelleyKeyCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyKeyCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyKeyCmdByronKeyFailure e -> Byron.renderByronKeyFailure e
    ShelleyKeyCmdByronKeyParseError errTxt -> errTxt
    ShelleyKeyCmdItnKeyConvError convErr -> renderConversionError convErr

runKeyCmd :: KeyCmd -> ExceptT ShelleyKeyCmdError IO ()
runKeyCmd cmd =
  case cmd of
    KeyGetVerificationKey skf vkf ->
      runGetVerificationKey skf vkf

    KeyNonExtendedKey evkf vkf ->
      runNonExtendedKey evkf vkf

    KeyConvertByronKey keytype skfOld skfNew ->
      runConvertByronKey keytype skfOld skfNew

    KeyConvertByronGenesisVKey oldVk newVkf ->
      runConvertByronGenesisVerificationKey oldVk newVkf

    KeyConvertITNStakeKey itnKeyFile outFile ->
      runConvertITNStakeKey itnKeyFile outFile


runGetVerificationKey :: SigningKeyFile
                      -> VerificationKeyFile
                      -> ExceptT ShelleyKeyCmdError IO ()
runGetVerificationKey skf (VerificationKeyFile vkf) = do
    ssk <- firstExceptT ShelleyKeyCmdReadFileError $
             readSigningKeyFile skf
    withSomeSigningKey ssk $ \sk ->
      let vk = getVerificationKey sk in
      firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
        writeFileTextEnvelope vkf Nothing vk


data SomeSigningKey
  = AByronSigningKey           (SigningKey ByronKey)
  | APaymentSigningKey         (SigningKey PaymentKey)
  | APaymentExtendedSigningKey (SigningKey PaymentExtendedKey)
  | AStakeSigningKey           (SigningKey StakeKey)
  | AStakeExtendedSigningKey   (SigningKey StakeExtendedKey)
  | AStakePoolSigningKey       (SigningKey StakePoolKey)
  | AGenesisSigningKey         (SigningKey GenesisKey)
  | AGenesisExtendedSigningKey (SigningKey GenesisExtendedKey)
  | AGenesisDelegateSigningKey (SigningKey GenesisDelegateKey)
  | AGenesisDelegateExtendedSigningKey
                               (SigningKey GenesisDelegateExtendedKey)
  | AGenesisUTxOSigningKey     (SigningKey GenesisUTxOKey)
  | AVrfSigningKey             (SigningKey VrfKey)
  | AKesSigningKey             (SigningKey KesKey)

withSomeSigningKey :: SomeSigningKey
                   -> (forall keyrole. Key keyrole => SigningKey keyrole -> a)
                   -> a
withSomeSigningKey ssk f =
    case ssk of
      AByronSigningKey           sk -> f sk
      APaymentSigningKey         sk -> f sk
      APaymentExtendedSigningKey sk -> f sk
      AStakeSigningKey           sk -> f sk
      AStakeExtendedSigningKey   sk -> f sk
      AStakePoolSigningKey       sk -> f sk
      AGenesisSigningKey         sk -> f sk
      AGenesisExtendedSigningKey sk -> f sk
      AGenesisDelegateSigningKey sk -> f sk
      AGenesisDelegateExtendedSigningKey 
                                 sk -> f sk
      AGenesisUTxOSigningKey     sk -> f sk
      AVrfSigningKey             sk -> f sk
      AKesSigningKey             sk -> f sk

readSigningKeyFile
  :: SigningKeyFile
  -> ExceptT (FileError TextEnvelopeError) IO SomeSigningKey
readSigningKeyFile (SigningKeyFile skfile) =
    newExceptT $ readFileTextEnvelopeAnyOf fileTypes skfile
  where
    fileTypes =
      [ FromSomeType (AsSigningKey AsByronKey)
                      AByronSigningKey
      , FromSomeType (AsSigningKey AsPaymentKey)
                      APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                      APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                      AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                      AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                      AStakePoolSigningKey
      , FromSomeType (AsSigningKey AsGenesisKey)
                      AGenesisSigningKey
      , FromSomeType (AsSigningKey AsGenesisExtendedKey)
                      AGenesisExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateKey)
                      AGenesisDelegateSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey)
                      AGenesisDelegateExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisUTxOKey)
                      AGenesisUTxOSigningKey
      , FromSomeType (AsSigningKey AsVrfKey)
                      AVrfSigningKey
      , FromSomeType (AsSigningKey AsKesKey)
                      AKesSigningKey
      ]


runNonExtendedKey :: VerificationKeyFile
                  -> VerificationKeyFile
                  -> ExceptT ShelleyKeyCmdError IO ()
runNonExtendedKey evkf (VerificationKeyFile vkf) = do
    evk <- firstExceptT ShelleyKeyCmdReadFileError $
             readExtendedVerificationKeyFile evkf
    withNonExtendedKey evk $ \vk ->
      firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
        writeFileTextEnvelope vkf Nothing vk

withNonExtendedKey :: SomeExtendedVerificationKey
                   -> (forall keyrole. Key keyrole => VerificationKey keyrole -> a)
                   -> a
withNonExtendedKey (APaymentExtendedVerificationKey vk) f =
    f (castVerificationKey vk :: VerificationKey PaymentKey)

withNonExtendedKey (AStakeExtendedVerificationKey vk) f =
    f (castVerificationKey vk :: VerificationKey StakeKey)

withNonExtendedKey (AGenesisExtendedVerificationKey vk) f =
    f (castVerificationKey vk :: VerificationKey GenesisKey)

withNonExtendedKey (AGenesisDelegateExtendedVerificationKey vk) f =
    f (castVerificationKey vk :: VerificationKey GenesisDelegateKey)


data SomeExtendedVerificationKey
  = APaymentExtendedVerificationKey (VerificationKey PaymentExtendedKey)
  | AStakeExtendedVerificationKey   (VerificationKey StakeExtendedKey)
  | AGenesisExtendedVerificationKey (VerificationKey GenesisExtendedKey)
  | AGenesisDelegateExtendedVerificationKey
                                    (VerificationKey GenesisDelegateExtendedKey)

readExtendedVerificationKeyFile
  :: VerificationKeyFile
  -> ExceptT (FileError TextEnvelopeError) IO SomeExtendedVerificationKey
readExtendedVerificationKeyFile (VerificationKeyFile evkfile) =
    newExceptT $ readFileTextEnvelopeAnyOf fileTypes evkfile
  where
    fileTypes =
      [ FromSomeType (AsVerificationKey AsPaymentExtendedKey)
                      APaymentExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsStakeExtendedKey)
                      AStakeExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsGenesisExtendedKey)
                      AGenesisExtendedVerificationKey
      , FromSomeType (AsVerificationKey AsGenesisDelegateExtendedKey)
                      AGenesisDelegateExtendedVerificationKey
      ]


runConvertByronKey
  :: ByronKeyType
  -> SomeKeyFile     -- ^ Input file: old format
  -> OutputFile      -- ^ Output file: new format
  -> ExceptT ShelleyKeyCmdError IO ()
runConvertByronKey (ByronPaymentKey format) (ASigningKeyFile skeyPathOld) =
    convertByronSigningKey format convert skeyPathOld
  where
    convert :: Byron.SigningKey -> SigningKey ByronKey
    convert = ByronSigningKey

runConvertByronKey (ByronGenesisKey format) (ASigningKeyFile skeyPathOld) =
    convertByronSigningKey format convert skeyPathOld
  where
    convert :: Byron.SigningKey -> SigningKey GenesisExtendedKey
    convert (Byron.SigningKey xsk) = GenesisExtendedSigningKey xsk

runConvertByronKey (ByronDelegateKey format) (ASigningKeyFile skeyPathOld) =
    convertByronSigningKey format convert skeyPathOld
  where
    convert :: Byron.SigningKey -> SigningKey GenesisDelegateExtendedKey
    convert (Byron.SigningKey xsk) = GenesisDelegateExtendedSigningKey xsk

runConvertByronKey (ByronPaymentKey NonLegacyByronKeyFormat)
                   (AVerificationKeyFile vkeyPathOld) =
    convertByronVerificationKey convert vkeyPathOld
  where
    convert :: Byron.VerificationKey -> VerificationKey ByronKey
    convert = ByronVerificationKey

runConvertByronKey (ByronGenesisKey NonLegacyByronKeyFormat)
                   (AVerificationKeyFile vkeyPathOld) =
    convertByronVerificationKey convert vkeyPathOld
  where
    convert :: Byron.VerificationKey -> VerificationKey GenesisExtendedKey
    convert (Byron.VerificationKey xvk) = GenesisExtendedVerificationKey xvk

runConvertByronKey (ByronDelegateKey NonLegacyByronKeyFormat)
                   (AVerificationKeyFile vkeyPathOld) =
    convertByronVerificationKey convert vkeyPathOld
  where
    convert :: Byron.VerificationKey
            -> VerificationKey GenesisDelegateExtendedKey
    convert (Byron.VerificationKey xvk) =
      GenesisDelegateExtendedVerificationKey xvk

runConvertByronKey (ByronPaymentKey  LegacyByronKeyFormat)
                    AVerificationKeyFile{} =
    const legacyVerificationKeysNotSupported

runConvertByronKey (ByronGenesisKey  LegacyByronKeyFormat)
                    AVerificationKeyFile{} =
    const legacyVerificationKeysNotSupported

runConvertByronKey (ByronDelegateKey LegacyByronKeyFormat)
                    AVerificationKeyFile{} =
    const legacyVerificationKeysNotSupported

legacyVerificationKeysNotSupported :: ExceptT e IO a
legacyVerificationKeysNotSupported =
    liftIO $ do
      putStrLn $ "convert keys: byron legacy format not supported for "
              ++ "verification keys. Convert the signing key and then get the "
              ++ "verification key."
      exitFailure


convertByronSigningKey
  :: forall keyrole.
     Key keyrole
  => ByronKeyFormat
  -> (Byron.SigningKey -> SigningKey keyrole)
  -> SigningKeyFile      -- ^ Input file: old format
  -> OutputFile          -- ^ Output file: new format
  -> ExceptT ShelleyKeyCmdError IO ()
convertByronSigningKey byronFormat convert
                       skeyPathOld
                       (OutputFile skeyPathNew) = do

    sk <- firstExceptT ShelleyKeyCmdByronKeyFailure $
            Byron.readEraSigningKey (toCarandoEra byronFormat) skeyPathOld

    let sk' :: SigningKey keyrole
        sk' = convert sk

    firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
      writeFileTextEnvelope skeyPathNew Nothing sk'

  where
    -- TODO: merge these two types
    toCarandoEra :: ByronKeyFormat -> CardanoEra
    toCarandoEra NonLegacyByronKeyFormat = ByronEra
    toCarandoEra LegacyByronKeyFormat    = ByronEraLegacy


convertByronVerificationKey
  :: forall keyrole.
     Key keyrole
  => (Byron.VerificationKey -> VerificationKey keyrole)
  -> VerificationKeyFile -- ^ Input file: old format
  -> OutputFile          -- ^ Output file: new format
  -> ExceptT ShelleyKeyCmdError IO ()
convertByronVerificationKey convert
                            (VerificationKeyFile vkeyPathOld)
                            (OutputFile vkeyPathNew) = do

    vk <- firstExceptT ShelleyKeyCmdByronKeyFailure $
            Byron.readPaymentVerificationKey (Byron.VerificationKeyFile vkeyPathOld)

    let vk' :: VerificationKey keyrole
        vk' = convert vk

    firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
      writeFileTextEnvelope vkeyPathNew Nothing vk'


runConvertByronGenesisVerificationKey
  :: VerificationKeyBase64  -- ^ Input key raw old format
  -> OutputFile             -- ^ Output file: new format
  -> ExceptT ShelleyKeyCmdError IO ()
runConvertByronGenesisVerificationKey (VerificationKeyBase64 b64ByronVKey)
                                      (OutputFile vkeyPathNew) = do

    vk <- firstExceptT (ShelleyKeyCmdByronKeyParseError . show)
        . hoistEither
        . Byron.Crypto.parseFullVerificationKey
        . Text.pack
        $ b64ByronVKey

    let vk' :: VerificationKey GenesisKey
        vk' = convert vk

    firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
      writeFileTextEnvelope vkeyPathNew Nothing vk'
  where
    convert :: Byron.VerificationKey -> VerificationKey GenesisKey
    convert (Byron.VerificationKey xvk) =
      castVerificationKey (GenesisExtendedVerificationKey xvk)


--------------------------------------------------------------------------------
-- ITN verification/signing key conversion to Haskell verficiation/signing keys
--------------------------------------------------------------------------------

runConvertITNStakeKey
  :: SomeKeyFile
  -> OutputFile
  -> ExceptT ShelleyKeyCmdError IO ()
runConvertITNStakeKey (AVerificationKeyFile (VerificationKeyFile vk))
                      (OutputFile outFile) = do
  bech32publicKey <- firstExceptT ShelleyKeyCmdItnKeyConvError . newExceptT $
                     readFileITNKey vk
  vkey <- hoistEither
    . first ShelleyKeyCmdItnKeyConvError
    $ convertITNVerificationKey bech32publicKey
  firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
    writeFileTextEnvelope outFile Nothing vkey

runConvertITNStakeKey (ASigningKeyFile (SigningKeyFile sk))
                      (OutputFile outFile) = do
  bech32privateKey <- firstExceptT ShelleyKeyCmdItnKeyConvError . newExceptT $
                      readFileITNKey sk
  skey <- hoistEither
    . first ShelleyKeyCmdItnKeyConvError
    $ convertITNSigningKey bech32privateKey
  firstExceptT ShelleyKeyCmdWriteFileError . newExceptT $
    writeFileTextEnvelope outFile Nothing skey


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

