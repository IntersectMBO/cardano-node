{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.CLI.Byron.Key
  ( -- * Keys
    ByronKeyFailure(..)
  , NewSigningKeyFile(..)
  , NewVerificationKeyFile(..)
  , VerificationKeyFile(..)
  , serialiseSigningKey
  , deserialiseSigningKey
  , keygen
  , prettyPublicKey
  , readEraSigningKey
  , readPaymentVerificationKey
  , renderByronKeyFailure
  , serialisePoorKey
    -- * Passwords
  , PasswordRequirement(..)
  , PasswordPrompt
  , getPassphrase
  )
where

import           Cardano.Prelude hiding (option, show, trace, (%))
import           Prelude (String, show)

import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as UTF8
import           Data.String (fromString)
import qualified Data.Text as T
import           Formatting (build, sformat, (%))

import           System.IO (hFlush, hSetEcho)

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.CLI.Byron.Legacy as Legacy
import           Cardano.CLI.Helpers (textShow)
import           Cardano.CLI.Shelley.Commands (ByronKeyFormat (..))
import           Cardano.CLI.Types
import           Cardano.Crypto (SigningKey (..))
import qualified Cardano.Crypto.Random as Crypto
import qualified Cardano.Crypto.Signing as Crypto


data ByronKeyFailure
  = ReadSigningKeyFailure !FilePath !Text
  | ReadVerificationKeyFailure !FilePath !Text
  | SigningKeyDeserialisationFailed !FilePath !DeserialiseFailure
  | VerificationKeyDeserialisationFailed !FilePath !Text
  deriving Show

renderByronKeyFailure :: ByronKeyFailure -> Text
renderByronKeyFailure err =
  case err of
    ReadSigningKeyFailure sKeyFp readErr ->
      "Error reading signing key at: " <> textShow sKeyFp <> " Error: " <> textShow readErr
    ReadVerificationKeyFailure vKeyFp readErr ->
      "Error reading verification key at: " <> textShow vKeyFp <> " Error: " <> textShow readErr
    SigningKeyDeserialisationFailed sKeyFp deSerError ->
      "Error derserializing signing key at: " <> textShow sKeyFp <> " Error: " <> textShow deSerError
    VerificationKeyDeserialisationFailed vKeyFp deSerError ->
      "Error derserializing verification key at: " <> textShow vKeyFp <> " Error: " <> textShow deSerError

newtype NewSigningKeyFile =
  NewSigningKeyFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewVerificationKeyFile =
  NewVerificationKeyFile FilePath
   deriving (Eq, Ord, Show, IsString)

-- | Whether to require a password, or to supply an empty one.
data PasswordRequirement
  =  GetPassword
  |  EmptyPassword
  deriving (Eq, Show)

type PasswordPrompt = String

-- | Some commands have variants or file formats that depend on the era.

serialiseSigningKey
  :: ByronKeyFormat
  -> Crypto.SigningKey
  -> Either ByronKeyFailure LB.ByteString
serialiseSigningKey _ (Crypto.SigningKey k) = pure $ toLazyByteString (Crypto.toCBORXPrv k)

deserialiseSigningKey :: ByronKeyFormat -> FilePath -> LB.ByteString
                      -> Either ByronKeyFailure SigningKey
deserialiseSigningKey LegacyByronKeyFormat fp delSkey =
  case deserialiseFromBytes Legacy.decodeLegacyDelegateKey delSkey of
    Left deSerFail -> Left $ SigningKeyDeserialisationFailed fp deSerFail
    Right (_, Legacy.LegacyDelegateKey sKey ) -> pure sKey

deserialiseSigningKey NonLegacyByronKeyFormat fp delSkey =
  case deserialiseFromBytes Crypto.fromCBORXPrv delSkey of
    Left deSerFail -> Left $ SigningKeyDeserialisationFailed fp deSerFail
    Right (_, sKey) -> Right $ SigningKey sKey


-- | Print some invariant properties of a public key:
--   its hash and formatted view.
prettyPublicKey :: Crypto.VerificationKey -> Text
prettyPublicKey vk =
  sformat (  "    public key hash: "% build %
           "\npublic key (base64): "% Crypto.fullVerificationKeyF %
           "\n   public key (hex): "% Crypto.fullVerificationKeyHexF)
    (Common.addressHash vk) vk vk

-- TODO:  we need to support password-protected secrets.
-- | Read signing key from a file.  Throw an error if the file can't be read or
-- fails to deserialise.
readEraSigningKey :: ByronKeyFormat -> SigningKeyFile -> ExceptT ByronKeyFailure IO SigningKey
readEraSigningKey bKeyFormat (SigningKeyFile fp) = do
  sK <- handleIOExceptT (ReadSigningKeyFailure fp . T.pack . displayException) $ LB.readFile fp

  -- Signing Key
  hoistEither $ deserialiseSigningKey bKeyFormat fp sK

-- | Read verification key from a file.  Throw an error if the file can't be read
-- or the key fails to deserialise.
readPaymentVerificationKey :: VerificationKeyFile -> ExceptT ByronKeyFailure IO Crypto.VerificationKey
readPaymentVerificationKey (VerificationKeyFile fp) = do
  vkB <- handleIOExceptT (ReadVerificationKeyFailure fp . T.pack . displayException) (SB.readFile fp)
  -- Verification Key
  let eVk = hoistEither . Crypto.parseFullVerificationKey . fromString $ UTF8.toString vkB
  -- Convert error to 'CliError'
  firstExceptT (VerificationKeyDeserialisationFailed fp . T.pack . show) eVk


serialisePoorKey :: ByronKeyFormat -> Genesis.PoorSecret
                 -> Either ByronKeyFailure LB.ByteString
serialisePoorKey bKeyFormat ps =
  serialiseSigningKey bKeyFormat $ Genesis.poorSecretToKey ps

-- | Generate a cryptographically random signing key,
--   protected with a (potentially empty) passphrase.
keygen :: Crypto.PassPhrase -> IO SigningKey
keygen passphrase =
  snd <$> Crypto.runSecureRandom (Crypto.safeKeyGen passphrase)

-- | Get a passphrase from the standard input,
--   depending on whether it's required.
getPassphrase :: PasswordPrompt -> PasswordRequirement -> IO Crypto.PassPhrase
getPassphrase desc GetPassword   = readPassword desc
getPassphrase _    EmptyPassword = pure Crypto.emptyPassphrase

-- | Obtain a 'Crypto.PassPhrase' from the standard input.
--   Terminal echoing is disabled.
readPassword :: String -> IO Crypto.PassPhrase
readPassword prompt =
  Crypto.PassPhrase . BA.convert <$> loop

  where
    loop :: IO ByteString
    loop = do
      (v1, v2) <- (,) <$> readOne prompt <*> readOne "Repeat to validate: "
      if v1 == v2
        then pure v1
        else hPutStrLn stdout ("Sorry, entered passwords don't match." :: String)
             >> loop

    readOne :: String -> IO ByteString
    readOne pr = do
      hPutStr stdout pr >> hFlush stdout
      hSetEcho stdout False
      pp <- SB.hGetLine stdin
      hSetEcho stdout True
      hPutStrLn stdout ("" :: String)
      pure pp
