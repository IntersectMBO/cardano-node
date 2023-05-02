{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Cardano.CLI.Byron.Key
  ( -- * Keys
    ByronKeyFailure(..)
  , NewSigningKeyFile(..)
  , NewVerificationKeyFile(..)
  , VerificationKeyFile
  , prettyPublicKey
  , readByronSigningKey
  , readPaymentVerificationKey
  , renderByronKeyFailure
  , byronWitnessToVerKey
  )
where

import           Control.Exception (Exception (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left,
                   right)
import qualified Data.ByteString as SB
import qualified Data.ByteString.UTF8 as UTF8
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Formatting (build, sformat, (%))

import           Cardano.Api.Byron
import           Cardano.Api.Pretty

import qualified Cardano.Chain.Common as Common
import           Cardano.CLI.Shelley.Commands (ByronKeyFormat (..))
import           Cardano.CLI.Types
import qualified Cardano.Crypto.Signing as Crypto


data ByronKeyFailure
  = ReadSigningKeyFailure !FilePath !Text
  | ReadVerificationKeyFailure !FilePath !Text
  | LegacySigningKeyDeserialisationFailed !FilePath
  | SigningKeyDeserialisationFailed !FilePath
  | VerificationKeyDeserialisationFailed !FilePath !Text
  | CannotMigrateFromNonLegacySigningKey !FilePath
  deriving Show

renderByronKeyFailure :: ByronKeyFailure -> Doc Ann
renderByronKeyFailure err =
  case err of
    CannotMigrateFromNonLegacySigningKey fp ->
      "Migrate from non-legacy Byron key unnecessary: " <> pretty fp
    ReadSigningKeyFailure sKeyFp readErr ->
      "Error reading signing key at: " <> pretty sKeyFp <> " Error: " <> pretty readErr
    ReadVerificationKeyFailure vKeyFp readErr ->
      "Error reading verification key at: " <> pretty vKeyFp <> " Error: " <> pretty readErr
    LegacySigningKeyDeserialisationFailed fp ->
      "Error attempting to deserialise a legacy signing key at: " <> pretty fp
    SigningKeyDeserialisationFailed sKeyFp  ->
      "Error deserialising signing key at: " <> pretty sKeyFp
    VerificationKeyDeserialisationFailed vKeyFp deSerError ->
      "Error deserialising verification key at: " <> pretty vKeyFp <> " Error: " <> pretty deSerError

newtype NewSigningKeyFile =
  NewSigningKeyFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewVerificationKeyFile =
  NewVerificationKeyFile FilePath
   deriving (Eq, Ord, Show, IsString)

-- | Print some invariant properties of a public key:
--   its hash and formatted view.
prettyPublicKey :: VerificationKey ByronKey-> Text
prettyPublicKey (ByronVerificationKey vk) =
  sformat (  "    public key hash: " % build %
           "\npublic key (base64): " % Crypto.fullVerificationKeyF %
           "\n   public key (hex): " % Crypto.fullVerificationKeyHexF)
    (Common.addressHash vk) vk vk

byronWitnessToVerKey :: SomeByronSigningKey -> VerificationKey ByronKey
byronWitnessToVerKey (AByronSigningKeyLegacy sKeyLeg) = castVerificationKey $ getVerificationKey sKeyLeg
byronWitnessToVerKey (AByronSigningKey sKeyNonLeg) = getVerificationKey sKeyNonLeg

-- TODO:  we need to support password-protected secrets.
-- | Read signing key from a file.
readByronSigningKey :: ByronKeyFormat -> SigningKeyFile In -> ExceptT ByronKeyFailure IO SomeByronSigningKey
readByronSigningKey bKeyFormat (File fp) = do
  sK <- handleIOExceptT (ReadSigningKeyFailure fp . T.pack . displayException) $ SB.readFile fp
  case bKeyFormat of
    LegacyByronKeyFormat ->
      case deserialiseFromRawBytes (AsSigningKey AsByronKeyLegacy) sK of
        Right legKey -> right $ AByronSigningKeyLegacy legKey
        Left _ -> left $ LegacySigningKeyDeserialisationFailed fp
    NonLegacyByronKeyFormat ->
      case deserialiseFromRawBytes (AsSigningKey AsByronKey) sK of
        Right nonLegSKey -> right $ AByronSigningKey nonLegSKey
        Left _ -> left $ SigningKeyDeserialisationFailed fp

-- | Read verification key from a file.  Throw an error if the file can't be read
-- or the key fails to deserialise.
readPaymentVerificationKey :: VerificationKeyFile In -> ExceptT ByronKeyFailure IO Crypto.VerificationKey
readPaymentVerificationKey (File fp) = do
  vkB <- handleIOExceptT (ReadVerificationKeyFailure fp . T.pack . displayException) (SB.readFile fp)
  -- Verification Key
  let eVk = hoistEither . Crypto.parseFullVerificationKey . fromString $ UTF8.toString vkB
  -- Convert error to 'CliError'
  firstExceptT (VerificationKeyDeserialisationFailed fp . T.pack . show) eVk

