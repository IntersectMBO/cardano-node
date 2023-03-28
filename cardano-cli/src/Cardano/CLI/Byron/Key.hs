{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.CLI.Byron.Key
  ( -- * Keys
    ByronKeyFailure(..)
  , NewSigningKeyFile(..)
  , NewVerificationKeyFile(..)
  , VerificationKeyFile(..)
  , prettyPublicKey
  , readByronSigningKey
  , readPaymentVerificationKey
  , renderByronKeyFailure
  , byronWitnessToVerKey

  , toNewVerificationKeyFileIn
  , toNewVerificationKeyFileOut
  , toNewSigningKeyFileIn
  , toNewSigningKeyFileOut
  )
where

import           Control.Exception (Exception (..))
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left,
                   right)
import qualified Data.ByteString as SB
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Coerce (coerce)
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Formatting (build, sformat, (%))

import           Cardano.Api.Byron

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

renderByronKeyFailure :: ByronKeyFailure -> Text
renderByronKeyFailure err =
  case err of
    CannotMigrateFromNonLegacySigningKey fp ->
      "Migrate from non-legacy Byron key unnecessary: " <> textShow fp
    ReadSigningKeyFailure sKeyFp readErr ->
      "Error reading signing key at: " <> textShow sKeyFp <> " Error: " <> textShow readErr
    ReadVerificationKeyFailure vKeyFp readErr ->
      "Error reading verification key at: " <> textShow vKeyFp <> " Error: " <> textShow readErr
    LegacySigningKeyDeserialisationFailed fp ->
      "Error attempting to deserialise a legacy signing key at: " <> textShow fp
    SigningKeyDeserialisationFailed sKeyFp  ->
      "Error deserialising signing key at: " <> textShow sKeyFp
    VerificationKeyDeserialisationFailed vKeyFp deSerError ->
      "Error deserialising verification key at: " <> textShow vKeyFp <> " Error: " <> textShow deSerError

newtype NewSigningKeyFile (direction :: FileDirection) = NewSigningKeyFile
  { unNewSigningKeyFile :: File direction
  } deriving newtype (Eq, Ord, Show, IsString, MapFile, FromJSON, ToJSON)

toNewSigningKeyFileIn :: NewSigningKeyFile 'InOut -> NewSigningKeyFile 'In
toNewSigningKeyFileIn = coerce

toNewSigningKeyFileOut :: NewSigningKeyFile 'InOut -> NewSigningKeyFile 'Out
toNewSigningKeyFileOut = coerce

newtype NewVerificationKeyFile (direction :: FileDirection) = NewVerificationKeyFile
  { unNewVerificationKeyFile :: File direction
  } deriving newtype (Eq, Ord, Show, IsString, MapFile, FromJSON, ToJSON)

toNewVerificationKeyFileIn :: NewVerificationKeyFile 'InOut -> NewVerificationKeyFile 'In
toNewVerificationKeyFileIn = coerce

toNewVerificationKeyFileOut :: NewVerificationKeyFile 'InOut -> NewVerificationKeyFile 'Out
toNewVerificationKeyFileOut = coerce

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
readByronSigningKey :: ByronKeyFormat -> SigningKeyFile 'In -> ExceptT ByronKeyFailure IO SomeByronSigningKey
readByronSigningKey bKeyFormat (SigningKeyFile fp) = do
  sK <- handleIOExceptT (ReadSigningKeyFailure (unFile fp) . T.pack . displayException) $ SB.readFile (unFile fp)
  case bKeyFormat of
    LegacyByronKeyFormat ->
      case deserialiseFromRawBytes (AsSigningKey AsByronKeyLegacy) sK of
        Right legKey -> right $ AByronSigningKeyLegacy legKey
        Left _ -> left $ LegacySigningKeyDeserialisationFailed (unFile fp)
    NonLegacyByronKeyFormat ->
      case deserialiseFromRawBytes (AsSigningKey AsByronKey) sK of
        Right nonLegSKey -> right $ AByronSigningKey nonLegSKey
        Left _ -> left $ SigningKeyDeserialisationFailed (unFile fp)

-- | Read verification key from a file.  Throw an error if the file can't be read
-- or the key fails to deserialise.
readPaymentVerificationKey :: VerificationKeyFile 'In -> ExceptT ByronKeyFailure IO Crypto.VerificationKey
readPaymentVerificationKey (VerificationKeyFile fp) = do
  vkB <- handleIOExceptT (ReadVerificationKeyFailure (unFile fp) . T.pack . displayException) (SB.readFile (unFile fp))
  -- Verification Key
  let eVk = hoistEither . Crypto.parseFullVerificationKey . fromString $ UTF8.toString vkB
  -- Convert error to 'CliError'
  firstExceptT (VerificationKeyDeserialisationFailed (unFile fp) . T.pack . show) eVk

