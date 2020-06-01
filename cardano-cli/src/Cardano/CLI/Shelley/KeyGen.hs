
module Cardano.CLI.Shelley.KeyGen
  ( ShelleyKeyGenError
  , renderShelleyKeyGenError
  , runColdKeyGen
  ) where

import           Cardano.Prelude

import qualified Data.Text as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.Api.Typed as Api

import           Cardano.CLI.Shelley.Commands
                   (VerificationKeyFile(..), SigningKeyFile(..))

data ShelleyKeyGenError = ShelleyColdKeyGenError !(FileError ())
                        deriving Show

renderShelleyKeyGenError :: ShelleyKeyGenError -> Text
renderShelleyKeyGenError err =
  case err of
    ShelleyColdKeyGenError keyErr ->
      "Error generating shelley cold keys: " <> Text.pack (displayError keyErr)

runColdKeyGen :: Key keyrole => AsType keyrole
              -> VerificationKeyFile -> SigningKeyFile
              -> ExceptT ShelleyKeyGenError IO ()
runColdKeyGen role (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ Api.generateSigningKey role
    let vkey = getVerificationKey skey
    firstExceptT ShelleyColdKeyGenError $ newExceptT $
      Api.writeFileTextEnvelope vkeyPath Nothing vkey
    firstExceptT ShelleyColdKeyGenError $ newExceptT $
      Api.writeFileTextEnvelope skeyPath Nothing skey
