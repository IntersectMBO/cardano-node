module Cardano.CLI.Shelley.Run.Key
  ( ShelleyKeyCmdError
  , renderShelleyKeyCmdError
  , runKeyCmd
  ) where

import           Cardano.Prelude

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT,
                   handleIOExceptT, hoistEither, hoistMaybe, newExceptT)

import           Cardano.Api.TextView (TextViewDescription (..))
import           Cardano.Api.Typed

import           Cardano.CLI.Byron.Key (ByronKeyFailure, CardanoEra (..),
                   readEraSigningKey, renderByronKeyFailure)
import           Cardano.CLI.Helpers
import           Cardano.CLI.Shelley.Parsers (ITNKeyFile (..), KeyCmd (..),
                   OutputFile (..), SigningKeyFile (..),
                   VerificationKeyFile (..))

import qualified Cardano.Crypto.DSIGN.Class as Crypto
import qualified Cardano.Crypto.Signing as Byron.Crypto
import qualified Cardano.Crypto.Wallet as Crypto.HD

import qualified Shelley.Spec.Ledger.Keys as Shelley

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

runSingleITNKeyConversion
  :: ITNKeyFile
  -> Maybe OutputFile
  -> ExceptT ShelleyKeyCmdError IO ()
runSingleITNKeyConversion (ITNVerificationKeyFile (VerificationKeyFile vk)) mOutFile = do
  bech32publicKey <- firstExceptT ShelleyKeyCmdItnKeyConvError . newExceptT $ readBech32 vk
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
  bech32privateKey <- firstExceptT ShelleyKeyCmdItnKeyConvError . newExceptT $ readBech32 sk
  skey <- hoistEither
    . first ShelleyKeyCmdItnKeyConvError
    $ convertITNSigningKey bech32privateKey
  case mOutFile of
    Just (OutputFile fp) ->
      firstExceptT ShelleyKeyCmdWriteFileError
        . newExceptT
        $ writeFileTextEnvelope fp Nothing skey
    Nothing -> print skey
